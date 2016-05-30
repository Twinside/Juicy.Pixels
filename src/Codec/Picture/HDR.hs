{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections   #-}
-- | Module dedicated of Radiance file decompression (.hdr or .pic) file.
-- Radiance file format is used for High dynamic range imaging.
module Codec.Picture.HDR( decodeHDR
                        , decodeHDRWithMetadata
                        , encodeHDR
                        , encodeRawHDR
                        , encodeRLENewStyleHDR
                        , writeHDR
                        , writeRLENewStyleHDR
                        ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( pure, (<*>), (<$>) )
#endif

import Data.Bits( Bits, (.&.), (.|.), unsafeShiftL, unsafeShiftR )
import Data.Char( ord, chr, isDigit )
import Data.Word( Word8 )
import Data.Monoid( (<>) )
import Control.Monad( when, foldM, foldM_, forM, forM_, unless )
import Control.Monad.Trans.Class( lift )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as BC

import Data.List( partition )
import Data.Binary( Binary( .. ), encode )
import Data.Binary.Get( Get, getByteString, getWord8 )
import Data.Binary.Put( putByteString, putLazyByteString )

import Control.Monad.ST( ST, runST )
import Foreign.Storable ( Storable )
import Control.Monad.Primitive ( PrimState, PrimMonad )
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

import Codec.Picture.Metadata( Metadatas
                             , SourceFormat( SourceHDR )
                             , basicMetadata )
import Codec.Picture.InternalHelper
import Codec.Picture.Types
import Codec.Picture.VectorByteConversion

#if MIN_VERSION_transformers(0, 4, 0)
import Control.Monad.Trans.Except( ExceptT, throwE, runExceptT )
#else
-- Transfomers 0.3 compat
import Control.Monad.Trans.Error( Error, ErrorT, throwError, runErrorT )

type ExceptT = ErrorT

throwE :: (Monad m, Error e) => e -> ErrorT e m a
throwE = throwError

runExceptT :: ErrorT e m a -> m (Either e a)
runExceptT = runErrorT
#endif

{-# INLINE (.<<.) #-}
(.<<.), (.>>.) :: (Bits a) => a -> Int -> a
(.<<.) = unsafeShiftL
(.>>.) = unsafeShiftR

{-# INLINE (.<-.) #-}
(.<-.) :: (PrimMonad m, Storable a)
       => M.STVector (PrimState m) a -> Int -> a -> m ()
(.<-.) = M.write 
         {-M.unsafeWrite-}

type HDRReader s a = ExceptT String (ST s) a

data RGBE = RGBE !Word8 !Word8 !Word8 !Word8

instance Binary RGBE where
    put (RGBE r g b e) = put r >> put g >> put b >> put e
    get = RGBE <$> get <*> get <*> get <*> get

checkLineLength :: RGBE -> Int
checkLineLength (RGBE _ _ a b) =
    (fromIntegral a .<<. 8) .|. fromIntegral b

isNewRunLengthMarker :: RGBE -> Bool
isNewRunLengthMarker (RGBE 2 2 _ _) = True
isNewRunLengthMarker _ = False

data RadianceFormat =
      FormatRGBE
    | FormatXYZE

radiance32bitRleRGBEFormat, radiance32bitRleXYZEFromat :: B.ByteString
radiance32bitRleRGBEFormat = BC.pack "32-bit_rle_rgbe"
radiance32bitRleXYZEFromat = BC.pack "32-bit_rle_xyze"

instance Binary RadianceFormat where
  put FormatRGBE = putByteString radiance32bitRleRGBEFormat
  put FormatXYZE = putByteString radiance32bitRleXYZEFromat

  get = getByteString (B.length radiance32bitRleRGBEFormat) >>= format
    where format sig
            | sig == radiance32bitRleRGBEFormat = pure FormatRGBE
            | sig == radiance32bitRleXYZEFromat = pure FormatXYZE
            | otherwise = fail "Unrecognized Radiance format"

toRGBE :: PixelRGBF -> RGBE
toRGBE (PixelRGBF r g b)
    | d <= 1e-32 = RGBE 0 0 0 0
    | otherwise = RGBE (fix r) (fix g) (fix b) (fromIntegral $ e + 128)
  where d = maximum [r, g, b]
        e = exponent d
        coeff = significand d *  255.9999 / d
        fix v = truncate $ v * coeff


dropUntil :: Word8 -> Get ()
dropUntil c = getWord8 >>= inner
  where inner val | val == c = pure ()
        inner _ = getWord8 >>= inner

getUntil :: (Word8 -> Bool) -> B.ByteString -> Get B.ByteString
getUntil f initialAcc = getWord8 >>= inner initialAcc
  where inner acc c | f c = pure acc
        inner acc c = getWord8 >>= inner (B.snoc acc c)

data RadianceHeader = RadianceHeader
  { radianceInfos :: [(B.ByteString, B.ByteString)]
  , radianceFormat :: RadianceFormat
  , radianceHeight :: !Int
  , radianceWidth  :: !Int
  , radianceData   :: L.ByteString
  }

radianceFileSignature :: B.ByteString
radianceFileSignature = BC.pack "#?RADIANCE\n"

unpackColor :: L.ByteString -> Int -> RGBE
unpackColor str idx = RGBE (at 0) (at 1) (at 2) (at 3)
  where at n = L.index str . fromIntegral $ idx + n

storeColor :: M.STVector s Word8 -> Int -> RGBE -> ST s ()
storeColor vec idx (RGBE r g b e) = do
    (vec .<-. (idx + 0)) r
    (vec .<-. (idx + 1)) g
    (vec .<-. (idx + 2)) b
    (vec .<-. (idx + 3)) e

parsePair :: Char -> Get (B.ByteString, B.ByteString)
parsePair firstChar = do
    let eol c = c == fromIntegral (ord '\n')
    line <- getUntil eol B.empty
    case BC.split '=' line of
      [] -> pure (BC.singleton firstChar, B.empty)
      [val] -> pure (BC.singleton firstChar, val)
      [key, val] -> pure (BC.singleton firstChar <> key, val)
      (key : vals) -> pure (BC.singleton firstChar <> key, B.concat vals)

decodeInfos :: Get [(B.ByteString, B.ByteString)]
decodeInfos = do
    char <- getChar8
    case char of
      -- comment
      '#' -> dropUntil (fromIntegral $ ord '\n') >> decodeInfos
      -- end of header, no more information
      '\n' -> pure []
      -- Classical parsing
      c -> (:) <$> parsePair c <*> decodeInfos


-- | Decode an HDR (radiance) image, the resulting image can be:
--
--  * 'ImageRGBF'
--
decodeHDR :: B.ByteString -> Either String DynamicImage
decodeHDR = fmap fst . decodeHDRWithMetadata

-- | Equivalent to decodeHDR but with aditional metadatas.
decodeHDRWithMetadata :: B.ByteString -> Either String (DynamicImage, Metadatas)
decodeHDRWithMetadata str = runST $ runExceptT $
  case runGet decodeHeader $ L.fromChunks [str] of
    Left err -> throwE err
    Right rez ->
      let meta = basicMetadata SourceHDR (abs $ radianceWidth rez) (abs $ radianceHeight rez) in
      (, meta) . ImageRGBF <$> (decodeRadiancePicture rez >>= lift . unsafeFreezeImage)

getChar8 :: Get Char
getChar8 = chr . fromIntegral <$> getWord8

isSign :: Char -> Bool
isSign c = c == '+' || c == '-'

isAxisLetter :: Char -> Bool
isAxisLetter c = c == 'X' || c == 'Y'

decodeNum :: Get Int
decodeNum = do
    sign <- getChar8
    letter <- getChar8
    space <- getChar8

    unless (isSign sign && isAxisLetter letter && space == ' ')
           (fail "Invalid radiance size declaration")

    let numDec acc c | isDigit c =
            getChar8 >>= numDec (acc * 10 + ord c - ord '0')
        numDec acc _
            | sign == '-' = pure $ negate acc
            | otherwise = pure acc

    getChar8 >>= numDec 0

copyPrevColor :: M.STVector s Word8 -> Int -> ST s ()
copyPrevColor scanLine idx = do
    r <- scanLine `M.unsafeRead` (idx - 4)
    g <- scanLine `M.unsafeRead` (idx - 3)
    b <- scanLine `M.unsafeRead` (idx - 2)
    e <- scanLine `M.unsafeRead` (idx - 1)

    (scanLine `M.unsafeWrite` (idx + 0)) r
    (scanLine `M.unsafeWrite` (idx + 1)) g
    (scanLine `M.unsafeWrite` (idx + 2)) b
    (scanLine `M.unsafeWrite` (idx + 3)) e

oldStyleRLE :: L.ByteString -> Int -> M.STVector s Word8
            -> HDRReader s Int
oldStyleRLE inputData initialIdx scanLine = inner initialIdx 0 0
  where maxOutput = M.length scanLine
        maxInput = fromIntegral $ L.length inputData

        inner readIdx writeIdx _
            | readIdx >= maxInput || writeIdx >= maxOutput = pure readIdx
        inner readIdx writeIdx shift = do
          let color@(RGBE r g b e) = unpackColor inputData readIdx
              isRun = r == 1 && g == 1 && b == 1

          if not isRun
            then do
              lift $ storeColor scanLine writeIdx color
              inner (readIdx + 4) (writeIdx + 4) 0
         
            else do
              let count = fromIntegral e .<<. shift
              lift $ forM_ [0 .. count] $ \i -> copyPrevColor scanLine (writeIdx + 4 * i)
              inner (readIdx + 4) (writeIdx + 4 * count) (shift + 8)

newStyleRLE :: L.ByteString -> Int -> M.STVector s Word8
            -> HDRReader s Int
newStyleRLE inputData initialIdx scanline = foldM inner initialIdx [0 .. 3]
  where dataAt idx
            | fromIntegral idx >= maxInput = throwE $ "Read index out of bound (" ++ show idx ++ ")"
            | otherwise = pure $ L.index inputData (fromIntegral idx)

        maxOutput = M.length scanline
        maxInput = fromIntegral $ L.length inputData
        stride = 4


        strideSet count destIndex _ | endIndex > maxOutput + stride =
          throwE $ "Out of bound HDR scanline " ++ show endIndex ++ " (max " ++ show maxOutput ++ ")"
            where endIndex = destIndex + count * stride
        strideSet count destIndex val = aux destIndex count
            where aux i 0 =  pure i
                  aux i c = do
                    lift $ (scanline .<-. i) val
                    aux (i + stride) (c - 1)


        strideCopy _ count destIndex
            | writeEndBound > maxOutput + stride = throwE "Out of bound HDR scanline"
                where writeEndBound = destIndex + count * stride
        strideCopy sourceIndex count destIndex = aux sourceIndex destIndex count
          where aux _ j 0 = pure j
                aux i j c = do
                    val <- dataAt i
                    lift $ (scanline .<-. j) val
                    aux (i + 1) (j + stride) (c - 1)

        inner readIdx writeIdx
            | readIdx >= maxInput || writeIdx >= maxOutput = pure readIdx
        inner readIdx writeIdx = do
          code <- dataAt readIdx
          if code > 128
            then do
              let repeatCount = fromIntegral code .&. 0x7F
              newVal <- dataAt $ readIdx + 1
              endIndex <- strideSet repeatCount writeIdx newVal
              inner (readIdx + 2) endIndex 

            else do
              let iCode = fromIntegral code
              endIndex <- strideCopy (readIdx + 1) iCode writeIdx
              inner (readIdx + iCode + 1) endIndex

instance Binary RadianceHeader where
    get = decodeHeader
    put hdr = do
        putByteString radianceFileSignature
        putByteString $ BC.pack "FORMAT="
        put $ radianceFormat hdr
        let sizeString =
              BC.pack $ "\n\n-Y " ++ show (radianceHeight hdr)
                        ++ " +X " ++ show (radianceWidth hdr) ++ "\n"
        putByteString sizeString
        putLazyByteString $ radianceData hdr


decodeHeader :: Get RadianceHeader
decodeHeader = do
    sig <- getByteString $ B.length radianceFileSignature
    when (sig /= radianceFileSignature)
         (fail "Invalid radiance file signature")

    infos <- decodeInfos
    let formatKey = BC.pack "FORMAT"
    case partition (\(k,_) -> k /= formatKey) infos of
      (_, []) -> fail "No radiance format specified"
      (info, [(_, formatString)]) ->
        case runGet get $ L.fromChunks [formatString] of
          Left err -> fail err
          Right format -> do
              (n1, n2, b) <- (,,) <$> decodeNum
                                  <*> decodeNum
                                  <*> getRemainingBytes
              return . RadianceHeader info format n1 n2 $ L.fromChunks [b]

      _ -> fail "Multiple radiance format specified"

toFloat :: RGBE -> PixelRGBF
toFloat (RGBE r g b e) = PixelRGBF rf gf bf
  where f = encodeFloat 1 $ fromIntegral e - (128 + 8)
        rf = (fromIntegral r + 0.0) * f
        gf = (fromIntegral g + 0.0) * f
        bf = (fromIntegral b + 0.0) * f

encodeScanlineColor :: M.STVector s Word8
                    -> M.STVector s Word8
                    -> Int
                    -> ST s Int
encodeScanlineColor vec outVec outIdx = do
    val <- vec `M.unsafeRead` 0
    runLength 1 0 val 1 outIdx
  where maxIndex = M.length vec

        pushRun len val at = do
            (outVec `M.unsafeWrite` at) $ fromIntegral $ len .|. 0x80
            (outVec `M.unsafeWrite` (at + 1)) val
            return $ at + 2

        pushData start len at = do
            (outVec `M.unsafeWrite` at) $ fromIntegral len
            let first = start - len
                end = start - 1
                offset = at - first + 1
            forM_ [first .. end] $ \i -> do
                v <- vec `M.unsafeRead` i
                (outVec `M.unsafeWrite` (offset + i)) v

            return $ at + len + 1

        -- End of scanline, empty the thing
        runLength run cpy prev idx at | idx >= maxIndex =
            case (run, cpy) of
                (0, 0) -> pure at
                (0, n) -> pushData idx n at
                (n, 0) -> pushRun n prev at
                (_, _) -> error "HDR - Run length algorithm is wrong"

        -- full runlength, we must write the packet
        runLength r@127   _ prev idx at = do
            val <- vec `M.unsafeRead` idx
            pushRun r prev at >>=
                runLength 1 0 val (idx + 1)

        -- full copy, we must write the packet
        runLength   _ c@127    _ idx at = do
            val <- vec `M.unsafeRead` idx
            pushData idx c at >>=
                runLength 1 0 val (idx + 1)

        runLength n 0 prev idx at = do
            val <- vec `M.unsafeRead` idx
            case val == prev of
               True -> runLength (n + 1) 0 prev (idx + 1) at
               False | n < 4 -> runLength 0 (n + 1) val (idx + 1) at
               False ->
                    pushRun n prev at >>=
                        runLength 1 0 val (idx + 1)

        runLength 0 n prev idx at = do
            val <- vec `M.unsafeRead` idx
            if val /= prev
               then runLength 0 (n + 1) val (idx + 1) at
               else
                pushData (idx - 1) (n - 1) at >>=
                    runLength (2 :: Int) 0 val (idx + 1)

        runLength _ _ _ _ _ =
            error "HDR RLE inconsistent state"

-- | Write an High dynamic range image into a radiance
-- image file on disk.
writeHDR :: FilePath -> Image PixelRGBF -> IO ()
writeHDR filename img = L.writeFile filename $ encodeHDR img

-- | Write a RLE encoded High dynamic range image into a radiance
-- image file on disk.
writeRLENewStyleHDR :: FilePath -> Image PixelRGBF -> IO ()
writeRLENewStyleHDR filename img =
    L.writeFile filename $ encodeRLENewStyleHDR img

-- | Encode an High dynamic range image into a radiance image
-- file format.
-- Alias for encodeRawHDR
encodeHDR :: Image PixelRGBF -> L.ByteString
encodeHDR = encodeRawHDR

-- | Encode an High dynamic range image into a radiance image
-- file format. without compression
encodeRawHDR :: Image PixelRGBF -> L.ByteString
encodeRawHDR pic = encode descriptor
  where
    newImage = pixelMap rgbeInRgba pic
    -- we are cheating to death here, the layout we want
    -- correspond to the layout of pixelRGBA8, so we
    -- convert
    rgbeInRgba pixel = PixelRGBA8 r g b e
      where RGBE r g b e = toRGBE pixel

    descriptor = RadianceHeader
        { radianceInfos = []
        , radianceFormat = FormatRGBE
        , radianceHeight = imageHeight pic
        , radianceWidth  = imageWidth pic
        , radianceData = L.fromChunks [toByteString $ imageData newImage]
        }


-- | Encode an High dynamic range image into a radiance image
-- file format using a light RLE compression. Some problems
-- seem to arise with some image viewer.
encodeRLENewStyleHDR :: Image PixelRGBF -> L.ByteString
encodeRLENewStyleHDR pic = encode $ runST $ do
    let w = imageWidth pic
        h = imageHeight pic

    scanLineR <- M.new w :: ST s (M.STVector s Word8)
    scanLineG <- M.new w
    scanLineB <- M.new w
    scanLineE <- M.new w

    encoded <-
        forM [0 .. h - 1] $ \line -> do
            buff <- M.new $ w * 4 + w `div` 127 + 2
            let columner col | col >= w = return ()
                columner col = do
                      let RGBE r g b e = toRGBE $ pixelAt pic col line
                      (scanLineR `M.unsafeWrite` col) r
                      (scanLineG `M.unsafeWrite` col) g
                      (scanLineB `M.unsafeWrite` col) b
                      (scanLineE `M.unsafeWrite` col) e

                      columner (col + 1)

            columner 0

            (buff `M.unsafeWrite` 0) 2
            (buff `M.unsafeWrite` 1) 2
            (buff `M.unsafeWrite` 2) $ fromIntegral ((w .>>. 8) .&. 0xFF)
            (buff `M.unsafeWrite` 3) $ fromIntegral (w .&. 0xFF)

            i1 <- encodeScanlineColor scanLineR buff 4        
            i2 <- encodeScanlineColor scanLineG buff i1
            i3 <- encodeScanlineColor scanLineB buff i2
            endIndex <- encodeScanlineColor scanLineE buff i3

            (\v -> blitVector v 0 endIndex) <$> V.unsafeFreeze buff

    pure RadianceHeader
        { radianceInfos = []
        , radianceFormat = FormatRGBE
        , radianceHeight = h
        , radianceWidth  = w
        , radianceData = L.fromChunks encoded 
        }
    

decodeRadiancePicture :: RadianceHeader -> HDRReader s (MutableImage s PixelRGBF)
decodeRadiancePicture hdr = do
    let width = abs $ radianceWidth hdr
        height = abs $ radianceHeight hdr
        packedData = radianceData hdr

    scanLine <- lift $ M.new $ width * 4
    resultBuffer <- lift $ M.new $ width * height * 3

    let scanLineImage = MutableImage
                      { mutableImageWidth = width
                      , mutableImageHeight = 1
                      , mutableImageData = scanLine
                      }

        finalImage = MutableImage
                   { mutableImageWidth = width
                   , mutableImageHeight = height
                   , mutableImageData = resultBuffer
                   }

    let scanLineExtractor readIdx line = do
          let color = unpackColor packedData readIdx
              inner | isNewRunLengthMarker color = do
                          let calcSize = checkLineLength color
                          when (calcSize /= width)
                               (throwE "Invalid sanline size")
                          pure $ \idx -> newStyleRLE packedData (idx + 4)
                    | otherwise = pure $ oldStyleRLE packedData
          f <- inner
          newRead <- f readIdx scanLine
          forM_ [0 .. width - 1] $ \i -> do
              -- mokay, it's a hack, but I don't want to define a
              -- pixel instance of RGBE...
              PixelRGBA8 r g b e <- lift $ readPixel scanLineImage i 0
              lift $ writePixel finalImage i line . toFloat $ RGBE r g b e

          return newRead

    foldM_ scanLineExtractor 0 [0 .. height - 1]

    return finalImage

