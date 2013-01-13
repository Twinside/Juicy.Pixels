module Codec.Picture.HDR( decodeHDR ) where

import Data.Bits( Bits, (.&.), (.|.), shiftL, shiftR, testBit )
import Data.Char( ord, chr, isDigit )
import Data.Word( Word8 )
import Data.Int( Int32 )
import Data.Monoid( (<>) )
import Control.Applicative( pure, (<$>), (<*>) )
import Control.Monad( when, foldM, foldM_, forM_ )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as BC

import Data.List( partition )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( Get, getByteString, getWord8 )
import Data.Binary.Put( putByteString )

import Control.Monad.ST( ST, runST )
import Foreign.Storable ( Storable )
import Control.Monad.Primitive ( PrimState, PrimMonad )
import qualified Data.Vector.Storable.Mutable as M

import Codec.Picture.InternalHelper
import Codec.Picture.Types

import Debug.Trace
import Text.Printf( printf )

{-# INLINE (.<<.) #-}
{-# INLINE (.>>.) #-}
(.<<.), (.>>.) :: (Bits a) => a -> Int -> a
(.<<.) = shiftL
(.>>.) = shiftR

{-# INLINE (.!!!.) #-}
(.!!!.) :: (PrimMonad m, Storable a)
        => M.STVector (PrimState m) a -> Int -> m a
(.!!!.) = M.read
          {-M.unsafeRead-}

{-# INLINE (.<-.) #-}
(.<-.) :: (PrimMonad m, Storable a)
       => M.STVector (PrimState m) a -> Int -> a -> m ()
(.<-.) = M.write 
         {-M.unsafeWrite-}

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

dropUntil :: Word8 -> Get ()
dropUntil c = getWord8 >>= inner
  where inner val | val == c = pure ()
        inner _ = getWord8 >>= inner

getUntil :: (Word8 -> Bool) -> B.ByteString -> Get B.ByteString
getUntil f initialAcc = getWord8 >>= inner initialAcc
  where inner acc c | f c = pure acc
        inner acc c = getWord8 >>= inner (B.snoc acc c)

data RadianceHeader = RadianceHeader
  { _radianceInfos :: [(B.ByteString, B.ByteString)]
  , _radianceFormat :: RadianceFormat 
  , radianceHeight :: !Int
  , radianceWidth  :: !Int
  , radianceData   :: B.ByteString
  }

radianceFileSignature :: B.ByteString
radianceFileSignature = BC.pack "#?RADIANCE\n"

unpackColor :: B.ByteString -> Int -> RGBE
unpackColor str idx = RGBE (at 0) (at 1) (at 2) (at 3)
  where at n = B.index str $ idx + n

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


decodeHDR :: B.ByteString -> Either String DynamicImage
decodeHDR str = runST $ do
    case runGet decodeHeader $ L.fromChunks [str] of
      Left err -> pure $ Left err
      Right rez ->
        Right . ImageRGBF <$> (decodeRadiancePicture rez >>= unsafeFreezeImage)

{-encodeHDR :: a -> L.ByteString-}
{-encodeHDR _ = L.empty-}

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

    when (not $ isSign sign && isAxisLetter letter && space == ' ')
         (fail "Invalid radiance size declaration")

    let numDec acc c | isDigit c =
            getChar8 >>= numDec (acc * 10 + ord c - (ord '0'))
        numDec acc _
            | sign == '-' = pure $ negate acc
            | otherwise = pure acc

    getChar8 >>= numDec 0

strideCopy :: B.ByteString -> M.STVector s Word8
           -> Int -> Int -> Int -> Int -> ST s Int
strideCopy src dest sourceIndex size destIndex stride = aux sourceIndex destIndex
  where endIndex = sourceIndex + size
        aux i j | i >= endIndex  = return j
        aux i j = do
          (dest .<-. j) $ B.index src i
          aux (i + 1) (j + stride)

strideSet :: M.STVector s Word8 -> Int -> Int -> Int -> Word8 -> ST s Int
strideSet dest count destIndex stride val = aux destIndex
  where endIndex = -- min (M.length dest) $
                   destIndex + count * stride
        aux i | i >= endIndex  = return endIndex
        aux i = (dest .<-. i) val >> aux (i + stride)

copyPrevColor :: M.STVector s Word8 -> Int -> ST s ()
copyPrevColor scanLine idx = do
    r <- scanLine .!!!. (idx - 4)
    g <- scanLine .!!!. (idx - 3)
    b <- scanLine .!!!. (idx - 2)
    e <- scanLine .!!!. (idx - 1)

    (scanLine .<-. (idx + 0)) r
    (scanLine .<-. (idx + 1)) g
    (scanLine .<-. (idx + 2)) b
    (scanLine .<-. (idx + 3)) e

oldStyleRLE :: B.ByteString -> Int -> M.STVector s Word8
            -> ST s Int
oldStyleRLE inputData initialIdx scanLine = trace "oldStyleRLE" $ inner initialIdx 0 0
  where maxOutput = M.length scanLine
        maxInput = B.length inputData

        inner readIdx writeIdx _
            | readIdx >= maxInput || writeIdx >= maxOutput = return readIdx
        inner readIdx writeIdx shift = trace (printf "r:%d w:%d (mi:%d mo:%d)" readIdx writeIdx maxInput maxOutput) $ do
          let color@(RGBE r g b e) = unpackColor inputData readIdx
              isRun = r == 1 && g == 1 && b == 1

          if not isRun
            then do
              storeColor scanLine writeIdx color
              inner (readIdx + 4) (writeIdx + 4) 0
         
            else do
              let count = fromIntegral e .<<. shift
              forM_ [0 .. count] $ \i -> copyPrevColor scanLine (writeIdx + 4 * i)
              inner (readIdx + 4) (writeIdx + 4 * count) (shift + 8)

newStyleRLE :: B.ByteString -> Int -> M.STVector s Word8
            -> ST s Int
newStyleRLE inputData initialIdx scanline = foldm [0 .. 3] initialIdx inner
  where dataAt = B.index inputData
        maxOutput = M.length scanline
        maxInput = B.length inputData
        foldm lst acc f = foldM f acc lst

        inner readIdx writeIdx
            | readIdx >= maxInput || writeIdx >= maxOutput = return readIdx
        inner readIdx writeIdx = 
          let code = dataAt readIdx
          in if code > 128
            then do
              let repeatCount = fromIntegral code .&. 0x7F
                  newVal = dataAt $ readIdx + 1
              endIndex <- strideSet scanline repeatCount writeIdx 4 newVal
              inner (readIdx + 2) endIndex 

            else do
              let iCode = fromIntegral code
              endIndex <- strideCopy inputData scanline readIdx iCode writeIdx 4
              inner (readIdx + iCode + 1) endIndex

decodeHeader :: Get RadianceHeader
decodeHeader = do
    sig <- getByteString $ B.length radianceFileSignature
    when (sig /= radianceFileSignature)
         (fail "Invalid radiance file signature")

    infos <- decodeInfos
    let formatKey = trace (show infos) $ BC.pack "FORMAT"
    case partition (\(k,_) -> k /= formatKey) infos of
      (_, []) -> fail "No radiance format specified"
      (info, [(_, formatString)]) ->
        case runGet get $ L.fromChunks [formatString] of
          Left err -> fail err
          Right format ->
              RadianceHeader info format <$> decodeNum
                                         <*> decodeNum
                                         <*> getRemainingBytes

      _ -> fail "Multiple radiance format specified"

toFloat :: RGBE -> PixelRGBF
toFloat (RGBE r g b e) = PixelRGBF rf gf bf
  where f = encodeFloat 1 $ fromIntegral e - (128 + 8)
        rf = (fromIntegral r + 0.5) * f
        gf = (fromIntegral g + 0.5) * f
        bf = (fromIntegral b + 0.5) * f

decodeRadiancePicture :: RadianceHeader -> ST s (MutableImage s (PixelRGBF))
decodeRadiancePicture hdr = do
    let width = abs $ radianceWidth hdr
        height = abs $ radianceHeight hdr
        packedData = radianceData hdr

    scanLine <- M.new $ width * 4
    resultBuffer <- M.new $ width * height * 3

    let scanLineImage = MutableImage
                      { mutableImageWidth = width
                      , mutableImageHeight = 0
                      , mutableImageData = scanLine
                      }

        finalImage = MutableImage
                   { mutableImageWidth = width
                   , mutableImageHeight = height
                   , mutableImageData = resultBuffer
                   }

    let scanLineExtractor readIdx line = do
            newRead <- inner readIdx scanLine
            forM_ [0 .. width - 1] $ \i -> do
                -- mokay, it's a hack, but I don't want to define a
                -- pixel instance of RGBE...
                PixelRGBA8 r g b e <- readPixel scanLineImage i 0
                writePixel finalImage i line . toFloat $ RGBE r g b e

            return newRead

           where color = unpackColor packedData readIdx
                 inner | isNewRunLengthMarker color = 
                            trace (printf "check : %d" $ checkLineLength color) $ \idx -> newStyleRLE packedData (idx + 4)
                       | otherwise = oldStyleRLE packedData

    foldM_ scanLineExtractor 0 [0 .. height - 1]

    return finalImage

