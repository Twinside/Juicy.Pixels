{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | Modules used for Bitmap file (.bmp) file loading and writing
module Codec.Picture.Bitmap( -- * Functions
                             writeBitmap
                           , encodeBitmap
                           , encodeBitmapWithMetadata
                           , decodeBitmap
                           , decodeBitmapWithMetadata
                           , decodeBitmapWithPaletteAndMetadata
                           , encodeDynamicBitmap 
                           , encodeBitmapWithPaletteAndMetadata
                           , writeDynamicBitmap 
                             -- * Accepted format in output
                           , BmpEncodable( )
                           ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mempty )
import Control.Applicative( (<$>) )
#endif

import Control.Arrow( first )
import Control.Monad( replicateM, when, foldM_, forM_ )
import Control.Monad.ST ( ST, runST )
import Data.Maybe( fromMaybe )
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as M
import Data.Binary( Binary( .. ) )
import Data.Binary.Put( Put
                      , runPut
                      , putWord16le
                      , putWord32le
                      , putByteString 
                      )

import Data.Binary.Get( Get
                      , getWord8
                      , getWord16le 
                      , getWord32le
                      , getWord32be
                      , bytesRead
                      , skip
                      )

import Data.Int( Int32 )
import Data.Word( Word32, Word16, Word8 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Codec.Picture.InternalHelper
import Codec.Picture.Types
import Codec.Picture.VectorByteConversion
import qualified Codec.Picture.Metadata as Met
import Codec.Picture.Metadata ( Metadatas )

data BmpHeader = BmpHeader
    { magicIdentifier :: !Word16
    , fileSize        :: !Word32 -- ^ in bytes
    , reserved1       :: !Word16
    , reserved2       :: !Word16
    , dataOffset      :: !Word32
    }

bitmapMagicIdentifier :: Word16
bitmapMagicIdentifier = 0x4D42

instance Binary BmpHeader where
    put hdr = do
        putWord16le $ magicIdentifier hdr
        putWord32le $ fileSize hdr
        putWord16le $ reserved1 hdr
        putWord16le $ reserved2 hdr
        putWord32le $ dataOffset hdr

    get = do
        ident <- getWord16le
        when (ident /= bitmapMagicIdentifier)
             (fail "Invalid Bitmap magic identifier")
        fsize <- getWord32le
        r1 <- getWord16le
        r2 <- getWord16le
        offset <- getWord32le
        return BmpHeader
            { magicIdentifier = ident
            , fileSize = fsize
            , reserved1 = r1
            , reserved2 = r2
            , dataOffset = offset
            }


data BmpInfoHeader = BmpInfoHeader
    { size              :: !Word32 -- Header size in bytes
    , width             :: !Int32
    , height            :: !Int32
    , planes            :: !Word16 -- Number of colour planes
    , bitPerPixel       :: !Word16
    , bitmapCompression :: !Word32
    , byteImageSize     :: !Word32
    , xResolution       :: !Int32 -- ^ Pixels per meter
    , yResolution       :: !Int32 -- ^ Pixels per meter
    , colorCount        :: !Word32
    , importantColours  :: !Word32
    }
    deriving Show

sizeofBmpHeader, sizeofBmpInfo  :: Word32
sizeofBmpHeader = 2 + 4 + 2 + 2 + 4
sizeofBmpInfo = 3 * 4 + 2 * 2 + 6 * 4

instance Binary BmpInfoHeader where
    put hdr = do
        putWord32le $ size hdr
        putWord32le . fromIntegral $ width hdr
        putWord32le . fromIntegral $ height hdr
        putWord16le $ planes hdr
        putWord16le $ bitPerPixel hdr
        putWord32le $ bitmapCompression hdr
        putWord32le $ byteImageSize hdr
        putWord32le . fromIntegral $ xResolution hdr
        putWord32le . fromIntegral $ yResolution hdr
        putWord32le $ colorCount hdr
        putWord32le $ importantColours hdr

    get = do
        readSize <- getWord32le
        readWidth <- fromIntegral <$> getWord32le
        readHeight <- fromIntegral <$> getWord32le
        readPlanes <- getWord16le
        readBitPerPixel <- getWord16le
        readBitmapCompression <- getWord32le
        readByteImageSize <- getWord32le
        readXResolution <- fromIntegral <$> getWord32le
        readYResolution <- fromIntegral <$> getWord32le
        readColorCount <- getWord32le
        readImportantColours <- getWord32le
        return BmpInfoHeader {
            size = readSize,
            width = readWidth,
            height = readHeight,
            planes = readPlanes,
            bitPerPixel = readBitPerPixel,
            bitmapCompression = readBitmapCompression,
            byteImageSize = readByteImageSize,
            xResolution = readXResolution,
            yResolution = readYResolution,
            colorCount = readColorCount,
            importantColours = readImportantColours
        }

newtype BmpPalette = BmpPalette [(Word8, Word8, Word8, Word8)]

putPalette :: BmpPalette -> Put
putPalette (BmpPalette p) = mapM_ (\(r, g, b, a) -> put r >> put g >> put b >> put a) p

-- | All the instance of this class can be written as a bitmap file
-- using this library.
class BmpEncodable pixel where
    bitsPerPixel   :: pixel -> Int
    bmpEncode      :: Image pixel -> Put
    defaultPalette :: pixel -> BmpPalette
    defaultPalette _ = BmpPalette []

stridePut :: M.STVector s Word8 -> Int -> Int -> ST s ()
{-# INLINE stridePut #-}
stridePut vec = inner
 where inner  _ 0 = return ()
       inner ix n = do
           (vec `M.unsafeWrite` ix) 0
           inner (ix + 1) (n - 1)

instance BmpEncodable Pixel8 where
    defaultPalette _ = BmpPalette [(x,x,x, 255) | x <- [0 .. 255]]
    bitsPerPixel _ = 8
    bmpEncode (Image {imageWidth = w, imageHeight = h, imageData = arr}) =
      forM_ [h - 1, h - 2 .. 0] $ \l -> putVector $ runST $ encodeLine l
        where stride = fromIntegral $ linePadding 8 w
              putVector vec = putByteString $ blitVector vec 0 lineWidth
              lineWidth = w + stride

              encodeLine :: forall s. Int -> ST s (VS.Vector Word8)
              encodeLine line = do
                  buff <- M.new lineWidth

                  let lineIdx = line * w
                      inner col | col >= w = return ()
                      inner col = do
                          let v = arr `VS.unsafeIndex` (lineIdx + col)
                          (buff `M.unsafeWrite` col) v
                          inner (col + 1)

                  inner 0

                  stridePut buff w stride
                  VS.unsafeFreeze buff

instance BmpEncodable PixelRGBA8 where
    bitsPerPixel _ = 32
    bmpEncode (Image {imageWidth = w, imageHeight = h, imageData = arr}) = 
      forM_ [h - 1, h - 2 .. 0] $ \l -> putVector $ runST $ putLine l
      where
        putVector vec = putByteString . blitVector vec 0 $ w * 4

        putLine :: forall s. Int -> ST s (VS.Vector Word8)
        putLine line = do
            buff <- M.new $ 4 * w
            let initialIndex = line * w * 4
                inner col _ _ | col >= w = return ()
                inner col writeIdx readIdx = do
                    let r = arr `VS.unsafeIndex` readIdx
                        g = arr `VS.unsafeIndex` (readIdx + 1)
                        b = arr `VS.unsafeIndex` (readIdx + 2)
                        a = arr `VS.unsafeIndex` (readIdx + 3)

                    (buff `M.unsafeWrite` writeIdx) b
                    (buff `M.unsafeWrite` (writeIdx + 1)) g
                    (buff `M.unsafeWrite` (writeIdx + 2)) r
                    (buff `M.unsafeWrite` (writeIdx + 3)) a

                    inner (col + 1) (writeIdx + 4) (readIdx + 4)

            inner 0 0 initialIndex
            VS.unsafeFreeze buff

instance BmpEncodable PixelRGB8 where
    bitsPerPixel _ = 24
    bmpEncode (Image {imageWidth = w, imageHeight = h, imageData = arr}) =
       forM_ [h - 1, h - 2 .. 0] $ \l -> putVector $ runST $ putLine l
        where
          stride = fromIntegral . linePadding 24 $ w

          putVector vec = putByteString $ blitVector vec 0 (w * 3 + stride)

          putLine :: forall s. Int -> ST s (VS.Vector Word8)
          putLine line = do
              buff <- M.new $ w * 3 + stride
              let initialIndex = line * w * 3
                  inner col _ _ | col >= w = return ()
                  inner col writeIdx readIdx = do
                      let r = arr `VS.unsafeIndex` readIdx
                          g = arr `VS.unsafeIndex` (readIdx + 1)
                          b = arr `VS.unsafeIndex` (readIdx + 2)
                      
                      (buff `M.unsafeWrite` writeIdx) b
                      (buff `M.unsafeWrite` (writeIdx + 1)) g
                      (buff `M.unsafeWrite` (writeIdx + 2)) r

                      inner (col + 1) (writeIdx + 3) (readIdx + 3)

              inner 0 0 initialIndex
              VS.unsafeFreeze buff

decodeImageRGBA8 :: BmpInfoHeader -> (Int, Int, Int, Int) -> B.ByteString -> Image PixelRGBA8
decodeImageRGBA8 (BmpInfoHeader { width = w, height = h }) (posR, posG, posB, posA) str = Image wi hi stArray where
  wi = fromIntegral w
  hi = abs $ fromIntegral h
  stArray = runST $ do
      arr <- M.new (fromIntegral $ w * abs h * 4)
      if h > 0 then
        foldM_ (readLine arr) 0 [0 .. hi - 1]
      else
        foldM_ (readLine arr) 0 [hi - 1, hi - 2 .. 0]
      VS.unsafeFreeze arr

  stride = linePadding 32 wi -- will be 0

  readLine :: forall s. M.MVector s Word8 -> Int -> Int -> ST s Int
  readLine arr readIndex line = inner readIndex writeIndex where
    lastIndex = wi * (hi - 1 - line + 1) * 4
    writeIndex = wi * (hi - 1 - line) * 4

    inner readIdx writeIdx | writeIdx >= lastIndex = return $ readIdx + stride
    inner readIdx writeIdx = do
        -- 32-bit BMP pixels are BGRA
        (arr `M.unsafeWrite`  writeIdx     ) (str `B.index` (readIdx + posR))
        (arr `M.unsafeWrite` (writeIdx + 1)) (str `B.index` (readIdx + posG))
        (arr `M.unsafeWrite` (writeIdx + 2)) (str `B.index` (readIdx + posB))
        (arr `M.unsafeWrite` (writeIdx + 3)) (str `B.index` (readIdx + posA))
        inner (readIdx + 4) (writeIdx + 4)

decodeImageRGB8 :: BmpInfoHeader -> B.ByteString -> Image PixelRGB8
decodeImageRGB8 (BmpInfoHeader { width = w, height = h }) str = Image wi hi stArray where
  wi = fromIntegral w
  hi = abs $ fromIntegral h
  stArray = runST $ do
      arr <- M.new (fromIntegral $ w * abs h * 3)
      if h > 0 then
        foldM_ (readLine arr) 0 [0 .. hi - 1]
      else
        foldM_ (readLine arr) 0 [hi - 1, hi - 2 .. 0]
      VS.unsafeFreeze arr

  stride = linePadding 24 wi

  readLine :: forall s. M.MVector s Word8 -> Int -> Int -> ST s Int
  readLine arr readIndex line = inner readIndex writeIndex where
    lastIndex = wi * (hi - 1 - line + 1) * 3
    writeIndex = wi * (hi - 1 - line) * 3

    inner readIdx writeIdx | writeIdx >= lastIndex = return $ readIdx + stride
    inner readIdx writeIdx = do
        (arr `M.unsafeWrite`  writeIdx     ) (str `B.index` (readIdx + 2))
        (arr `M.unsafeWrite` (writeIdx + 1)) (str `B.index` (readIdx + 1))
        (arr `M.unsafeWrite` (writeIdx + 2)) (str `B.index`  readIdx)
        inner (readIdx + 3) (writeIdx + 3)

decodeImageY8 :: BmpInfoHeader -> B.ByteString -> Image Pixel8
decodeImageY8 (BmpInfoHeader { width = w, height = h }) str = Image wi hi stArray where
  wi = fromIntegral w
  hi = abs $ fromIntegral h
  stArray = runST $ do
      arr <- M.new . fromIntegral $ w * abs h
      if h > 0 then
        foldM_ (readLine arr) 0 [0 .. hi - 1]
      else
        foldM_ (readLine arr) 0 [hi - 1, hi - 2 .. 0]
      VS.unsafeFreeze arr

  stride = linePadding 8 wi
  
  readLine :: forall s. M.MVector s Word8 -> Int -> Int -> ST s Int
  readLine arr readIndex line = inner readIndex writeIndex where
    lastIndex = wi * (hi - 1 - line + 1)
    writeIndex = wi * (hi - 1 - line)

    inner readIdx writeIdx | writeIdx >= lastIndex = return $ readIdx + stride
    inner readIdx writeIdx = do
      (arr `M.unsafeWrite` writeIdx) (str `B.index` readIdx)
      inner (readIdx + 1) (writeIdx + 1)


pixelGet :: Get [Word8]
pixelGet = do
    b <- getWord8
    g <- getWord8
    r <- getWord8
    _ <- getWord8
    return $ [r, g, b]

metadataOfHeader :: BmpInfoHeader -> Metadatas
metadataOfHeader hdr = 
  Met.simpleMetadata Met.SourceBitmap (width hdr) (abs $ height hdr) dpiX dpiY
  where
    dpiX = Met.dotsPerMeterToDotPerInch . fromIntegral $ xResolution hdr
    dpiY = Met.dotsPerMeterToDotPerInch . fromIntegral $ yResolution hdr

-- | Try to decode a bitmap image.
-- Right now this function can output the following image:
--
--   - 'ImageY8'
--
--   - 'ImageRGB8'
--
--   - 'ImageRGBA8'
--
decodeBitmap :: B.ByteString -> Either String DynamicImage
decodeBitmap = fmap fst . decodeBitmapWithMetadata

-- | Same as 'decodeBitmap' but also extracts metadata.
decodeBitmapWithMetadata :: B.ByteString -> Either String (DynamicImage, Metadatas)
decodeBitmapWithMetadata byte =
  first palettedToTrueColor <$> decodeBitmapWithPaletteAndMetadata byte

-- | Same as 'decodeBitmap' but also extracts metadata and provide separated palette.
decodeBitmapWithPaletteAndMetadata :: B.ByteString -> Either String (PalettedImage, Metadatas)
decodeBitmapWithPaletteAndMetadata str = flip runGetStrict str $ do
  hdr      <- get :: Get BmpHeader
  bmpHeader <- get :: Get BmpInfoHeader

  readed <- bytesRead
  when (readed > fromIntegral (dataOffset hdr))
       (fail "Invalid bmp image, data in header")
  
  when (width bmpHeader <= 0)
       (fail $ "Invalid bmp width, " ++ show (width bmpHeader))

  when (height bmpHeader == 0)
       (fail $ "Invalid bmp height (0) ")

  let bpp = fromIntegral $ bitPerPixel bmpHeader :: Int
      paletteColorCount
        | colorCount bmpHeader == 0 = 2 ^ bpp
        | otherwise = fromIntegral $ colorCount bmpHeader
      getData = do
        readed' <- bytesRead
        skip . fromIntegral $ dataOffset hdr - fromIntegral readed'
        getRemainingBytes
      addMetadata i = (i, metadataOfHeader bmpHeader)

  case (bitPerPixel bmpHeader, planes  bmpHeader,
              bitmapCompression bmpHeader) of
    (32, 1, 0) -> do
      rest <- getData
      return . addMetadata . TrueColorImage . ImageRGBA8 
             $ decodeImageRGBA8 bmpHeader (2, 1, 0, 3) rest
      -- (2, 1, 0, 3) means BGRA pixel order
    (32, 1, 3) -> do
      posRed   <- getBitfield
      posGreen <- getBitfield
      posBlue  <- getBitfield
      posAlpha <- getBitfield
      rest     <- getData
      return . addMetadata . TrueColorImage . ImageRGBA8 $
        decodeImageRGBA8 bmpHeader (posRed, posGreen, posBlue, posAlpha) rest
    (24, 1, 0) -> do
      rest <- getData
      return . addMetadata . TrueColorImage . ImageRGB8  $ 
        decodeImageRGB8  bmpHeader rest
    ( 8, 1, 0) -> do
      table <- replicateM paletteColorCount pixelGet
      rest <- getData
      let palette = Palette'
            { _paletteSize = paletteColorCount
            , _paletteData = VS.fromListN (paletteColorCount * 3) $ concat table
            }
      return . addMetadata $ PalettedRGB8 (decodeImageY8 bmpHeader rest) palette

    a          -> fail $ "Can't handle BMP file " ++ show a


getBitfield :: Get Int
getBitfield = do
  w32 <- getWord32be
  case w32 of
    0xFF000000 -> return 0
    0x00FF0000 -> return 1
    0x0000FF00 -> return 2
    0x000000FF -> return 3
    _          -> fail $
      "Codec.Picture.Bitmap.getBitfield: unsupported bitfield of " ++ show w32

-- | Write an image in a file use the bitmap format.
writeBitmap :: (BmpEncodable pixel)
            => FilePath -> Image pixel -> IO ()
writeBitmap filename img = L.writeFile filename $ encodeBitmap img

linePadding :: Int -> Int -> Int
linePadding bpp imgWidth = (4 - (bytesPerLine `mod` 4)) `mod` 4
    where bytesPerLine = imgWidth * (fromIntegral bpp `div` 8)

-- | Encode an image into a bytestring in .bmp format ready to be written
-- on disk.
encodeBitmap :: forall pixel. (BmpEncodable pixel) => Image pixel -> L.ByteString
encodeBitmap = encodeBitmapWithPalette (defaultPalette (undefined :: pixel))

-- | Equivalent to 'encodeBitmap' but also store
-- the following metadatas:
--
--  * 'Codec.Picture.Metadata.DpiX'
--  * 'Codec.Picture.Metadata.DpiY' 
--
encodeBitmapWithMetadata :: forall pixel. BmpEncodable pixel
                         => Metadatas -> Image pixel -> L.ByteString
encodeBitmapWithMetadata metas =
  encodeBitmapWithPaletteAndMetadata metas (defaultPalette (undefined :: pixel))

-- | Write a dynamic image in a .bmp image file if possible.
-- The same restriction as 'encodeDynamicBitmap' apply.
writeDynamicBitmap :: FilePath -> DynamicImage -> IO (Either String Bool)
writeDynamicBitmap path img = case encodeDynamicBitmap img of
        Left err -> return $ Left err
        Right b  -> L.writeFile path b >> return (Right True)

-- | Encode a dynamic image in BMP if possible, supported images are:
--
--   - 'ImageY8'
--
--   - 'ImageRGB8'
--
--   - 'ImageRGBA8'
--
encodeDynamicBitmap :: DynamicImage -> Either String L.ByteString
encodeDynamicBitmap (ImageRGB8 img) = Right $ encodeBitmap img
encodeDynamicBitmap (ImageRGBA8 img) = Right $ encodeBitmap img
encodeDynamicBitmap (ImageY8 img) = Right $ encodeBitmap img
encodeDynamicBitmap _ = Left "Unsupported image format for bitmap export"

extractDpiOfMetadata :: Metadatas -> (Word32, Word32)
extractDpiOfMetadata metas = (fetch Met.DpiX, fetch Met.DpiY) where
  fetch k = fromMaybe 0
          $ fromIntegral . Met.dotPerInchToDotsPerMeter <$> Met.lookup k metas

-- | Convert an image to a bytestring ready to be serialized.
encodeBitmapWithPalette :: forall pixel. (BmpEncodable pixel)
                        => BmpPalette -> Image pixel -> L.ByteString
encodeBitmapWithPalette = encodeBitmapWithPaletteAndMetadata mempty

-- | Equivalent to 'encodeBitmapWithPalette' but also store
-- the following metadatas:
--
--  * 'Codec.Picture.Metadata.DpiX'
--  * 'Codec.Picture.Metadata.DpiY' 
--
encodeBitmapWithPaletteAndMetadata :: forall pixel. (BmpEncodable pixel)
                                   => Metadatas -> BmpPalette -> Image pixel
                                   -> L.ByteString
encodeBitmapWithPaletteAndMetadata metas pal@(BmpPalette palette) img =
  runPut $ put hdr >> put info >> putPalette pal >> bmpEncode img
    where imgWidth = fromIntegral $ imageWidth img
          imgHeight = fromIntegral $ imageHeight img
          (dpiX, dpiY) = extractDpiOfMetadata metas

          paletteSize = fromIntegral $ length palette
          bpp = bitsPerPixel (undefined :: pixel)
          padding = linePadding bpp imgWidth
          imagePixelSize = fromIntegral $ (imgWidth * div bpp 8 + padding) * imgHeight
          hdr = BmpHeader {
              magicIdentifier = bitmapMagicIdentifier,
              fileSize = sizeofBmpHeader + sizeofBmpInfo + 4 * paletteSize + imagePixelSize,
              reserved1 = 0,
              reserved2 = 0,
              dataOffset = sizeofBmpHeader + sizeofBmpInfo + 4 * paletteSize
          }

          info = BmpInfoHeader {
              size = sizeofBmpInfo,
              width = fromIntegral imgWidth,
              height = fromIntegral imgHeight,
              planes = 1,
              bitPerPixel = fromIntegral bpp,
              bitmapCompression = 0, -- no compression
              byteImageSize = imagePixelSize,
              xResolution = fromIntegral dpiX,
              yResolution = fromIntegral dpiY,
              colorCount = 0,
              importantColours = paletteSize
          }


{-# ANN module "HLint: ignore Reduce duplication" #-}

