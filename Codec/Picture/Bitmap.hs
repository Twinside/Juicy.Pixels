{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Modules used for Bitmap file (.bmp) file loading and writing
module Codec.Picture.Bitmap( -- * Functions
                             writeBitmap
                           , encodeBitmap
                           , decodeBitmap
                             -- * Accepted formt in output
                           , BmpEncodable( )
                           ) where

import Control.Monad( replicateM_, when )
import Data.Array.Unboxed
import Data.Serialize
import Data.Word

import qualified Data.ByteString as B

import Codec.Picture.Types

data BmpHeader = BmpHeader
    { magicIdentifier :: !Word16
    , fileSize        :: !Word32 -- ^ in bytes
    , reserved1       :: !Word16
    , reserved2       :: !Word16
    , dataOffset      :: !Word32
    }

bitmapMagicIdentifier :: Word16
bitmapMagicIdentifier = 0x4D42

instance Serialize BmpHeader where
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
    , width             :: !Word32
    , height            :: !Word32
    , planes            :: !Word16 -- Number of colour planes
    , bitPerPixel       :: !Word16
    , bitmapCompression :: !Word32
    , byteImageSize     :: !Word32
    , xResolution       :: !Word32 -- ^ Pixels per meter
    , yResolution       :: !Word32 -- ^ Pixels per meter
    , colorCount        :: !Word32
    , importantColours  :: !Word32
    }

sizeofBmpHeader, sizeofBmpInfo  :: Word32
sizeofBmpHeader = 2 + 4 + 2 + 2 + 4
sizeofBmpInfo = 3 * 4 + 2 * 2 + 6 * 4

instance Serialize BmpInfoHeader where
    put hdr = do
        putWord32le $ size hdr
        putWord32le $ width hdr
        putWord32le $ height hdr
        putWord16le $ planes hdr
        putWord16le $ bitPerPixel hdr
        putWord32le $ bitmapCompression hdr
        putWord32le $ byteImageSize hdr
        putWord32le $ xResolution hdr
        putWord32le $ yResolution hdr
        putWord32le $ colorCount hdr
        putWord32le $ importantColours hdr

    get = do
        readSize <- getWord32le
        readWidth <- getWord32le
        readHeight <- getWord32le
        readPlanes <- getWord16le
        readBitPerPixel <- getWord16le
        readBitmapCompression <- getWord32le
        readByteImageSize <- getWord32le
        readXResolution <- getWord32le
        readYResolution <- getWord32le
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

instance BmpEncodable Pixel8 where
    defaultPalette _ = BmpPalette [(x,x,x, 255) | x <- [0 .. 255]]
    bitsPerPixel _ = 8
    bmpEncode (Image {imageWidth = w, imageHeight = h, imageData = arr}) =
      mapM_ putLine [h - 1, h - 2..0]
        where stride = fromIntegral $ linePadding 8 w
              putLine line = do
                  let lineIdx = line * w
                  mapM_ put [arr ! (lineIdx + col)  | col <- [0..w - 1]]
                  replicateM_ stride $ put (0 :: Word8)

instance BmpEncodable PixelRGBA8 where
    bitsPerPixel _ = 32
    bmpEncode (Image {imageWidth = w, imageHeight = h, imageData = arr}) =
        mapM_ put [arr ! (pixelIdx * 4 + comp) | line <- [h-1, h-2..0]
                                               , let lineIndex = line * w
                                               , col <- [0..w - 1]
                                               , let pixelIdx = lineIndex + col
                                               , comp <- [2, 1, 0, 3]
                                               ]

instance BmpEncodable PixelRGB8 where
    bitsPerPixel _ = 24
    bmpEncode (Image {imageWidth = w, imageHeight = h, imageData = arr}) =
      mapM_ putLine [h - 1, h - 2..0]
        where stride = fromIntegral . linePadding 24 $ w
              putLine line = do
                  let lineIdx = line * w
                  mapM_ put [arr ! ((lineIdx + col) * 3 + comp) | col <- [0..w - 1], comp <- [2, 1, 0]]
                  replicateM_ stride $ put (0 :: Word8)

-- | Try to decode a bitmap image
decodeBitmap :: B.ByteString -> Either String DynamicImage
decodeBitmap str = flip runGet str $ do
    _hdr      <- (get :: Get BmpHeader)
    bmpHeader <- get
    case (bitPerPixel bmpHeader,
                planes  bmpHeader,
                bitmapCompression bmpHeader) of
         (32, 1, 0) -> {- ImageRGBA8 <$>-} fail "Meuh"
         (24, 1, 0) -> fail "Meuh"
         _          -> fail "Can't handle BMP file"

-- | Write an image in a file use the bitmap format.
writeBitmap :: (BmpEncodable pixel)
            => FilePath -> Image pixel -> IO ()
writeBitmap filename img = B.writeFile filename $ encodeBitmap img

linePadding :: Int -> Int -> Int
linePadding bpp imgWidth = (4 - (bytesPerLine `mod` 4)) `mod` 4
    where bytesPerLine = imgWidth * (fromIntegral bpp `div` 8)

encodeBitmap :: forall pixel. (BmpEncodable pixel) => Image pixel -> B.ByteString
encodeBitmap = encodeBitmapWithPalette (defaultPalette (undefined :: pixel))

-- | Convert an image to a bytestring ready to be serialized.
encodeBitmapWithPalette :: forall pixel. (BmpEncodable pixel)
                        => BmpPalette -> Image pixel -> B.ByteString
encodeBitmapWithPalette pal@(BmpPalette palette) img =
  runPut $ put hdr >> put info >> putPalette pal >> bmpEncode img
    where imgWidth = fromIntegral $ imageWidth img
          imgHeight = fromIntegral $ imageHeight img

          paletteSize = fromIntegral $ length palette

          bpp = bitsPerPixel (undefined :: pixel)
          padding = linePadding bpp (imgWidth + 1)
          imagePixelSize = fromIntegral $ (imgWidth + padding) * imgHeight * 4
          hdr = BmpHeader {
              magicIdentifier = bitmapMagicIdentifier,
              fileSize = sizeofBmpHeader + sizeofBmpInfo + 4 * paletteSize + imagePixelSize,
              reserved1 = 0,
              reserved2 = 0,
              dataOffset = sizeofBmpHeader + sizeofBmpInfo + 4 * paletteSize
          }

          info = BmpInfoHeader {
              size = sizeofBmpInfo,
              width = fromIntegral $ imgWidth,
              height = fromIntegral $ imgHeight,
              planes = 1,
              bitPerPixel = fromIntegral $ bpp,
              bitmapCompression = 0, -- no compression
              byteImageSize = imagePixelSize,
              xResolution = 0,
              yResolution = 0,
              colorCount = 0,
              importantColours = paletteSize
          }

