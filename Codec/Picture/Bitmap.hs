{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Modules used for Bitmap file (.bmp) file loading and writing
module Codec.Picture.Bitmap( -- * Functions
                             writeBitmap
                           , encodeBitmap
                           , decodeBitmap
                             -- * Accepted formt in output
                           , BmpEncodable()
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

-- | All the instance of this class can be written as a bitmap file
-- using this library.
class BmpEncodable pixel where
    bitsPerPixel :: pixel -> Word16
    bmpEncode    :: Image pixel -> Put

instance BmpEncodable PixelRGBA8 where
    bitsPerPixel _ = 32
    bmpEncode arr = mapM_ put [arr ! (col, line) | line <- [h, h-1..0], col <- [0..w]]
        where (_, (w,h)) = bounds arr

instance BmpEncodable PixelRGB8 where
    bitsPerPixel _ = 24
    bmpEncode arr = mapM_ putLine [h, h - 1..0]
        where (_, (w,h)) = bounds arr
              swapBlueRedRGB8 (PixelRGB8 r g b) = PixelRGB8 b g r
              stride = fromIntegral . linePadding 24 $ w + 1
              putLine line = do
                  mapM_ put [swapBlueRedRGB8 $ arr ! (col, line) | col <- [0..w]]
                  replicateM_ stride $ put (0 :: Word8)

-- | Try to decode a bitmap image
decodeBitmap :: B.ByteString -> Either String DynamicImage
decodeBitmap str = flip runGet str $ do
    _hdr      <- (get :: Get BmpHeader)
    bmpHeader <- get
    case (bitPerPixel bmpHeader,
                planes  bmpHeader,
                bitmapCompression bmpHeader) of
         (32, 1, 0) -> fail "Meuh"
         (24, 1, 0) -> fail "Meuh"
         _          -> fail "Can't handle BMP file"

-- | Write an image in a file use the bitmap format.
writeBitmap :: (IArray UArray pixel, BmpEncodable pixel) 
            => FilePath -> Image pixel -> IO ()
writeBitmap filename img = B.writeFile filename $ encodeBitmap img

linePadding :: Word16 -> Word32 -> Word32
linePadding bpp imgWidth = (4 - (bytesPerLine `mod` 4)) `mod` 4
    where bytesPerLine = imgWidth * (fromIntegral bpp `div` 8)

-- | Convert an image to a bytestring ready to be serialized.
encodeBitmap :: forall pixel. (IArray UArray pixel, BmpEncodable pixel) 
                 => Image pixel -> B.ByteString
encodeBitmap img = runPut $ put hdr >> put info >> bmpEncode img
    where (_, (imgWidth, imgHeight)) = bounds img

          bpp = bitsPerPixel (undefined :: pixel)
          padding = linePadding bpp (imgWidth + 1)
          imagePixelSize = (imgWidth + 1 + padding) * (imgHeight + 1) * 4
          hdr = BmpHeader {
              magicIdentifier = bitmapMagicIdentifier,
              fileSize = sizeofBmpHeader + sizeofBmpInfo + imagePixelSize,
              reserved1 = 0,
              reserved2 = 0,
              dataOffset = sizeofBmpHeader + sizeofBmpInfo
          }

          info = BmpInfoHeader {
              size = sizeofBmpInfo,
              width = imgWidth + 1,
              height = imgHeight + 1,
              planes = 1,
              bitPerPixel = bpp,
              bitmapCompression = 0, -- no compression
              byteImageSize = imagePixelSize,
              xResolution = 0,
              yResolution = 0,
              colorCount = 0,
              importantColours = 0
          }

