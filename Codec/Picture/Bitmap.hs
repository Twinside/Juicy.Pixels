{-# LANGUAGE FlexibleInstances #-}
module Codec.Picture.Bitmap( -- * Functions
                             writeBitmapFile
                           ) where

import Data.Array.Unboxed
import Data.Serialize
import Data.Word

import qualified Data.ByteString as B
{-import qualified Data.ByteString.Lazy as Lb-}

import Codec.Picture.Types

data BmpHeader = BmpHeader
    { magicIdentifier :: !Word16
    , fileSize        :: !Word32 -- ^ in bytes
    , reserved1       :: !Word16
    , reserved2       :: !Word16
    , dataOffset      :: !Word32
    }


instance Serialize BmpHeader where
    put hdr = do
        putWord16le $ magicIdentifier hdr
        putWord32le $ fileSize hdr
        putWord16le $ reserved1 hdr
        putWord16le $ reserved2 hdr
        putWord32le $ dataOffset hdr

    get = do
        ident <- getWord16le
        fsize <- getWord32le
        r1 <- getWord16le
        r2 <- getWord16le
        offset <- getWord32le
        return $ BmpHeader
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

    get = error "Unimplemented"

data BmpImage = BmpImage (BmpHeader, BmpInfoHeader, Image PixelRGBA8)

instance Serialize BmpImage where
    put (BmpImage (hdr, ihdr, img)) = put hdr >> put ihdr >> bmpEncode img
    get = error "Unimplemented"

bmpEncode :: Image PixelRGBA8 -> Put
bmpEncode arr = mapM_ put [arr ! (col, line) | line <- [0..h], col <- [0..w]]
    where (_, (w,h)) = bounds arr


writeBitmapFile :: FilePath -> Image PixelRGBA8 -> IO ()
writeBitmapFile filename img = do
    {-mapM_ print $ elems img-}
    B.writeFile filename encodedImage
    where (_, (imgWidth, imgHeight)) = bounds img

          encodedImage = encode $ BmpImage (hdr, info, img)

          imagePixelSize = (imgWidth + 1) * (imgHeight + 1) * 4
          hdr = BmpHeader {
              magicIdentifier = 0x4D42,
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
              bitPerPixel = 32,
              bitmapCompression = 0, -- no compression
              byteImageSize = imagePixelSize,
              xResolution = 0,
              yResolution = 0,
              colorCount = 0,
              importantColours = 0
          }

