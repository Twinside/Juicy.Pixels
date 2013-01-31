{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | Module implementing a basic png export, no filtering is applyed, but
-- export at least valid images.
module Codec.Picture.Png.Export( PngSavable( .. )
                               , writePng
                               , encodeDynamicPng
                               , writeDynamicPng
                               ) where

import Data.Binary( encode )
import Data.Word(Word8)
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lb

import Codec.Picture.Types
import Codec.Picture.Png.Type
import Codec.Picture.VectorByteConversion

-- | Encode an image into a png if possible.
class PngSavable a where
    -- | Transform an image into a png encoded bytestring, ready
    -- to be writte as a file.
    encodePng :: Image a -> Lb.ByteString

preparePngHeader :: Image a -> PngImageType -> Word8 -> PngIHdr
preparePngHeader (Image { imageWidth = w, imageHeight = h }) imgType depth = PngIHdr
    { width             = fromIntegral w
    , height            = fromIntegral h
    , bitDepth          = depth
    , colourType        = imgType
    , compressionMethod = 0
    , filterMethod      = 0
    , interlaceMethod   = PngNoInterlace
    }

-- | Helper function to directly write an image as a png on disk.
writePng :: (PngSavable pixel) => FilePath -> Image pixel -> IO ()
writePng path img = Lb.writeFile path $ encodePng img

endChunk :: PngRawChunk
endChunk = PngRawChunk { chunkLength = 0
                       , chunkType = iENDSignature
                       , chunkCRC = pngComputeCrc [iENDSignature]
                       , chunkData = Lb.empty
                       }


prepareIDatChunk :: Lb.ByteString -> PngRawChunk
prepareIDatChunk imgData = PngRawChunk
    { chunkLength = fromIntegral $ Lb.length imgData
    , chunkType   = iDATSignature
    , chunkCRC    = pngComputeCrc [iDATSignature, imgData]
    , chunkData   = imgData
    }

genericEncodePng :: (PixelBaseComponent a ~ Word8)
                 => PngImageType -> Int -> Image a -> Lb.ByteString
genericEncodePng imgKind compCount 
                 image@(Image { imageWidth = w, imageHeight = h, imageData = arr }) =
  encode PngRawImage { header = hdr, chunks = [prepareIDatChunk imgEncodedData, endChunk]}
    where hdr = preparePngHeader image imgKind 8
          zero = B.singleton 0

          lineSize = compCount * w
          encodeLine line = blitVector arr (line * lineSize) lineSize
          imgEncodedData = Z.compress . Lb.fromChunks
                        $ concat [[zero, encodeLine line] | line <- [0 .. h - 1]]

instance PngSavable PixelRGBA8 where
    encodePng = genericEncodePng PngTrueColourWithAlpha 4
        
instance PngSavable PixelRGB8 where
    encodePng = genericEncodePng PngTrueColour 3

instance PngSavable Pixel8 where
    encodePng = genericEncodePng PngGreyscale 1

instance PngSavable PixelYA8 where
    encodePng = genericEncodePng PngGreyscaleWithAlpha 2

-- | Write a dynamic image in a .png image file if possible.
-- The same restriction as encodeDynamicPng apply.
writeDynamicPng :: FilePath -> DynamicImage -> IO (Either String Bool)
writeDynamicPng path img = case encodeDynamicPng img of
        Left err -> return $ Left err
        Right b  -> Lb.writeFile path b >> return (Right True)

-- | Encode a dynamic image in bmp if possible, supported pixel type are :
--
--   - Y8
--
--   - YA8
--
--   - RGB8
--
--   - RGBA8
--
encodeDynamicPng :: DynamicImage -> Either String Lb.ByteString
encodeDynamicPng (ImageRGB8 img) = Right $ encodePng img
encodeDynamicPng (ImageRGBA8 img) = Right $ encodePng img
encodeDynamicPng (ImageY8 img) = Right $ encodePng img
encodeDynamicPng (ImageYA8 img) = Right $ encodePng img
encodeDynamicPng _ = Left "Unsupported image format for PNG export"
