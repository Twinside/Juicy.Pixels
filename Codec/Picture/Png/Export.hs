{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Codec.Picture.Png.Export where

import Data.Serialize(encode)
import Data.Array.Unboxed(IArray, UArray, (!))
import Data.Word(Word8)
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lb

import Codec.Picture.Types
import Codec.Picture.Png.Type

-- | Encode an image into a png if possible.
class PngSavable a where
    -- | Real encoding function.
    encodePng :: Image a -> B.ByteString

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

writePng :: (IArray UArray pixel, PngSavable pixel)
             => FilePath -> Image pixel -> IO ()
writePng path img = B.writeFile path $ encodePng img

endChunk :: PngRawChunk
endChunk = PngRawChunk { chunkLength = 0
                       , chunkType = iENDSignature
                       , chunkCRC = pngComputeCrc [iENDSignature]
                       , chunkData = B.empty
                       }


prepareIDatChunk :: B.ByteString -> PngRawChunk
prepareIDatChunk imgData = PngRawChunk
    { chunkLength = fromIntegral $ B.length imgData
    , chunkType   = iDATSignature
    , chunkCRC    = pngComputeCrc [iDATSignature, imgData]
    , chunkData   = imgData
    }

genericEncodePng :: PngImageType -> Int -> Image a -> B.ByteString
genericEncodePng imgKind compCount 
                 image@(Image { imageWidth = w, imageHeight = h, imageData = arr }) =
  encode PngRawImage { header = hdr, chunks = [prepareIDatChunk strictEncoded, endChunk]}
    where hdr = preparePngHeader image imgKind 8
          compBound = compCount - 1
          encodeLine line =
            0 : [arr ! ((line * w + column) * 4 + comp) | column <- [0 .. w - 1]
                                                        , comp <- [0 .. compBound]]
          imgEncodedData = Z.compress . Lb.pack 
                         $ concat [encodeLine line | line <- [0 .. h - 1]]
          strictEncoded = B.concat $ Lb.toChunks imgEncodedData

instance PngSavable PixelRGBA8 where
    encodePng = genericEncodePng PngTrueColourWithAlpha 4
        
instance PngSavable PixelRGB8 where
    encodePng = genericEncodePng PngTrueColour 3

instance PngSavable Pixel8 where
    encodePng = genericEncodePng PngGreyscale 1

