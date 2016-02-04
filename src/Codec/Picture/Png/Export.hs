{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Module implementing a basic png export, no filtering is applyed, but
-- export at least valid images.
module Codec.Picture.Png.Export( PngSavable( .. )
                               , writePng
                               , encodeDynamicPng
                               , writeDynamicPng
                               , encodePalettedPng
                               , encodePalettedPngWithMetadata
                               ) where
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mempty )
#endif

import Control.Monad( forM_ )
import Control.Monad.ST( ST, runST )
import Data.Bits( unsafeShiftR, (.&.) )
import Data.Binary( encode )
import Data.Monoid( (<>) )
import Data.Word(Word8, Word16)
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lb

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as M

import Codec.Picture.Types
import Codec.Picture.Png.Type
import Codec.Picture.Png.Metadata
import Codec.Picture.Metadata( Metadatas )
import Codec.Picture.VectorByteConversion( blitVector, toByteString )

-- | Encode an image into a png if possible.
class PngSavable a where
    -- | Transform an image into a png encoded bytestring, ready
    -- to be written as a file.
    encodePng :: Image a -> Lb.ByteString
    encodePng = encodePngWithMetadata mempty

    -- | Encode a png using some metadatas. The following metadata keys will
    -- be stored in a `tEXt` field :
    --
    --  * 'Codec.Picture.Metadata.Title'
    --  * 'Codec.Picture.Metadata.Description'
    --  * 'Codec.Picture.Metadata.Author'
    --  * 'Codec.Picture.Metadata.Copyright'
    --  * 'Codec.Picture.Metadata.Software'
    --  * 'Codec.Picture.Metadata.Comment'
    --  * 'Codec.Picture.Metadata.Disclaimer'
    --  * 'Codec.Picture.Metadata.Source'
    --  * 'Codec.Picture.Metadata.Warning'
    --  * 'Codec.Picture.Metadata.Unknown' using the key present in the constructor.
    -- 
    -- the followings metadata will bes tored in the `gAMA` chunk.
    --
    --  * 'Codec.Picture.Metadata.Gamma'
    --
    -- The followings metadata will be stored in a `pHYs` chunk
    --
    --  * 'Codec.Picture.Metadata.DpiX'
    --  * 'Codec.Picture.Metadata.DpiY' 
    encodePngWithMetadata :: Metadatas -> Image a -> Lb.ByteString

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
endChunk = mkRawChunk iENDSignature mempty

prepareIDatChunk :: Lb.ByteString -> PngRawChunk
prepareIDatChunk = mkRawChunk iDATSignature

genericEncode16BitsPng :: forall px. (Pixel px, PixelBaseComponent px ~ Word16)
                       => PngImageType -> Metadatas -> Image px -> Lb.ByteString
genericEncode16BitsPng imgKind metas
                 image@(Image { imageWidth = w, imageHeight = h, imageData = arr }) =
  encode PngRawImage { header = hdr
                     , chunks = encodeMetadatas metas 
                              <> [ prepareIDatChunk imgEncodedData
                                 , endChunk
                                 ]
                     }
    where hdr = preparePngHeader image imgKind 16
          zero = B.singleton 0
          compCount = componentCount (undefined :: px)

          lineSize = compCount * w
          blitToByteString vec = blitVector vec 0 (lineSize * 2)
          encodeLine line = blitToByteString $ runST $ do
              finalVec <- M.new $ lineSize * 2 :: ST s (M.STVector s Word8)
              let baseIndex = line * lineSize
              forM_ [0 ..  lineSize - 1] $ \ix -> do
                  let v = arr `VS.unsafeIndex` (baseIndex + ix)
                      high = fromIntegral $ (v `unsafeShiftR` 8) .&. 0xFF
                      low = fromIntegral $ v .&. 0xFF

                  (finalVec `M.unsafeWrite` (ix * 2 + 0)) high
                  (finalVec `M.unsafeWrite` (ix * 2 + 1)) low

              VS.unsafeFreeze finalVec

          imgEncodedData = Z.compress . Lb.fromChunks
                        $ concat [[zero, encodeLine line] | line <- [0 .. h - 1]]

preparePalette :: Palette -> PngRawChunk
preparePalette pal = PngRawChunk
  { chunkLength = fromIntegral $ imageWidth pal * 3
  , chunkType   = pLTESignature
  , chunkCRC    = pngComputeCrc [pLTESignature, binaryData]
  , chunkData   = binaryData
  }
   where binaryData = Lb.fromChunks [toByteString $ imageData pal]

genericEncodePng :: forall px. (Pixel px, PixelBaseComponent px ~ Word8)
                 => Maybe Palette -> PngImageType -> Metadatas -> Image px
                 -> Lb.ByteString
genericEncodePng palette imgKind metas
                 image@(Image { imageWidth = w, imageHeight = h, imageData = arr }) =
  encode PngRawImage { header = hdr
                     , chunks = encodeMetadatas metas
                              <> prependPalette palette
                                    [ prepareIDatChunk imgEncodedData
                                    , endChunk]}
    where hdr = preparePngHeader image imgKind 8
          zero = B.singleton 0
          compCount = componentCount (undefined :: px)

          prependPalette Nothing l = l
          prependPalette (Just p) l = preparePalette p : l

          lineSize = compCount * w
          encodeLine line = blitVector arr (line * lineSize) lineSize
          imgEncodedData = Z.compress . Lb.fromChunks
                        $ concat [[zero, encodeLine line] | line <- [0 .. h - 1]]

instance PngSavable PixelRGBA8 where
  encodePngWithMetadata = genericEncodePng Nothing PngTrueColourWithAlpha

instance PngSavable PixelRGB8 where
  encodePngWithMetadata = genericEncodePng Nothing PngTrueColour

instance PngSavable Pixel8 where
  encodePngWithMetadata = genericEncodePng Nothing PngGreyscale

instance PngSavable PixelYA8 where
  encodePngWithMetadata = genericEncodePng Nothing PngGreyscaleWithAlpha

instance PngSavable PixelYA16 where
  encodePngWithMetadata = genericEncode16BitsPng PngGreyscaleWithAlpha

instance PngSavable Pixel16 where
  encodePngWithMetadata = genericEncode16BitsPng PngGreyscale

instance PngSavable PixelRGB16 where
  encodePngWithMetadata = genericEncode16BitsPng PngTrueColour

instance PngSavable PixelRGBA16 where
  encodePngWithMetadata = genericEncode16BitsPng PngTrueColourWithAlpha

-- | Write a dynamic image in a .png image file if possible.
-- The same restriction as encodeDynamicPng apply.
writeDynamicPng :: FilePath -> DynamicImage -> IO (Either String Bool)
writeDynamicPng path img = case encodeDynamicPng img of
        Left err -> return $ Left err
        Right b  -> Lb.writeFile path b >> return (Right True)

-- | Encode a paletted image as a color indexed 8-bit PNG.
-- the palette must have between 1 and 256 values in it.
encodePalettedPng :: Palette -> Image Pixel8 -> Either String Lb.ByteString
encodePalettedPng = encodePalettedPngWithMetadata mempty

-- | Equivalent to 'encodePalettedPng' but allow writing of metadatas.
encodePalettedPngWithMetadata :: Metadatas -> Palette -> Image Pixel8 -> Either String Lb.ByteString
encodePalettedPngWithMetadata metas pal img
    | w <= 0 || w > 256 || h /= 1 = Left "Invalid palette"
    | VS.any isTooBig $ imageData img =
        Left "Image contains indexes absent from the palette"
    | otherwise = Right $ genericEncodePng (Just pal) PngIndexedColor metas img
      where w = imageWidth pal
            h = imageHeight pal
            isTooBig v = fromIntegral v >= w

-- | Encode a dynamic image in PNG if possible, supported images are:
--
--   * 'ImageY8'
--
--   * 'ImageY16'
--
--   * 'ImageYA8'
--
--   * 'ImageYA16'
--
--   * 'ImageRGB8'
--
--   * 'ImageRGB16'
--
--   * 'ImageRGBA8'
--
--   * 'ImageRGBA16'
--
encodeDynamicPng :: DynamicImage -> Either String Lb.ByteString
encodeDynamicPng (ImageRGB8 img) = Right $ encodePng img
encodeDynamicPng (ImageRGBA8 img) = Right $ encodePng img
encodeDynamicPng (ImageY8 img) = Right $ encodePng img
encodeDynamicPng (ImageY16 img) = Right $ encodePng img
encodeDynamicPng (ImageYA8 img) = Right $ encodePng img
encodeDynamicPng (ImageYA16 img) = Right $ encodePng img
encodeDynamicPng (ImageRGB16 img) = Right $ encodePng img
encodeDynamicPng (ImageRGBA16 img) = Right $ encodePng img
encodeDynamicPng _ = Left "Unsupported image format for PNG export"
