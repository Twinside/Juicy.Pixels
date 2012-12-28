module Codec.Picture.Saving( imageToJpg
                           , imageToPng
                           , imageToBitmap
                           ) where

import qualified Data.ByteString.Lazy as L
import Codec.Picture.Bitmap
import Codec.Picture.Jpg
import Codec.Picture.Png
import Codec.Picture.Types

-- | This function will try to do anything to encode an image
-- as JPEG, make all color conversion and such. Equivalent
-- of 'decodeImage' for jpeg encoding
imageToJpg :: Int -> DynamicImage -> L.ByteString
imageToJpg quality dynImage =
    let encodeAtQuality = encodeJpegAtQuality (fromIntegral quality)
    in case dynImage of
        ImageYCbCr8 img -> encodeAtQuality img
        ImageRGB8   img -> encodeAtQuality (convertImage img)
        ImageRGBA8  img -> encodeAtQuality (convertImage $ dropAlphaLayer img)
        ImageY8     img -> encodeAtQuality . convertImage
                                           $ (promoteImage img :: Image PixelRGB8)
        ImageYA8    img -> encodeAtQuality $
                            convertImage (promoteImage $ dropAlphaLayer img :: Image PixelRGB8)

-- | This function will try to do anything to encode an image
-- as PNG, make all color conversion and such. Equivalent
-- of 'decodeImage' for PNG encoding
imageToPng :: DynamicImage -> L.ByteString
imageToPng (ImageYCbCr8 img) = encodePng (convertImage img :: Image PixelRGB8)
imageToPng (ImageRGB8   img) = encodePng img
imageToPng (ImageRGBA8  img) = encodePng img
imageToPng (ImageY8     img) = encodePng img
imageToPng (ImageYA8    img) = encodePng (promoteImage img :: Image PixelRGBA8)

-- | This function will try to do anything to encode an image
-- as bitmap, make all color conversion and such. Equivalent
-- of 'decodeImage' for Bitmap encoding
imageToBitmap :: DynamicImage -> L.ByteString
imageToBitmap (ImageYCbCr8 img) = encodeBitmap (convertImage img :: Image PixelRGB8)
imageToBitmap (ImageRGB8   img) = encodeBitmap img
imageToBitmap (ImageRGBA8  img) = encodeBitmap img
imageToBitmap (ImageY8     img) = encodeBitmap img
imageToBitmap (ImageYA8    img) = encodeBitmap (promoteImage img :: Image PixelRGBA8)

