-- | Helper functions to save dynamic images to other file format
-- with automatic color space/sample format conversion done automatically.
module Codec.Picture.Saving( imageToJpg
                           , imageToPng
                           , imageToBitmap
                           , imageToRadiance
                           ) where

import Data.Word( Word8 )
import qualified Data.ByteString.Lazy as L
import Codec.Picture.Bitmap
import Codec.Picture.Jpg
import Codec.Picture.Png
import Codec.Picture.HDR
import Codec.Picture.Types

componentToLDR :: Float -> Word8
componentToLDR = truncate . (255 *) . min 1.0 . max 0.0

toStandardDef :: Image PixelRGBF -> Image PixelRGB8
toStandardDef img = pixelMap pixelConverter img
  where pixelConverter (PixelRGBF rf gf bf) = PixelRGB8 r g b
          where r = componentToLDR rf
                g = componentToLDR gf
                b = componentToLDR bf

greyScaleToStandardDef :: Image PixelF -> Image Pixel8
greyScaleToStandardDef = pixelMap componentToLDR

-- | This function will try to do anything to encode an image
-- as RADIANCE, make all color conversion and such. Equivalent
-- of 'decodeImage' for radiance encoding
imageToRadiance :: DynamicImage -> L.ByteString
imageToRadiance (ImageYCbCr8 img) =
    imageToRadiance . ImageRGB8 $ convertImage img
imageToRadiance (ImageRGB8   img) =
    imageToRadiance . ImageRGBF $ promoteImage img
imageToRadiance (ImageRGBF   img) = encodeHDR img
imageToRadiance (ImageRGBA8  img) =
    imageToRadiance . ImageRGBF . promoteImage $ dropAlphaLayer img
imageToRadiance (ImageY8     img) =
    imageToRadiance . ImageRGB8 $ promoteImage img
imageToRadiance (ImageYF     img) =
    imageToRadiance . ImageRGBF $ promoteImage img
imageToRadiance (ImageYA8    img) =
    imageToRadiance . ImageRGB8 . promoteImage $ dropAlphaLayer img

-- | This function will try to do anything to encode an image
-- as JPEG, make all color conversion and such. Equivalent
-- of 'decodeImage' for jpeg encoding
imageToJpg :: Int -> DynamicImage -> L.ByteString
imageToJpg quality dynImage =
    let encodeAtQuality = encodeJpegAtQuality (fromIntegral quality)
    in case dynImage of
        ImageYCbCr8 img -> encodeAtQuality img
        ImageRGB8   img -> encodeAtQuality (convertImage img)
        ImageRGBF   img -> imageToJpg quality . ImageRGB8 $ toStandardDef img
        ImageRGBA8  img -> encodeAtQuality (convertImage $ dropAlphaLayer img)
        ImageYF     img -> imageToJpg quality . ImageY8 $ greyScaleToStandardDef img
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
imageToPng (ImageRGBF   img) = encodePng $ toStandardDef img
imageToPng (ImageRGBA8  img) = encodePng img
imageToPng (ImageY8     img) = encodePng img
imageToPng (ImageYF     img) = encodePng $ greyScaleToStandardDef img
imageToPng (ImageYA8    img) = encodePng (promoteImage img :: Image PixelRGBA8)
imageToPng (ImageY16    img) =
    encodePng (pixelMap (\v -> fromIntegral v :: Word8) img)
imageToPng (ImageRGB16 img) = encodePng (pixelMap conv img)
  where conv (PixelRGB16 r g b) = PixelRGB8 (toW8 r) (toW8 g) (toW8 b)
        toW8 = fromIntegral . (`div` 256)

-- | This function will try to do anything to encode an image
-- as bitmap, make all color conversion and such. Equivalent
-- of 'decodeImage' for Bitmap encoding
imageToBitmap :: DynamicImage -> L.ByteString
imageToBitmap (ImageYCbCr8 img) = encodeBitmap (convertImage img :: Image PixelRGB8)
imageToBitmap (ImageRGBF   img) = encodeBitmap $ toStandardDef img
imageToBitmap (ImageRGB8   img) = encodeBitmap img
imageToBitmap (ImageRGBA8  img) = encodeBitmap img
imageToBitmap (ImageY8     img) = encodeBitmap img
imageToBitmap (ImageYF     img) = encodeBitmap $ greyScaleToStandardDef img
imageToBitmap (ImageYA8    img) = encodeBitmap (promoteImage img :: Image PixelRGBA8)

