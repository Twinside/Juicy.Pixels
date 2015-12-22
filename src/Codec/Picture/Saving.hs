{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
-- | Helper functions to save dynamic images to other file format
-- with automatic color space/sample format conversion done automatically.
module Codec.Picture.Saving( imageToJpg
                           , imageToPng
                           , imageToGif
                           , imageToBitmap
                           , imageToTiff
                           , imageToRadiance
                           , imageToTga
                           ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mempty )
#endif

import Data.Bits( unsafeShiftR )
import Data.Word( Word8, Word16 )
import qualified Data.ByteString.Lazy as L
import Codec.Picture.Bitmap
import Codec.Picture.Jpg
import Codec.Picture.Png
import Codec.Picture.Gif
import Codec.Picture.ColorQuant
import Codec.Picture.HDR
import Codec.Picture.Types
import Codec.Picture.Tiff
import Codec.Picture.Tga

import qualified Data.Vector.Storable as V

componentToLDR :: Float -> Word8
componentToLDR = truncate . (255 *) . min 1.0 . max 0.0

toStandardDef :: Image PixelRGBF -> Image PixelRGB8
toStandardDef = pixelMap pixelConverter
  where pixelConverter (PixelRGBF rf gf bf) = PixelRGB8 r g b
          where r = componentToLDR rf
                g = componentToLDR gf
                b = componentToLDR bf

greyScaleToStandardDef :: Image PixelF -> Image Pixel8
greyScaleToStandardDef = pixelMap componentToLDR

from16to8 :: ( PixelBaseComponent source ~ Word16
             , PixelBaseComponent dest ~ Word8 )
          => Image source -> Image dest
from16to8 Image { imageWidth = w, imageHeight = h
                , imageData = arr } = Image w h transformed
   where transformed = V.map toWord8 arr
         toWord8 v = fromIntegral (v `unsafeShiftR` 8)

from16toFloat :: ( PixelBaseComponent source ~ Word16
                 , PixelBaseComponent dest ~ Float )
          => Image source -> Image dest
from16toFloat Image { imageWidth = w, imageHeight = h
                    , imageData = arr } = Image w h transformed
   where transformed = V.map toWord8 arr
         toWord8 v = fromIntegral v / 65536.0

-- | This function will try to do anything to encode an image
-- as RADIANCE, make all color conversion and such. Equivalent
-- of 'decodeImage' for radiance encoding
imageToRadiance :: DynamicImage -> L.ByteString
imageToRadiance (ImageCMYK8 img) =
    imageToRadiance . ImageRGB8 $ convertImage img
imageToRadiance (ImageCMYK16 img) =
    imageToRadiance . ImageRGB16 $ convertImage img
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
imageToRadiance (ImageY16    img) =
  imageToRadiance . ImageRGBF $ pixelMap toRgbf img
    where toRgbf v = PixelRGBF val val val
            where val = fromIntegral v / 65536.0

imageToRadiance (ImageYA16   img) =
  imageToRadiance . ImageRGBF $ pixelMap toRgbf img
    where toRgbf (PixelYA16 v _) = PixelRGBF val val val
            where val = fromIntegral v / 65536.0
imageToRadiance (ImageRGB16  img) =
    imageToRadiance . ImageRGBF $ from16toFloat img
imageToRadiance (ImageRGBA16 img) =
    imageToRadiance . ImageRGBF $ pixelMap toRgbf img
    where toRgbf (PixelRGBA16 r g b _) = PixelRGBF (f r) (f g) (f b)
            where f v = fromIntegral v / 65536.0

-- | This function will try to do anything to encode an image
-- as JPEG, make all color conversion and such. Equivalent
-- of 'decodeImage' for jpeg encoding
-- Save Y or YCbCr Jpeg only, all other colorspaces are converted.
-- To save a RGB or CMYK JPEG file, use the
-- 'Codec.Picture.Jpg.encodeDirectJpegAtQualityWithMetadata' function
imageToJpg :: Int -> DynamicImage -> L.ByteString
imageToJpg quality dynImage =
    let encodeAtQuality = encodeJpegAtQuality (fromIntegral quality)
        encodeWithMeta = encodeDirectJpegAtQualityWithMetadata (fromIntegral quality) mempty
    in case dynImage of
        ImageYCbCr8 img -> encodeAtQuality img
        ImageCMYK8  img -> imageToJpg quality . ImageRGB8 $ convertImage img
        ImageCMYK16 img -> imageToJpg quality . ImageRGB16 $ convertImage img
        ImageRGB8   img -> encodeAtQuality (convertImage img)
        ImageRGBF   img -> imageToJpg quality . ImageRGB8 $ toStandardDef img
        ImageRGBA8  img -> encodeAtQuality (convertImage $ dropAlphaLayer img)
        ImageYF     img -> imageToJpg quality . ImageY8 $ greyScaleToStandardDef img
        ImageY8     img -> encodeWithMeta img
        ImageYA8    img -> encodeWithMeta $ dropAlphaLayer img
        ImageY16    img -> imageToJpg quality . ImageY8 $ from16to8 img
        ImageYA16   img -> imageToJpg quality . ImageYA8 $ from16to8 img
        ImageRGB16  img -> imageToJpg quality . ImageRGB8 $ from16to8 img
        ImageRGBA16 img -> imageToJpg quality . ImageRGBA8 $ from16to8 img

-- | This function will try to do anything to encode an image
-- as PNG, make all color conversion and such. Equivalent
-- of 'decodeImage' for PNG encoding
imageToPng :: DynamicImage -> L.ByteString
imageToPng (ImageYCbCr8 img) = encodePng (convertImage img :: Image PixelRGB8)
imageToPng (ImageCMYK8 img)  = encodePng (convertImage img :: Image PixelRGB8)
imageToPng (ImageCMYK16 img) = encodePng (convertImage img :: Image PixelRGB16)
imageToPng (ImageRGB8   img) = encodePng img
imageToPng (ImageRGBF   img) = encodePng $ toStandardDef img
imageToPng (ImageRGBA8  img) = encodePng img
imageToPng (ImageY8     img) = encodePng img
imageToPng (ImageYF     img) = encodePng $ greyScaleToStandardDef img
imageToPng (ImageYA8    img) = encodePng img
imageToPng (ImageY16    img) = encodePng img
imageToPng (ImageYA16   img) = encodePng img
imageToPng (ImageRGB16  img) = encodePng img
imageToPng (ImageRGBA16 img) = encodePng img

-- | This function will try to do anything to encode an image
-- as a Tiff, make all color conversion and such. Equivalent
-- of 'decodeImage' for Tiff encoding
imageToTiff :: DynamicImage -> L.ByteString
imageToTiff (ImageYCbCr8 img) = encodeTiff img
imageToTiff (ImageCMYK8 img)  = encodeTiff img
imageToTiff (ImageCMYK16 img) = encodeTiff img
imageToTiff (ImageRGB8   img) = encodeTiff img
imageToTiff (ImageRGBF   img) = encodeTiff $ toStandardDef img
imageToTiff (ImageRGBA8  img) = encodeTiff img
imageToTiff (ImageY8     img) = encodeTiff img
imageToTiff (ImageYF     img) = encodeTiff $ greyScaleToStandardDef img
imageToTiff (ImageYA8    img) = encodeTiff $ dropAlphaLayer img
imageToTiff (ImageY16    img) = encodeTiff img
imageToTiff (ImageYA16   img) = encodeTiff $ dropAlphaLayer img
imageToTiff (ImageRGB16  img) = encodeTiff img
imageToTiff (ImageRGBA16 img) = encodeTiff img

-- | This function will try to do anything to encode an image
-- as bitmap, make all color conversion and such. Equivalent
-- of 'decodeImage' for Bitmap encoding
imageToBitmap :: DynamicImage -> L.ByteString
imageToBitmap (ImageYCbCr8 img) = encodeBitmap (convertImage img :: Image PixelRGB8)
imageToBitmap (ImageCMYK8  img) = encodeBitmap (convertImage img :: Image PixelRGB8)
imageToBitmap (ImageCMYK16 img) = imageToBitmap . ImageRGB16 $ convertImage img
imageToBitmap (ImageRGBF   img) = encodeBitmap $ toStandardDef img
imageToBitmap (ImageRGB8   img) = encodeBitmap img
imageToBitmap (ImageRGBA8  img) = encodeBitmap img
imageToBitmap (ImageY8     img) = encodeBitmap img
imageToBitmap (ImageYF     img) = encodeBitmap $ greyScaleToStandardDef img
imageToBitmap (ImageYA8    img) = encodeBitmap (promoteImage img :: Image PixelRGBA8)
imageToBitmap (ImageY16    img) = imageToBitmap . ImageY8 $ from16to8 img
imageToBitmap (ImageYA16   img) = imageToBitmap . ImageYA8 $ from16to8 img
imageToBitmap (ImageRGB16  img) = imageToBitmap . ImageRGB8 $ from16to8 img
imageToBitmap (ImageRGBA16 img) = imageToBitmap . ImageRGBA8 $ from16to8 img


-- | This function will try to do anything to encode an image
-- as a gif, make all color conversion and quantization. Equivalent
-- of 'decodeImage' for gif encoding
imageToGif :: DynamicImage -> Either String L.ByteString
imageToGif (ImageYCbCr8 img) = imageToGif . ImageRGB8 $ convertImage img
imageToGif (ImageCMYK8  img) = imageToGif . ImageRGB8 $ convertImage img
imageToGif (ImageCMYK16 img) = imageToGif . ImageRGB16 $ convertImage img
imageToGif (ImageRGBF   img) = imageToGif . ImageRGB8 $ toStandardDef img
imageToGif (ImageRGB8   img) = encodeGifImageWithPalette indexed pal
  where (indexed, pal) = palettize defaultPaletteOptions img
imageToGif (ImageRGBA8  img) = imageToGif . ImageRGB8 $ dropAlphaLayer img
imageToGif (ImageY8     img) = Right $ encodeGifImage img
imageToGif (ImageYF     img) = imageToGif . ImageY8 $ greyScaleToStandardDef img
imageToGif (ImageYA8    img) = imageToGif . ImageY8 $ dropAlphaLayer img
imageToGif (ImageY16    img) = imageToGif . ImageY8 $ from16to8 img
imageToGif (ImageYA16   img) = imageToGif . ImageYA8 $ from16to8 img
imageToGif (ImageRGB16  img) = imageToGif . ImageRGB8 $ from16to8 img
imageToGif (ImageRGBA16 img) = imageToGif . ImageRGBA8 $ from16to8 img

-- | This function will try to do anything to encode an image
-- as a tga, make all color conversion and quantization. Equivalent
-- of 'decodeImage' for tga encoding
imageToTga :: DynamicImage -> L.ByteString
imageToTga (ImageYCbCr8 img) = encodeTga (convertImage img :: Image PixelRGB8)
imageToTga (ImageCMYK8  img) = encodeTga (convertImage img :: Image PixelRGB8)
imageToTga (ImageCMYK16 img) = encodeTga (from16to8 img :: Image PixelRGB8)
imageToTga (ImageRGBF   img) = encodeTga $ toStandardDef img
imageToTga (ImageRGB8   img) = encodeTga img
imageToTga (ImageRGBA8  img) = encodeTga img
imageToTga (ImageY8     img) = encodeTga img
imageToTga (ImageYF     img) = encodeTga $ greyScaleToStandardDef img
imageToTga (ImageYA8    img) = encodeTga (promoteImage img :: Image PixelRGBA8)
imageToTga (ImageY16    img) = encodeTga (from16to8 img :: Image Pixel8)
imageToTga (ImageYA16   img) = encodeTga (from16to8 img :: Image PixelRGBA8)
imageToTga (ImageRGB16  img) = encodeTga (from16to8 img :: Image PixelRGB8)
imageToTga (ImageRGBA16 img) = encodeTga (from16to8 img :: Image PixelRGBA8)
