{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Main module for image import/export into various image formats.
--
-- To use the library without thinking about it, look after 'decodeImage' and
-- 'readImage'.
--
-- Generally, the @read*@ functions read the images from a file and try to decode
-- it, and the @decode*@ functions try to decode a bytestring.
--
-- For an easy image writing use the 'saveBmpImage', 'saveJpgImage' & 'savePngImage'
-- functions
module Codec.Picture (
                     -- * Generic functions
                       readImage
                     , readImageWithMetadata
                     , decodeImage
                     , decodeImageWithMetadata
                     , decodeImageWithPaletteAndMetadata
                     , pixelMap
                     , generateImage
                     , generateFoldImage
                     , withImage
                     , palettedToTrueColor

                      -- * RGB helper functions
                     , convertRGB8
                     , convertRGBA8

                     -- * Lens compatibility
                     , Traversal
                     , imagePixels
                     , imageIPixels

                     -- * Generic image writing
                     , saveBmpImage
                     , saveJpgImage
                     , saveGifImage
                     , savePngImage
                     , saveTiffImage
                     , saveRadianceImage

                     -- * Specific image format functions
                     -- ** Bitmap handling
                     , BmpEncodable
                     , writeBitmap
                     , encodeBitmap
                     , readBitmap
                     , decodeBitmap
                     , encodeDynamicBitmap
                     , writeDynamicBitmap

                     -- ** Gif handling
                     , readGif
                     , readGifImages
                     , decodeGif
                     , decodeGifImages

                     , encodeGifImage
                     , writeGifImage
                     , encodeGifImageWithPalette
                     , writeGifImageWithPalette
                     , encodeColorReducedGifImage
                     , writeColorReducedGifImage 
                     , encodeGifImages
                     , writeGifImages

                     -- *** Gif animation
                     , GifDelay
                     , GifLooping( .. )
                     , encodeGifAnimation
                     , writeGifAnimation

                     -- ** Jpeg handling
                     , readJpeg
                     , decodeJpeg
                     , encodeJpeg
                     , encodeJpegAtQuality

                     -- ** Png handling
                     , PngSavable( .. )
                     , readPng
                     , decodePng
                     , writePng
                     , encodePalettedPng
                     , encodeDynamicPng
                     , writeDynamicPng

                     -- ** TGA handling
                     , readTGA
                     , decodeTga
                     , TgaSaveable
                     , encodeTga
                     , writeTga

                     -- ** Tiff handling
                     , readTiff
                     , TiffSaveable
                     , decodeTiff
                     , encodeTiff
                     , writeTiff

                     -- ** HDR (Radiance/RGBE) handling
                     , readHDR
                     , decodeHDR
                     , encodeHDR
                     , writeHDR

                     -- ** Color Quantization
                     , PaletteCreationMethod(..)
                     , PaletteOptions(..)
                     , palettize

                     -- * Image types and pixel types
                     -- ** Image
                     , Image( .. )
                     , DynamicImage( .. )
                     , Palette
                     -- ** Pixels
                     , Pixel( .. )
                     -- $graph
                     , Pixel8
                     , Pixel16
                     , PixelF

                     , PixelYA8( .. )
                     , PixelYA16( .. )
                     , PixelRGB8( .. )
                     , PixelRGB16( .. )
                     , PixelRGBF( .. )
                     , PixelRGBA8( .. )
                     , PixelRGBA16( .. )
                     , PixelYCbCr8( .. )
                     , PixelCMYK8( .. )
                     , PixelCMYK16( .. )

                     -- * Foreign unsafe import
                     , imageFromUnsafePtr
                     ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>) )
#endif

import Control.Arrow( first )
import Data.Bits( unsafeShiftR )
import Control.DeepSeq( NFData, deepseq )
import qualified Control.Exception as Exc ( catch, IOException )
import Codec.Picture.Metadata( Metadatas )
import Codec.Picture.Bitmap( BmpEncodable
                           , decodeBitmap
                           , decodeBitmapWithPaletteAndMetadata
                           , writeBitmap, encodeBitmap
                           , encodeDynamicBitmap, writeDynamicBitmap )
import Codec.Picture.Jpg( decodeJpeg
                        , decodeJpegWithMetadata
                        , encodeJpeg
                        , encodeJpegAtQuality )
import Codec.Picture.Png( PngSavable( .. )
                        , decodePng
                        , decodePngWithPaletteAndMetadata
                        , writePng
                        , encodeDynamicPng
                        , encodePalettedPng
                        , writeDynamicPng
                        )

import Codec.Picture.Gif( GifDelay
                        , GifLooping( .. )
                        , decodeGif
                        , decodeGifWithPaletteAndMetadata
                        , decodeGifImages
                        , encodeGifImage
                        , encodeGifImageWithPalette
                        , encodeGifImages

                        , writeGifImage
                        , writeGifImageWithPalette
                        , writeGifImages
                        )

import Codec.Picture.HDR( decodeHDR
                        , decodeHDRWithMetadata
                        , encodeHDR
                        , writeHDR
                        )
import Codec.Picture.Tiff( decodeTiff
                         , decodeTiffWithPaletteAndMetadata
                         , TiffSaveable
                         , encodeTiff
                         , writeTiff )
import Codec.Picture.Tga( TgaSaveable
                        , decodeTga
                        , decodeTgaWithPaletteAndMetadata
                        , encodeTga
                        , writeTga
                        )
import Codec.Picture.Saving
import Codec.Picture.Types
import Codec.Picture.ColorQuant
import Codec.Picture.VectorByteConversion( imageFromUnsafePtr )
-- import System.IO ( withFile, IOMode(ReadMode) )
#ifdef WITH_MMAP_BYTESTRING
import System.IO.MMap ( mmapFileByteString )
#endif

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Storable as VS

-- | Return the first Right thing, accumulating error
eitherLoad :: c -> [(String, c -> Either String b)] -> Either String b
eitherLoad v = inner ""
    where inner errAcc [] = Left $ "Cannot load file\n" ++ errAcc
          inner errAcc ((hdr, f) : rest) = case f v of
                Left  err  -> inner (errAcc ++ hdr ++ " " ++ err ++ "\n") rest
                Right rez  -> Right rez

-- | Encode a full color image to a gif by applying a color quantization
-- algorithm on it.
encodeColorReducedGifImage :: Image PixelRGB8 -> Either String L.ByteString
encodeColorReducedGifImage img = encodeGifImageWithPalette indexed pal
  where (indexed, pal) = palettize defaultPaletteOptions img

-- | Write a full color image to a gif by applying a color quantization
-- algorithm on it.
writeColorReducedGifImage :: FilePath -> Image PixelRGB8 -> Either String (IO ())
writeColorReducedGifImage path img =
    L.writeFile path <$> encodeColorReducedGifImage img


-- | Helper function to create a gif animation.
-- All the images of the animation are separated
-- by the same delay.
encodeGifAnimation :: GifDelay -> GifLooping
                   -> [Image PixelRGB8] -> Either String L.ByteString
encodeGifAnimation delay looping lst =
    encodeGifImages looping
        [(pal, delay, img)
                | (img, pal) <- palettize defaultPaletteOptions <$> lst]

-- | Helper function to write a gif animation on disk.
-- See encodeGifAnimation
writeGifAnimation :: FilePath -> GifDelay -> GifLooping
                  -> [Image PixelRGB8] -> Either String (IO ())
writeGifAnimation path delay looping img =
    L.writeFile path <$> encodeGifAnimation delay looping img

withImageDecoder :: (NFData a)
                 => (B.ByteString -> Either String a) -> FilePath
                 -> IO (Either String a)
withImageDecoder decoder path = Exc.catch doit
                    (\e -> return . Left $ show (e :: Exc.IOException))
    where doit = force . decoder <$> get
#ifdef WITH_MMAP_BYTESTRING
          get = mmapFileByteString path Nothing
#else
          get = B.readFile path
#endif
          -- force appeared in deepseq 1.3, Haskell Platform
          -- provides 1.1
          force x = x `deepseq` x

-- | Load an image file without even thinking about it, it does everything
-- as 'decodeImage'
readImage :: FilePath -> IO (Either String DynamicImage)
readImage = withImageDecoder decodeImage

-- | Equivalent to 'readImage'  but also providing metadatas.
readImageWithMetadata :: FilePath -> IO (Either String (DynamicImage, Metadatas))
readImageWithMetadata = withImageDecoder decodeImageWithMetadata


-- | If you want to decode an image in a bytestring without even thinking
-- in term of format or whatever, this is the function to use. It will try
-- to decode in each known format and if one decoding succeeds, it will return
-- the decoded image in it's own colorspace.
decodeImage :: B.ByteString -> Either String DynamicImage
decodeImage = fmap fst . decodeImageWithMetadata 

class Decimable px1 px2 where
   decimateBitDepth :: Image px1 -> Image px2

decimateWord16 :: ( Pixel px1, Pixel px2
                  , PixelBaseComponent px1 ~ Pixel16
                  , PixelBaseComponent px2 ~ Pixel8
                  ) => Image px1 -> Image px2
decimateWord16 (Image w h da) =
  Image w h $ VS.map (\v -> fromIntegral $ v `unsafeShiftR` 8) da

decimateFloat :: ( Pixel px1, Pixel px2
                 , PixelBaseComponent px1 ~ PixelF
                 , PixelBaseComponent px2 ~ Pixel8
                 ) => Image px1 -> Image px2
decimateFloat (Image w h da) =
  Image w h $ VS.map (floor . (255*) . max 0 . min 1) da

instance Decimable Pixel16 Pixel8 where
   decimateBitDepth = decimateWord16

instance Decimable PixelYA16 PixelYA8 where
   decimateBitDepth = decimateWord16

instance Decimable PixelRGB16 PixelRGB8 where
   decimateBitDepth = decimateWord16

instance Decimable PixelRGBA16 PixelRGBA8 where
   decimateBitDepth = decimateWord16

instance Decimable PixelCMYK16 PixelCMYK8 where
   decimateBitDepth = decimateWord16

instance Decimable PixelF Pixel8 where
   decimateBitDepth = decimateFloat

instance Decimable PixelRGBF PixelRGB8 where
   decimateBitDepth = decimateFloat

-- | Convert by any mean possible a dynamic image to an image
-- in RGBA. The process can lose precision while converting from
-- 16bits pixels or Floating point pixels.
convertRGBA8 :: DynamicImage -> Image PixelRGBA8
convertRGBA8 dynImage = case dynImage of
  ImageY8     img -> promoteImage img
  ImageY16    img -> promoteImage (decimateBitDepth img :: Image Pixel8)
  ImageYF     img -> promoteImage (decimateBitDepth img :: Image Pixel8)
  ImageYA8    img -> promoteImage img
  ImageYA16   img -> promoteImage (decimateBitDepth img :: Image PixelYA8)
  ImageRGB8   img -> promoteImage img
  ImageRGB16  img -> promoteImage (decimateBitDepth img :: Image PixelRGB8)
  ImageRGBF   img -> promoteImage (decimateBitDepth img :: Image PixelRGB8)
  ImageRGBA8  img -> promoteImage img
  ImageRGBA16 img -> decimateBitDepth img
  ImageYCbCr8 img -> promoteImage (convertImage img :: Image PixelRGB8)
  ImageCMYK8  img -> promoteImage (convertImage img :: Image PixelRGB8)
  ImageCMYK16 img ->
    promoteImage (convertImage (decimateBitDepth img :: Image PixelCMYK8) :: Image PixelRGB8)

-- | Convert by any mean possible a dynamic image to an image
-- in RGB. The process can lose precision while converting from
-- 16bits pixels or Floating point pixels. Any alpha layer will
-- be dropped
convertRGB8 :: DynamicImage -> Image PixelRGB8
convertRGB8 dynImage = case dynImage of
  ImageY8     img -> promoteImage img
  ImageY16    img -> promoteImage (decimateBitDepth img :: Image Pixel8)
  ImageYF     img -> promoteImage (decimateBitDepth img :: Image Pixel8)
  ImageYA8    img -> promoteImage img
  ImageYA16   img -> promoteImage (decimateBitDepth img :: Image PixelYA8)
  ImageRGB8   img -> img
  ImageRGB16  img -> decimateBitDepth img
  ImageRGBF   img -> decimateBitDepth img :: Image PixelRGB8
  ImageRGBA8  img -> dropAlphaLayer img
  ImageRGBA16 img -> dropAlphaLayer (decimateBitDepth img :: Image PixelRGBA8)
  ImageYCbCr8 img -> convertImage img
  ImageCMYK8  img -> convertImage img
  ImageCMYK16 img -> convertImage (decimateBitDepth img :: Image PixelCMYK8)

-- | Equivalent to 'decodeImage', but also provide potential metadatas
-- present in the given file and the palettes if the format provides them.
decodeImageWithPaletteAndMetadata :: B.ByteString -> Either String (PalettedImage, Metadatas)
decodeImageWithPaletteAndMetadata str = eitherLoad str
    [ ("Jpeg", fmap (first TrueColorImage) . decodeJpegWithMetadata)
    , ("PNG", decodePngWithPaletteAndMetadata)
    , ("Bitmap", decodeBitmapWithPaletteAndMetadata)
    , ("GIF", decodeGifWithPaletteAndMetadata)
    , ("HDR", fmap (first TrueColorImage) . decodeHDRWithMetadata)
    , ("Tiff", decodeTiffWithPaletteAndMetadata)
    , ("TGA", decodeTgaWithPaletteAndMetadata)
    ]

-- | Equivalent to 'decodeImage', but also provide potential metadatas
-- present in the given file.
decodeImageWithMetadata :: B.ByteString -> Either String (DynamicImage, Metadatas)
decodeImageWithMetadata =
    fmap (first palettedToTrueColor) . decodeImageWithPaletteAndMetadata

-- | Helper function trying to load a png file from a file on disk.
readPng :: FilePath -> IO (Either String DynamicImage)
readPng = withImageDecoder decodePng

-- | Helper function trying to load a gif file from a file on disk.
readGif :: FilePath -> IO (Either String DynamicImage)
readGif = withImageDecoder decodeGif

-- | Helper function trying to load tiff file from a file on disk.
readTiff :: FilePath -> IO (Either String DynamicImage)
readTiff = withImageDecoder decodeTiff

-- | Helper function trying to load all the images of an animated
-- gif file.
readGifImages :: FilePath -> IO (Either String [DynamicImage])
readGifImages = withImageDecoder decodeGifImages

-- | Try to load a jpeg file and decompress. The colorspace is still
-- YCbCr if you want to perform computation on the luma part. You can
-- convert it to RGB using 'colorSpaceConversion'.
readJpeg :: FilePath -> IO (Either String DynamicImage)
readJpeg = withImageDecoder decodeJpeg

-- | Try to load a .bmp file. The colorspace would be RGB, RGBA or Y.
readBitmap :: FilePath -> IO (Either String DynamicImage)
readBitmap = withImageDecoder decodeBitmap

-- | Try to load a .pic file. The colorspace can only be
-- RGB with floating point precision.
readHDR :: FilePath -> IO (Either String DynamicImage)
readHDR = withImageDecoder decodeHDR

-- | Try to load a .tga file from disk.
readTGA :: FilePath -> IO (Either String DynamicImage)
readTGA = withImageDecoder decodeTga

-- | Save an image to a '.jpg' file, will do everything it can to save an image.
saveJpgImage :: Int -> FilePath -> DynamicImage -> IO ()
saveJpgImage quality path img = L.writeFile path $ imageToJpg quality img

-- | Save an image to a '.gif' file, will do everything it can to save it.
saveGifImage :: FilePath -> DynamicImage -> Either String (IO ())
saveGifImage path img = L.writeFile path <$> imageToGif img

-- | Save an image to a '.tiff' file, will do everything it can to save an image.
saveTiffImage :: FilePath -> DynamicImage -> IO ()
saveTiffImage path img = L.writeFile path $ imageToTiff img

-- | Save an image to a '.hdr' file, will do everything it can to save an image.
saveRadianceImage :: FilePath -> DynamicImage -> IO ()
saveRadianceImage path = L.writeFile path . imageToRadiance

-- | Save an image to a '.png' file, will do everything it can to save an image.
-- For example, a simple transcoder to png
--
-- > transcodeToPng :: FilePath -> FilePath -> IO ()
-- > transcodeToPng pathIn pathOut = do
-- >    eitherImg <- readImage pathIn
-- >    case eitherImg of
-- >        Left _ -> return ()
-- >        Right img -> savePngImage pathOut img
--
savePngImage :: FilePath -> DynamicImage -> IO ()
savePngImage path img = L.writeFile path $ imageToPng img

-- | Save an image to a '.bmp' file, will do everything it can to save an image.
saveBmpImage :: FilePath -> DynamicImage -> IO ()
saveBmpImage path img = L.writeFile path $ imageToBitmap img

