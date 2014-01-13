{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
-- | Main module for image import/export into various image formats.
--
-- To use the library without thinking about it, look after 'decodeImage' and
-- 'readImage'.
--
-- Generally, the read* functions read the images from a file and try to decode
-- it, and the decode* functions try to decode a bytestring.
--
-- For an easy image writing use the 'saveBmpImage', 'saveJpgImage' & 'savePngImage'
-- functions
module Codec.Picture (
                     -- * Generic functions
                       readImage
                     , decodeImage
                     , pixelMap
                     , generateImage
                     , generateFoldImage
                     , withImage

                     -- ** Color Quantization
                     , PaletteCreationMethod(..)
                     , PaletteOpts(..)
                     , palettize
                     , withPalette

                     -- * Generic image writing
                     , saveBmpImage
                     , saveJpgImage
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
                     , encodeGifImages
                     , writeGifImages

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
                     ) where

import Control.Applicative( (<$>) )
import Control.DeepSeq( NFData, deepseq )
import qualified Control.Exception as Exc ( catch, IOException )
import Codec.Picture.Bitmap( BmpEncodable, decodeBitmap
                           , writeBitmap, encodeBitmap
                           , encodeDynamicBitmap, writeDynamicBitmap )
import Codec.Picture.Jpg( decodeJpeg, encodeJpeg, encodeJpegAtQuality )
import Codec.Picture.Png( PngSavable( .. ), decodePng, writePng
                        , encodeDynamicPng
                        , encodePalettedPng
                        , writeDynamicPng
                        )

import Codec.Picture.Gif( decodeGif
                        , decodeGifImages
                        , encodeGifImage
                        , encodeGifImageWithPalette
                        , encodeGifImages

                        , writeGifImage
                        , writeGifImageWithPalette
                        , writeGifImages
                        )

import Codec.Picture.HDR( decodeHDR
                        , encodeHDR
                        , writeHDR
                        )
import Codec.Picture.Tiff( decodeTiff
                         , TiffSaveable
                         , encodeTiff
                         , writeTiff )
import Codec.Picture.Saving
import Codec.Picture.Types
import Codec.Picture.ColorQuant
-- import System.IO ( withFile, IOMode(ReadMode) )
#ifdef WITH_MMAP_BYTESTRING
import System.IO.MMap ( mmapFileByteString )
#endif

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

#include "ConvGraph.hs"

-- | Return the first Right thing, accumulating error
eitherLoad :: c -> [(String, c -> Either String b)] -> Either String b
eitherLoad v = inner ""
    where inner errAcc [] = Left $ "Cannot load file\n" ++ errAcc
          inner errAcc ((hdr, f) : rest) = case f v of
                Left  err  -> inner (errAcc ++ hdr ++ " " ++ err ++ "\n") rest
                Right rez  -> Right rez

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
          -- provide 1.1
          force x = x `deepseq` x

-- | Load an image file without even thinking about it, it does everything
-- as 'decodeImage'
readImage :: FilePath -> IO (Either String DynamicImage)
readImage = withImageDecoder decodeImage

-- | If you want to decode an image in a bytestring without even thinking
-- in term of format or whatever, this is the function to use. It will try
-- to decode in each known format and if one decoding succeed will return
-- the decoded image in it's own colorspace
decodeImage :: B.ByteString -> Either String DynamicImage
decodeImage str = eitherLoad str [("Jpeg", decodeJpeg)
                                 ,("PNG", decodePng)
                                 ,("Bitmap", decodeBitmap)
                                 ,("GIF", decodeGif)
                                 ,("HDR", decodeHDR)
                                 ,("Tiff", decodeTiff)
                                 ]

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
readGifImages :: FilePath -> IO (Either String [Image PixelRGB8])
readGifImages = withImageDecoder decodeGifImages

-- | Try to load a jpeg file and decompress. The colorspace is still
-- YCbCr if you want to perform computation on the luma part. You can
-- convert it to RGB using 'colorSpaceConversion'
readJpeg :: FilePath -> IO (Either String DynamicImage)
readJpeg = withImageDecoder decodeJpeg

-- | Try to load a .bmp file. The colorspace would be RGB or RGBA
readBitmap :: FilePath -> IO (Either String DynamicImage)
readBitmap = withImageDecoder decodeBitmap

-- | Try to load a .pic file. The colorspace can only be
-- RGB with floating point precision.
readHDR :: FilePath -> IO (Either String DynamicImage)
readHDR = withImageDecoder decodeHDR

-- | Save an image to a '.jpg' file, will do everything it can to save an image.
saveJpgImage :: Int -> FilePath -> DynamicImage -> IO ()
saveJpgImage quality path img = L.writeFile path $ imageToJpg quality img

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

