{-# LANGUAGE FlexibleContexts #-}
-- | Main module exporting import/export functions into various
-- image formats.
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

                     -- * Generic image writing
                     , saveBmpImage
                     , saveJpgImage 
                     , savePngImage 

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
                     , encodeDynamicPng
                     , writeDynamicPng

                     -- * Image types and pixel types
                     -- ** Image
                     , Image( .. )
                     , DynamicImage( .. )
                     -- ** Pixels
                     , Pixel( .. )
                     , Pixel8
                     , PixelYA8( .. )
                     , PixelRGB8( .. )
                     , PixelRGBA8( .. )
                     , PixelYCbCr8( .. )
                     ) where

import Control.Applicative( (<$>) )
import Control.DeepSeq( NFData, deepseq )
import qualified Control.Exception as Exc ( catch, IOException )
import Codec.Picture.Bitmap( BmpEncodable, decodeBitmap
                           , writeBitmap, encodeBitmap
                           , encodeDynamicBitmap, writeDynamicBitmap )
import Codec.Picture.Jpg( decodeJpeg, encodeJpeg, encodeJpegAtQuality )
import Codec.Picture.Png( PngSavable( .. ), decodePng, writePng
                        , encodeDynamicPng , writeDynamicPng )
import Codec.Picture.Gif( decodeGif, decodeGifImages )
import Codec.Picture.Saving
import Codec.Picture.Types
import System.IO ( withFile, IOMode(ReadMode) )


import qualified Data.ByteString as B

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
    where doit = withFile path ReadMode $ \h ->
                    force . decoder <$> B.hGetContents h
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
                                 ,("GIF", decodeGif)
                                 ,("Bitmap", decodeBitmap)
                                 ]
    
-- | Helper function trying to load a png file from a file on disk.
readPng :: FilePath -> IO (Either String DynamicImage)
readPng = withImageDecoder decodePng 

-- | Helper function trying to load a gif file from a file on disk.
readGif :: FilePath -> IO (Either String DynamicImage)
readGif = withImageDecoder decodeGif

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

-- | Save an image to a '.jpg' file, will do everything it can to save an image.
saveJpgImage :: Int -> String -> DynamicImage -> IO ()
saveJpgImage quality path img = B.writeFile path $ imageToJpg quality img

-- | Save an image to a '.png' file, will do everything it can to save an image.
-- For example, a simple transcoder to png
--
-- > transcodeToPng :: FilePath -> FilePath -> IO ()
-- > transcodeToPng pathIn pathOut = do
-- >    eitherImg <- decodeImage pathIn
-- >    case eitherImg of
-- >        Left _ -> return ()
-- >        Right img -> savePngImage img
--
savePngImage :: String -> DynamicImage -> IO ()
savePngImage path img = B.writeFile path $ imageToPng img

-- | Save an image to a '.bmp' file, will do everything it can to save an image.
saveBmpImage :: String -> DynamicImage -> IO ()
saveBmpImage path img = B.writeFile path $ imageToBitmap img

