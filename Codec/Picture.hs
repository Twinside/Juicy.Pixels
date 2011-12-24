{-# LANGUAGE FlexibleContexts #-}
-- | Main module exporting import/export functions into various
-- image formats.
--
-- To use the library without thinking about it, look after 'decodeImage' and
-- 'readImage'.
--
-- Generally, the read* functions read the images from a file and try to decode
-- it, and the decode* functions try to decode a bytestring.
module Codec.Picture ( 
                     -- * Generic function
                       readImage
                     , decodeImage
                     , generateImage
                     -- * Specific image format functions
                     -- ** Bitmap handling 
                     , BmpEncodable
                     , writeBitmap
                     , encodeBitmap
                     , decodeBitmap

                     -- ** Jpeg handling
                     , readJpeg
                     , decodeJpeg 

                     -- ** Png handling
                     , PngLoadable( .. )
                     , PngSavable( .. )
                     , readPng
                     , pngDecode
                     , writePng
                     -- * Image types and pixel types
                     -- ** Image
                     , Image
                     , DynamicImage( .. )
                     -- ** Pixels
                     , Pixel2
                     , Pixel8
                     , PixelYA8( .. )
                     , PixelRGB8( .. )
                     , PixelRGBA8( .. )
                     , PixelYCbCr8( .. )
                     ) where

import Data.Array.Unboxed
import Data.Word

import Control.Applicative
import Codec.Picture.Bitmap
import Codec.Picture.Jpg
import Codec.Picture.Png
import Codec.Picture.Types

import qualified Data.ByteString as B

-- | Return the first Right thing, accumulating error
eitherLoad :: c -> [(String, c -> Either String b)] -> Either String b
eitherLoad v = inner ""
    where inner errAcc [] = Left $ "Cannot load file\n" ++ errAcc
          inner errAcc ((hdr, f) : rest) = case f v of
                Left  err  -> inner (errAcc ++ hdr ++ " " ++ err ++ "\n") rest
                Right rez  -> Right rez

-- | Load an image file without even thinking about it, it does everything
-- as 'decodeImage'
readImage :: FilePath -> IO (Either String DynamicImage)
readImage path = decodeImage <$> B.readFile path

-- | If you want to decode an image in a bytestring without even thinking
-- in term of format or whatever, this is the function to use. It will try
-- to decode in each known format and if one decoding succeed will return
-- the decoded image in it's own colorspace
decodeImage :: B.ByteString -> Either String DynamicImage
decodeImage str = eitherLoad str [("Jpeg", \b -> ImageYCbCr <$> decodeJpeg b)
                                 ,("PNG", pngDecode)
                                 ]
    

-- | Little helper function to generate an image in a \'shader\' like fashion,
-- take a function, an image size and generate the image with the function.
generateImage :: (IArray UArray a) 
              => (Word32 -> Word32 -> a) -- ^ Image generating function, taking x and y parameter
              -> Word32                  -- ^ Image width in pixels
              -> Word32                  -- ^ Image Height in pixels
              -> Image a
generateImage f imageWidth imageHeight =
  listArray ((0, 0), (imageWidth - 1, imageHeight - 1)) pixels
    where pixels = [f x y | y <- [0 .. imageHeight - 1], x <- [0 .. imageWidth - 1]]

