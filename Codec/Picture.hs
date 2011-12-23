
-- | Main module exporting import/export functions into various
-- image formats.
module Codec.Picture ( -- * Bitmap handling 
                       BmpEncodable()
                     , writeBitmapFile
                     , encodeBitmapFile
                     , decodeBitmapImage 

                     -- * Jpeg handling
                     , loadJpeg
                     , decodeJpeg 

                     -- * Png handling
                     , PngLoadable( .. )
                     , PngSavable( .. )
                     , loadPng
                     , pngDecode
                     , writePngFile
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

import Codec.Picture.Bitmap
import Codec.Picture.Jpg
import Codec.Picture.Png
import Codec.Picture.Types

