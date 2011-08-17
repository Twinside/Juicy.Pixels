{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Module used to perform automatic 'upcasting' for pixel values,
-- it doesn't allow downcasting.
module Codec.Picture.ColorConversion( ColorConvertible(..) ) where

import Codec.Picture.Types
import Data.Array.Unboxed

class ColorConvertible a b where
    promotePixels :: Image a -> Image b

instance ColorConvertible Pixel2 Pixel8 where
    promotePixels = amap (\a -> if a then 255 else 0)

instance ColorConvertible Pixel2 PixelRGB8 where
    promotePixels = amap (\a -> if a then rgb 255 255 255
                                     else rgb   0   0   0)

instance ColorConvertible Pixel2 PixelRGBA8 where
    promotePixels = amap (\a -> if a then rgba 255 255 255 255
                                     else rgba   0   0   0 255)

instance ColorConvertible Pixel8 PixelRGB8 where
    promotePixels = amap (\c -> rgb c c c)

instance ColorConvertible Pixel8 PixelRGBA8 where
    promotePixels = amap (\c -> rgba c c c 255)

instance ColorConvertible PixelRGB8 PixelRGBA8 where
    promotePixels = amap (\(PixelRGB8 r g b)-> rgba r g b 255)

