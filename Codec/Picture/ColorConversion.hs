{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Module used to perform automatic 'upcasting' for pixel values,
-- it doesn't allow downcasting.
module Codec.Picture.ColorConversion( -- * Types
                                      PixelTypes 
                                      -- * Type classes
                                    , ColorConvertible( .. )
                                    , ColorConvertionQuery( .. )
                                      -- * Helper functions
                                    , canConvertTo
                                    ) where

import Codec.Picture.Types
import Data.Array.Unboxed

data PixelTypes = PixelMonochromatic
                | PixelGreyscale
                | PixelGreyscaleAlpha
                | PixelRedGreenBlue8
                | PixelRedGreenBlueAlpha8
                deriving Eq

class ColorConvertionQuery a where
    canPromoteTo :: a -> PixelTypes -> Bool
    promotionType :: a -> PixelTypes

canConvertTo :: (ColorConvertionQuery a, ColorConvertionQuery b)
             => a -> b -> Bool
canConvertTo a b = canPromoteTo a $ promotionType b

class (ColorConvertionQuery a) => ColorConvertible a b where
    promotePixels :: Image a -> Image b

-- | Free promotion for identic pixel types
instance (ColorConvertionQuery a) => ColorConvertible a a where
    promotePixels = id

--------------------------------------------------
----            Pixel2 instances
--------------------------------------------------
instance ColorConvertionQuery Pixel2 where
    canPromoteTo _ _ = True
    promotionType _ = PixelMonochromatic

instance ColorConvertible Pixel2 Pixel8 where
    promotePixels = amap (\a -> if a then 255 else 0)

instance ColorConvertible Pixel2 PixelYA8 where
    promotePixels = amap (\a -> if a then PixelYA8 255 255
                                     else PixelYA8   0 255)

instance ColorConvertible Pixel2 PixelRGB8 where
    promotePixels = amap (\a -> if a then rgb 255 255 255
                                     else rgb   0   0   0)

instance ColorConvertible Pixel2 PixelRGBA8 where
    promotePixels = amap (\a -> if a then rgba 255 255 255 255
                                     else rgba   0   0   0 255)

--------------------------------------------------
----            Pixel8 instances
--------------------------------------------------
instance ColorConvertionQuery Pixel8 where
    canPromoteTo _ a = a /= PixelMonochromatic 
    promotionType _ = PixelGreyscale

instance ColorConvertible Pixel8 PixelYA8 where
    promotePixels = amap (\c -> PixelYA8 c 255)
     
instance ColorConvertible Pixel8 PixelRGB8 where
    promotePixels = amap (\c -> rgb c c c)

instance ColorConvertible Pixel8 PixelRGBA8 where
    promotePixels = amap (\c -> rgba c c c 255)

--------------------------------------------------
----            PixelYA8 instances
--------------------------------------------------
instance ColorConvertionQuery PixelYA8 where
    canPromoteTo _ a = a == PixelRedGreenBlueAlpha8 
    promotionType _ = PixelGreyscaleAlpha

instance ColorConvertible PixelYA8 PixelRGBA8 where
    promotePixels = amap (\(PixelYA8 y a) -> rgba y y y a)

--------------------------------------------------
----            PixelRGB8 instances
--------------------------------------------------
instance ColorConvertionQuery PixelRGB8 where
    canPromoteTo _ PixelMonochromatic = False
    canPromoteTo _ PixelGreyscale = False
    canPromoteTo _ _ = True

    promotionType _ = PixelRedGreenBlue8

instance ColorConvertible PixelRGB8 PixelRGBA8 where
    promotePixels = amap (\(PixelRGB8 r g b)-> rgba r g b 255)

--------------------------------------------------
----            PixelRGBA8 instances
--------------------------------------------------
instance ColorConvertionQuery PixelRGBA8 where
    canPromoteTo _ PixelRedGreenBlueAlpha8 = True
    canPromoteTo _ _ = False

    promotionType _ = PixelRedGreenBlueAlpha8
