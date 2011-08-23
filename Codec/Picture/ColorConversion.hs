{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Module used to perform automatic 'upcasting' for pixel values,
-- it doesn't allow downcasting.
module Codec.Picture.ColorConversion( -- * Types
                                      PixelTypes 
                                      -- * Type classes
                                    , ColorConvertible( .. )
                                    , ColorConvertionQuery( .. )
                                      -- * Helper functions
                                    , canConvertTo
                                    , promotePixels
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

-- | Minimal declaration declaration `promotePixel`
class (ColorConvertionQuery a) => ColorConvertible a b where
    -- | Convert a pixel type to another pixel type.
    promotePixel  :: a -> b

    {-# INLINE fromRawData #-}
    -- | Given a list of raw elements, convert them to a new type
    fromRawData   :: [a] -> (Maybe b, [a])
    fromRawData [] = (Nothing, [])
    fromRawData (x:xs) = (Just $ promotePixel x, xs)

{-# INLINE promotePixels #-}
promotePixels :: (IArray UArray a, IArray UArray b, ColorConvertible a b) 
              => Image a -> Image b
promotePixels = amap promotePixel

-- | Free promotion for identic pixel types
instance (ColorConvertionQuery a) => ColorConvertible a a where
    {-# INLINE promotePixel #-}
    promotePixel = id

--------------------------------------------------
----            Pixel2 instances
--------------------------------------------------
instance ColorConvertionQuery Pixel2 where
    canPromoteTo _ _ = True
    promotionType _ = PixelMonochromatic

instance ColorConvertible Pixel2 Pixel8 where
    {-# INLINE promotePixel #-}
    promotePixel a | a = 255
                   | otherwise = 0

instance ColorConvertible Pixel2 PixelYA8 where
    {-# INLINE promotePixel #-}
    promotePixel a | a = PixelYA8 255 255
                   | otherwise = PixelYA8 0 255

instance ColorConvertible Pixel2 PixelRGB8 where
    {-# INLINE promotePixel #-}
    promotePixel a | a = rgb 255 255 255
                   | otherwise = rgb   0   0   0

instance ColorConvertible Pixel2 PixelRGBA8 where
    {-# INLINE promotePixel #-}
    promotePixel a | a = rgba 255 255 255 255
                   | otherwise = rgba   0   0   0 255

--------------------------------------------------
----            Pixel8 instances
--------------------------------------------------
instance ColorConvertionQuery Pixel8 where
    canPromoteTo _ a = a /= PixelMonochromatic 
    promotionType _ = PixelGreyscale

instance ColorConvertible Pixel8 PixelYA8 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelYA8 c 255

    {-# INLINE fromRawData #-}
    fromRawData (y:a:xs) = (Just $ PixelYA8 y a, xs)
    fromRawData _ = (Nothing, [])
     
instance ColorConvertible Pixel8 PixelRGB8 where
    {-# INLINE promotePixel #-}
    promotePixel c = rgb c c c

    {-# INLINE fromRawData #-}
    fromRawData (r:g:b:xs) = (Just $ PixelRGB8 r g b, xs)
    fromRawData _ = (Nothing, [])

instance ColorConvertible Pixel8 PixelRGBA8 where
    {-# INLINE promotePixel #-}
    promotePixel c = rgba c c c 255

    {-# INLINE fromRawData #-}
    fromRawData (r:g:b:a:xs) = (Just $ PixelRGBA8 r g b a, xs)
    fromRawData _ = (Nothing, [])

--------------------------------------------------
----            PixelYA8 instances
--------------------------------------------------
instance ColorConvertionQuery PixelYA8 where
    canPromoteTo _ a = a == PixelRedGreenBlueAlpha8 
    promotionType _ = PixelGreyscaleAlpha

instance ColorConvertible PixelYA8 PixelRGB8 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelYA8 y _) = rgb y y y

instance ColorConvertible PixelYA8 PixelRGBA8 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelYA8 y a) = rgba y y y a

--------------------------------------------------
----            PixelRGB8 instances
--------------------------------------------------
instance ColorConvertionQuery PixelRGB8 where
    canPromoteTo _ PixelMonochromatic = False
    canPromoteTo _ PixelGreyscale = False
    canPromoteTo _ _ = True

    promotionType _ = PixelRedGreenBlue8

instance ColorConvertible PixelRGB8 PixelRGBA8 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelRGB8 r g b) = rgba r g b 255

--------------------------------------------------
----            PixelRGBA8 instances
--------------------------------------------------
instance ColorConvertionQuery PixelRGBA8 where
    canPromoteTo _ PixelRedGreenBlueAlpha8 = True
    canPromoteTo _ _ = False

    promotionType _ = PixelRedGreenBlueAlpha8

