{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Module used to perform automatic 'upcasting' for pixel values,
-- it doesn't allow downcasting, to avoid any data loss
module Codec.Picture.ColorConversion( -- * Type classes
                                      ColorConvertible( .. )
                                    , ColorConvertionQuery
                                    , ColorSpaceConvertible( .. )
                                      -- * Helper functions
                                    , canConvertTo
                                    , promotePixels
                                    , changeImageColorSpace
                                    ) where

import Codec.Picture.Types
import Data.Array.Unboxed

data PixelTypes = PixelMonochromatic
                | PixelGreyscale
                | PixelGreyscaleAlpha
                | PixelRedGreenBlue8
                | PixelRedGreenBlueAlpha8
                | PixelYChromaRChromaB8
                deriving Eq

-- | Typeclass used to query a type about it's properties
-- regarding casting to other pixel types
class ColorConvertionQuery a where
    -- | Tell if a pixel can be converted to another pixel,
    -- the first value should not be used, and 'undefined' can
    -- be used as a valid value.
    canPromoteTo :: a -> PixelTypes -> Bool

    -- | Return the constructor associated to the type, again
    -- the value in the first parameter is not used, so you can use undefined
    promotionType :: a -> PixelTypes

-- | Tell if you can convert between two pixel types, both arguments
-- are unused.
canConvertTo :: (ColorConvertionQuery a, ColorConvertionQuery b)
             => a -> b -> Bool
canConvertTo a b = canPromoteTo a $ promotionType b

-- | Implement upcasting for pixel types
-- Minimal declaration declaration `promotePixel`
class (ColorConvertionQuery a) => ColorConvertible a b where
    -- | Convert a pixel type to another pixel type. This
    -- operation should never loss any data.
    promotePixel  :: a -> b

    {-# INLINE fromRawData #-}
    -- | Given a list of raw elements, convert them to a new type.
    -- Usefull to unserialize some elements from more basic types.
    fromRawData   :: [a] -> (Maybe b, [a])
    fromRawData [] = (Nothing, [])
    fromRawData (x:xs) = (Just $ promotePixel x, xs)

-- | This class abstract colorspace conversion. This
-- conversion can be lossy, which ColorConvertible cannot
class ColorSpaceConvertible a b where
    colorSpaceConversion :: a -> b

{-# INLINE changeImageColorSpace #-}
-- | Convert an image between different colorspace, this operation can result
-- in a loss of precision.
changeImageColorSpace :: (IArray UArray a, IArray UArray b, ColorSpaceConvertible a b)
                      => Image a -> Image b
changeImageColorSpace = amap colorSpaceConversion

{-# INLINE promotePixels #-}
-- | Convert a whole image to a new pixel type.
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
    promotePixel a | a = PixelRGB8 255 255 255
                   | otherwise = PixelRGB8   0   0   0

instance ColorConvertible Pixel2 PixelRGBA8 where
    {-# INLINE promotePixel #-}
    promotePixel a | a = PixelRGBA8 255 255 255 255
                   | otherwise = PixelRGBA8   0   0   0 255

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
    promotePixel c = PixelRGB8 c c c

    {-# INLINE fromRawData #-}
    fromRawData (r:g:b:xs) = (Just $ PixelRGB8 r g b, xs)
    fromRawData _ = (Nothing, [])

instance ColorConvertible Pixel8 PixelRGBA8 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelRGBA8 c c c 255

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
    promotePixel (PixelYA8 y _) = PixelRGB8 y y y

instance ColorConvertible PixelYA8 PixelRGBA8 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelYA8 y a) = PixelRGBA8 y y y a

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
    promotePixel (PixelRGB8 r g b) = PixelRGBA8 r g b 255

--------------------------------------------------
----            PixelRGBA8 instances
--------------------------------------------------
instance ColorConvertionQuery PixelRGBA8 where
    canPromoteTo _ PixelRedGreenBlueAlpha8 = True
    canPromoteTo _ _ = False

    promotionType _ = PixelRedGreenBlueAlpha8

--------------------------------------------------
----            PixelYCbCr8 instances
--------------------------------------------------
instance ColorConvertionQuery PixelYCbCr8 where
    canPromoteTo _ _ = False
    promotionType _ = PixelYChromaRChromaB8

instance ColorSpaceConvertible PixelYCbCr8 PixelRGB8 where
    {-# INLINE colorSpaceConversion #-}
    colorSpaceConversion (PixelYCbCr8 y_w8 cb_w8 cr_w8) = PixelRGB8 (clampWord8 r) (clampWord8 g) (clampWord8 b)
        where y :: Float
              y  = fromIntegral y_w8 - 128.0
              cb = fromIntegral cb_w8 - 128.0
              cr = fromIntegral cr_w8 - 128.0

              clampWord8 = truncate . max 0.0 . min 255.0 . (128 +)

              cred = 0.299
              cgreen = 0.587
              cblue = 0.114

              r = cr * (2 - 2 * cred) + y
              b = cb * (2 - 2 * cblue) + y
              g = (y - cblue * b - cred * r) / cgreen

