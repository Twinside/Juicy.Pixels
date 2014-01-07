module Codec.Picture.ColorQuant where

import           Codec.Picture.Types
import           Data.Bits                         ((.&.))
import           Data.Word                         (Word8)

-- | A naive one pass Color Quantiation algorithm. Simply take the 3 most
--   significant bits of red and green. Take the 2 most significant bits of
--   blue.
onePassCQ :: Image PixelRGB8 -> Image PixelRGB8
onePassCQ = pixelMap maskFunction
  where maskFunction (PixelRGB8 r g b) =
          PixelRGB8 (r .&. 224)
                    (g .&. 224)
                    (b .&. 192)

-- The *a dither* algorithm of Øyvind Kolås, pippin@gimp.org in 2013.
-- See, http://pippin.gimp.org/a_dither/.

type Mask = Int -> Int -> Int -> Double

-- No dithering.
mask0 :: Mask
mask0 _ _ _ = 0

-- Pattern 1.
mask1 :: Mask
mask1 _ x y = fromIntegral m / 511
  where
    m = (149 * 1234 * x ^ y) .&. 511

-- Pattern 2.
mask2 :: Mask
mask2 c x y = fromIntegral m / 511
  where
    m = (149 * 1234 * ((17 * c + x) ^ y)) .&. 511

-- Pattern 3.
mask3 :: Mask
mask3 _ x y = fromIntegral m / 255
  where
    m = (119 * (x + 237 * y)) .&. 255

-- \ Pattern 4.
mask4 :: Mask
mask4 c x y = fromIntegral m / 255
  where
    m = (119 * (x + (67 * c) + (236 * y))) .&. 255

-- Pattern 5.
mask5 :: Mask
mask5 _ _ _ = 0.5

masks :: [Mask]
masks = [mask0, mask1, mask2, mask3, mask4, mask5]

-- Apply a mask to a pixel's color component c, level l and coordinates x,y.
appMask :: Word8 -> Int -> Double -> Int -> Int -> Mask -> Word8
appMask v c l x y f = fromIntegral $ floor (z * 255)
  where
    z = (l * fromIntegral v / 255 + f c x y) / l

-- Dither an image using "a dither" with mask f and level l.
aDither :: Mask -> Double -> Image PixelRGB8 -> Image PixelRGB8
aDither f l = pixelMapXY maskFunction
  where
    maskFunction x y (PixelRGB8 r g b) =
      PixelRGB8 (appMask r 0 l x y f)
                (appMask g 1 l x y f)
                (appMask b 2 l x y f)