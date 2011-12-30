{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnboxedTuples #-}
-- | Module providing the basic types for image manipulation in the library.
-- Defining the types used to store all those _Juicy Pixels_
module Codec.Picture.Types( -- * Types
                            -- ** Image types
                            Image( .. )
                          , MutableImage
                          , DynamicImage( .. )
                            -- ** Pixel types
                          , Pixel2
                          , Pixel8
                          , PixelYA8( .. )
                          , PixelRGB8( .. )
                          , PixelRGBA8( .. )
                          , PixelYCbCr8( .. )
                            -- * Helper functions
                          , swapBlueRed 
                          , unsafeImageCast
                          ) where

import Control.Applicative
import Data.Word
import Data.Array.Unboxed
import Data.Array.Base
import Data.Serialize

-- | Image or pixel buffer, the coordinates are assumed to start
-- from the upper-left corner of the image, with the horizontal
-- position first, then the vertical one.
data Image a = Image
    { -- | Width of the image in pixels
      imageWidth  :: Int
      -- | Height of the image in pixels.
    , imageHeight :: Int

      -- | The real image, to extract pixels at some position
      -- you should use the helpers functions.
    , imageData   :: UArray Int Word8
    }

type MutableImage s a = STUArray s Int Word8

-- | Type allowing the loading of an image with different pixel
-- structures
data DynamicImage =
       -- | A greyscale image.
       ImageY8   (Image Pixel8)
       -- | An image in greyscale with an alpha channel.
     | ImageYA8  (Image PixelYA8)
       -- | An image in true color.
     | ImageRGB8 (Image PixelRGB8)
       -- | An image in true color and an alpha channel.
     | ImageRGBA8 (Image PixelRGBA8)

     | ImageYCbCr (Image PixelYCbCr8)

type Pixel2 = Bool

-- | Simple alias for greyscale value in 8 bits.
type Pixel8 = Word8

-- | Pixel type storing Luminance (Y) and alpha information
-- on 8 bits.
-- Value are stored in the following order :
--
--  * Luminance
--
--  * Alpha
--
data PixelYA8 = PixelYA8 {-# UNPACK #-} !Word8  -- Luminance
                         {-# UNPACK #-} !Word8  -- Alpha value

-- | Pixel type storing classic pixel on 8 bits
-- Value are stored in the following order :
--
--  * Red
--
--  * Green
--
--  * Blue
--
data PixelRGB8 = PixelRGB8 {-# UNPACK #-} !Word8 -- Red
                           {-# UNPACK #-} !Word8 -- Green
                           {-# UNPACK #-} !Word8 -- Blue

-- | Pixel storing data in the YCbCr colorspace,
-- value are stored in teh following order :
--
--  * Y (luminance)
--
--  * Cr
--
--  * Cb
--
data PixelYCbCr8 = PixelYCbCr8 {-# UNPACK #-} !Word8 -- Y luminance
                               {-# UNPACK #-} !Word8 -- Cr red difference
                               {-# UNPACK #-} !Word8 -- Cb blue difference

-- | Pixel type storing a classic pixel, with an alpha component.
-- Values are stored in the following order
--
--  * Red
--
--  * Green
--
--  * Blue
--
-- * Alpha
data PixelRGBA8 = PixelRGBA8 {-# UNPACK #-} !Word8 -- Red
                             {-# UNPACK #-} !Word8 -- Green
                             {-# UNPACK #-} !Word8 -- Blue
                             {-# UNPACK #-} !Word8 -- Alpha

instance Serialize PixelYA8 where
    {-# INLINE put #-}
    put (PixelYA8 y a) = put y >> put a
    {-# INLINE get #-}
    get = PixelYA8 <$> get <*> get

instance Serialize PixelRGB8 where
    {-# INLINE put #-}
    put (PixelRGB8 r g b) = put r >> put g >> put b
    {-# INLINE get #-}
    get = PixelRGB8 <$> get <*> get <*> get

instance Serialize PixelYCbCr8 where
    {-# INLINE put #-}
    put (PixelYCbCr8 y cb cr) = put y >> put cb >> put cr
    {-# INLINE get #-}
    get = PixelYCbCr8 <$> get <*> get <*> get

-- | Helper function to let put color in the "windows" order
-- used in the Bitmap file format.
{-# INLINE swapBlueRed #-}
swapBlueRed :: PixelRGBA8 -> PixelRGBA8
swapBlueRed (PixelRGBA8 r g b a) = PixelRGBA8 b g r a

instance Serialize PixelRGBA8 where
    {-# INLINE put #-}
    put (PixelRGBA8 r g b a) = put b >> put g >> put r >> put a
    {-# INLINE get #-}
    get = PixelRGBA8 <$> get <*> get <*> get <*> get

unsafeImageCast :: Image a -> Image b
unsafeImageCast (Image { imageWidth = w, imageHeight = h, imageData = d }) =
    Image { imageWidth = w, imageHeight = h, imageData = d }

