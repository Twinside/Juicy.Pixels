{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Module providing the basic types for image manipulation in the library.
-- Defining the types used to store all those _Juicy Pixels_
module Codec.Picture.Types( -- * Types
                            -- ** Image types
                            Image( .. )
                          , MutableImage( .. )
                          , DynamicImage( .. )
                          , PixelType( .. )
                            -- ** Pixel types
                          , Pixel8
                          , PixelYA8( .. )
                          , PixelRGB8( .. )
                          , PixelRGBA8( .. )
                          , PixelYCbCr8( .. )

                          -- * Type classes
                          , ColorConvertible( .. )
                          , Pixel(..)
                          , ColorSpaceConvertible( .. )
                            -- * Helper functions
                          , canConvertTo
                          ) where

import Control.Applicative( (<$>), (<*>) )
import Control.Monad.ST( ST, runST )
import Control.Monad.Primitive ( PrimMonad, PrimState )
import Foreign.Storable ( Storable, sizeOf, alignment, peek, poke )
import Foreign.Ptr ( plusPtr )
import Data.Word( Word8 )
import Data.Vector.Storable ( (!) )
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import Data.Serialize( Serialize, put, get )


-- | Image or pixel buffer, the coordinates are assumed to start
-- from the upper-left corner of the image, with the horizontal
-- position first, then the vertical one.
data Image a = Image
    { -- | Width of the image in pixels
      imageWidth  :: {-# UNPACK #-} !Int
      -- | Height of the image in pixels.
    , imageHeight :: {-# UNPACK #-} !Int

      -- | The real image, to extract pixels at some position
      -- you should use the helpers functions.
    , imageData   :: V.Vector Word8
    }

-- | Image or pixel buffer, the coordinates are assumed to start
-- from the upper-left corner of the image, with the horizontal
-- position first, then the vertical one. The image can be transformed in place.
data MutableImage s a = MutableImage 
    { -- | Width of the image in pixels
	  mutableImageWidth  :: {-# UNPACK #-} !Int

      -- | Height of the image in pixels.
	, mutableImageHeight :: {-# UNPACK #-} !Int

      -- | The real image, to extract pixels at some position
      -- you should use the helpers functions.
    , mutableImageData   :: M.STVector s Word8
    }

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
       -- | An image in the colorspace used by Jpeg images.
     | ImageYCbCr8 (Image PixelYCbCr8)

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
-- value are stored in the following order :
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

instance Storable PixelYA8 where
    {-# INLINE sizeOf #-}
    sizeOf _ = sizeOf (undefined :: Word8) * 2
    {-# INLINE alignment #-}
    alignment _ = alignment (undefined :: Word8)
    {-# INLINE peek #-}
    peek ptr = do
      let __   = undefined :: Word8
          yOff = sizeOf __ * 0
          aOff = sizeOf __ * 1
      y <- peek $ ptr `plusPtr` yOff
      a <- peek $ ptr `plusPtr` aOff
      return (PixelYA8 y a)
    {-# INLINE poke #-}
    poke ptr (PixelYA8 y a) = do
      let __   = undefined :: Word8
          yOff = sizeOf __ * 0
          aOff = sizeOf __ * 1
      poke (ptr `plusPtr` yOff) y
      poke (ptr `plusPtr` aOff) a

instance Serialize PixelRGB8 where
    {-# INLINE put #-}
    put (PixelRGB8 r g b) = put r >> put g >> put b
    {-# INLINE get #-}
    get = PixelRGB8 <$> get <*> get <*> get

instance Storable PixelRGB8 where
    {-# INLINE sizeOf #-}
    sizeOf _ = sizeOf (undefined :: Word8) * 3
    {-# INLINE alignment #-}
    alignment _ = alignment (undefined :: Word8)
    {-# INLINE peek #-}
    peek ptr = do
      let __   = undefined :: Word8
          rOff = sizeOf __ * 0
          gOff = sizeOf __ * 1
          bOff = sizeOf __ * 2
      r <- peek $ ptr `plusPtr` rOff
      g <- peek $ ptr `plusPtr` gOff
      b <- peek $ ptr `plusPtr` bOff
      return (PixelRGB8 r g b)
    {-# INLINE poke #-}
    poke ptr (PixelRGB8 r g b) = do
      let __   = undefined :: Word8
          rOff = sizeOf __ * 0
          gOff = sizeOf __ * 1
          bOff = sizeOf __ * 2
      poke (ptr `plusPtr` rOff) r
      poke (ptr `plusPtr` gOff) g
      poke (ptr `plusPtr` bOff) b

instance Serialize PixelYCbCr8 where
    {-# INLINE put #-}
    put (PixelYCbCr8 y cb cr) = put y >> put cb >> put cr
    {-# INLINE get #-}
    get = PixelYCbCr8 <$> get <*> get <*> get

instance Storable PixelYCbCr8 where
    {-# INLINE sizeOf #-}
    sizeOf _ = sizeOf (undefined :: Word8) * 3
    {-# INLINE alignment #-}
    alignment _ = alignment (undefined :: Word8)
    {-# INLINE peek #-}
    peek ptr = do
      let __   = undefined :: Word8
          yOff = sizeOf __ * 0
          cbOff = sizeOf __ * 1
          crOff = sizeOf __ * 2
      y  <- peek $ ptr `plusPtr` yOff
      cb <- peek $ ptr `plusPtr` cbOff
      cr <- peek $ ptr `plusPtr` crOff
      return (PixelYCbCr8 y cb cr)
    {-# INLINE poke #-}
    poke ptr (PixelYCbCr8 y cb cr) = do
      let __   = undefined :: Word8
          yOff  = sizeOf __ * 0
          cbOff = sizeOf __ * 1
          crOff = sizeOf __ * 2
      poke (ptr `plusPtr`  yOff) y
      poke (ptr `plusPtr` cbOff) cb
      poke (ptr `plusPtr` crOff) cr

instance Serialize PixelRGBA8 where
    {-# INLINE put #-}
    put (PixelRGBA8 r g b a) = put r >> put g >> put b >> put a
    {-# INLINE get #-}
    get = PixelRGBA8 <$> get <*> get <*> get <*> get

instance Storable PixelRGBA8 where
    {-# INLINE sizeOf #-}
    sizeOf _ = sizeOf (undefined :: Word8) * 4
    {-# INLINE alignment #-}
    alignment _ = alignment (undefined :: Word8)
    {-# INLINE peek #-}
    peek ptr = do
      let __   = undefined :: Word8
          rOff = sizeOf __ * 0
          gOff = sizeOf __ * 1
          bOff = sizeOf __ * 2
          aOff = sizeOf __ * 3
      r <- peek $ ptr `plusPtr` rOff
      g <- peek $ ptr `plusPtr` gOff
      b <- peek $ ptr `plusPtr` bOff
      a <- peek $ ptr `plusPtr` aOff
      return (PixelRGBA8 r g b a)
    {-# INLINE poke #-}
    poke ptr (PixelRGBA8 r g b a) = do
      let __   = undefined :: Word8
          rOff = sizeOf __ * 0
          gOff = sizeOf __ * 1
          bOff = sizeOf __ * 2
          aOff = sizeOf __ * 3
      poke (ptr `plusPtr` rOff) r
      poke (ptr `plusPtr` gOff) g
      poke (ptr `plusPtr` bOff) b
      poke (ptr `plusPtr` aOff) a

-- | Describe pixel kind at runtime
data PixelType = PixelMonochromatic         -- ^ For 2 bits pixels
               | PixelGreyscale
               | PixelGreyscaleAlpha
               | PixelRedGreenBlue8
               | PixelRedGreenBlueAlpha8
               | PixelYChromaRChromaB8
               deriving Eq

-- | Typeclass used to query a type about it's properties
-- regarding casting to other pixel types
class (Serialize a) => Pixel a where
    -- | Tell if a pixel can be converted to another pixel,
    -- the first value should not be used, and 'undefined' can
    -- be used as a valid value.
    canPromoteTo :: a -> PixelType -> Bool

    -- | Return the number of component of the pixel
    componentCount :: a -> Int

    -- | Calculate the index for the begining of the pixel
    pixelBaseIndex :: Image a -> Int -> Int -> Int
    pixelBaseIndex (Image { imageWidth = w }) x y =
            (x + y * w) * componentCount (undefined :: a)

    -- | Calculate theindex for the begining of the pixel at position x y
    mutablePixelBaseIndex :: MutableImage s a -> Int -> Int -> Int
    mutablePixelBaseIndex (MutableImage { mutableImageWidth = w }) x y =
            (x + y * w) * componentCount (undefined :: a)

    -- | Return the constructor associated to the type, again
    -- the value in the first parameter is not used, so you can use undefined
    promotionType :: a -> PixelType

    -- | Extract a pixel at a given position, (x, y), the origin
    -- is assumed to be at the corner top left, positive y to the
    -- bottom of the image
    pixelAt :: Image a -> Int -> Int -> a

    -- | Same as pixelAt but for mutable images.
    readPixel :: MutableImage s a -> Int -> Int -> ST s a

    -- | Write a pixel in a mutable image at position x y
    writePixel :: MutableImage s a -> Int -> Int -> a -> ST s ()

-- | Tell if you can convert between two pixel types, both arguments
-- are unused.
canConvertTo :: (Pixel a, Pixel b) => a -> b -> Bool
canConvertTo a b = canPromoteTo a $ promotionType b

-- | Implement upcasting for pixel types
-- Minimal declaration declaration `promotePixel`
-- It is strongly recommanded to overload promoteImage to keep
-- performance acceptable
class (Pixel a, Pixel b) => ColorConvertible a b where
    -- | Convert a pixel type to another pixel type. This
    -- operation should never loss any data.
    promotePixel :: a -> b

    -- | Change the underlying pixel type of an image by performing a full copy
    -- of it.
    promoteImage :: Image a -> Image b
    promoteImage image@(Image { imageWidth = w, imageHeight = h }) =
        Image w h pixels
         where pixels = runST $ do
                    newArr <- M.replicate (w * h * componentCount (undefined :: b)) 0
                    let wrapped = MutableImage w h newArr
                        promotedPixel :: Int -> Int -> b
                        promotedPixel x y = promotePixel $ pixelAt image x y
                    sequence_ [writePixel wrapped x y $ promotedPixel x y
                                        | y <- [0 .. h - 1], x <- [0 .. w - 1] ]
                    -- unsafeFreeze avoids making a second copy and it will be
                    -- safe because newArray can't be referenced as a mutable array
                    -- outside of this where block
                    V.unsafeFreeze newArr

-- | This class abstract colorspace conversion. This
-- conversion can be lossy, which ColorConvertible cannot
class (Pixel a, Pixel b) => ColorSpaceConvertible a b where
    convertPixel :: a -> b

    convertImage :: Image a -> Image b
    convertImage image@(Image { imageWidth = w, imageHeight = h }) =
        Image w h pixels
         where pixels = runST $ do
                    newArr <- M.replicate (w * h * componentCount (undefined :: b)) 0
                    let wrapped = MutableImage w h newArr
                        promotedPixel :: Int -> Int -> b
                        promotedPixel x y = convertPixel $ pixelAt image x y
                    sequence_ [writePixel wrapped x y $ promotedPixel x y
                                        | y <- [0 .. h - 1], x <- [0 .. w - 1] ]
                    -- unsafeFreeze avoids making a second copy and it will be
                    -- safe because newArray can't be referenced as a mutable array
                    -- outside of this where block
                    V.unsafeFreeze newArr

-- | Free promotion for identic pixel types
instance (Pixel a) => ColorConvertible a a where
    {-# INLINE promotePixel #-}
    promotePixel = id

    {-# INLINE promoteImage #-}
    promoteImage = id

{-# INLINE (.!!!.) #-}
(.!!!.) :: (PrimMonad m, Storable a) => M.STVector (PrimState m) a -> Int -> m a
(.!!!.) = M.read -- unsafeRead

{-# INLINE (.<-.) #-}
(.<-.) :: (PrimMonad m, Storable a) => M.STVector (PrimState m) a -> Int -> a -> m ()
(.<-.)  = M.write -- unsafeWrite

--------------------------------------------------
----            Pixel8 instances
--------------------------------------------------
instance Pixel Pixel8 where
    canPromoteTo _ a = a /= PixelMonochromatic 
    promotionType _ = PixelGreyscale
    componentCount _ = 1
    pixelAt (Image { imageWidth = w, imageData = arr }) x y = arr ! (x + y * w)

    readPixel image@(MutableImage { mutableImageData = arr }) x y =
        arr .!!!. mutablePixelBaseIndex image x y
    
    writePixel image@(MutableImage { mutableImageData = arr }) x y =
        arr .<-. mutablePixelBaseIndex image x y

instance ColorConvertible Pixel8 PixelYA8 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelYA8 c 255

instance ColorConvertible Pixel8 PixelRGB8 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelRGB8 c c c

instance ColorConvertible Pixel8 PixelRGBA8 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelRGBA8 c c c 255

--------------------------------------------------
----            PixelYA8 instances
--------------------------------------------------
instance Pixel PixelYA8 where
    canPromoteTo _ a = a == PixelRedGreenBlueAlpha8 
    promotionType _  = PixelGreyscaleAlpha
    componentCount _ = 2
    pixelAt image@(Image { imageData = arr }) x y = PixelYA8 (arr ! (baseIdx + 0))
                                                             (arr ! (baseIdx + 1))
        where baseIdx = pixelBaseIndex image x y

    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        yv <- arr .!!!. baseIdx
        av <- arr .!!!. (baseIdx + 1)
        return $ PixelYA8 yv av
        where baseIdx = mutablePixelBaseIndex image x y
    
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelYA8 yv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr .<-. (baseIdx + 0)) yv
        (arr .<-. (baseIdx + 1)) av


instance ColorConvertible PixelYA8 PixelRGB8 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelYA8 y _) = PixelRGB8 y y y

instance ColorConvertible PixelYA8 PixelRGBA8 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelYA8 y a) = PixelRGBA8 y y y a

--------------------------------------------------
----            PixelRGB8 instances
--------------------------------------------------
instance Pixel PixelRGB8 where
    canPromoteTo _ PixelMonochromatic = False
    canPromoteTo _ PixelGreyscale = False
    canPromoteTo _ _ = True

    componentCount _ = 3

    promotionType _ = PixelRedGreenBlue8

    pixelAt image@(Image { imageData = arr }) x y = PixelRGB8 (arr ! (baseIdx + 0))
                                                              (arr ! (baseIdx + 1))
                                                              (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr .!!!. baseIdx
        gv <- arr .!!!. (baseIdx + 1)
        bv <- arr .!!!. (baseIdx + 2)
        return $ PixelRGB8 rv gv bv
        where baseIdx = mutablePixelBaseIndex image x y
    
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGB8 rv gv bv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr .<-. (baseIdx + 0)) rv
        (arr .<-. (baseIdx + 1)) gv
        (arr .<-. (baseIdx + 2)) bv

instance ColorConvertible PixelRGB8 PixelRGBA8 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelRGB8 r g b) = PixelRGBA8 r g b 255

--------------------------------------------------
----            PixelRGBA8 instances
--------------------------------------------------
instance Pixel PixelRGBA8 where
    canPromoteTo _ PixelRedGreenBlueAlpha8 = True
    canPromoteTo _ _ = False

    promotionType _ = PixelRedGreenBlueAlpha8

    componentCount _ = 4

    pixelAt image@(Image { imageData = arr }) x y = PixelRGBA8 (arr ! (baseIdx + 0))
                                                               (arr ! (baseIdx + 1))
                                                               (arr ! (baseIdx + 2))
                                                               (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr .!!!. baseIdx
        gv <- arr .!!!. (baseIdx + 1)
        bv <- arr .!!!. (baseIdx + 2)
        av <- arr .!!!. (baseIdx + 3)
        return $ PixelRGBA8 rv gv bv av
        where baseIdx = mutablePixelBaseIndex image x y
    
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGBA8 rv gv bv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr .<-. (baseIdx + 0)) rv
        (arr .<-. (baseIdx + 1)) gv
        (arr .<-. (baseIdx + 2)) bv
        (arr .<-. (baseIdx + 3)) av

--------------------------------------------------
----            PixelYCbCr8 instances
--------------------------------------------------
instance Pixel PixelYCbCr8 where
    canPromoteTo _ _ = False
    promotionType _ = PixelYChromaRChromaB8
    componentCount _ = 3
    pixelAt image@(Image { imageData = arr }) x y = PixelYCbCr8 (arr ! (baseIdx + 0))
                                                                (arr ! (baseIdx + 1))
                                                                (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        yv <- arr .!!!. baseIdx
        cbv <- arr .!!!. (baseIdx + 1)
        crv <- arr .!!!. (baseIdx + 2)
        return $ PixelYCbCr8 yv cbv crv
        where baseIdx = mutablePixelBaseIndex image x y
    
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelYCbCr8 yv cbv crv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr .<-. (baseIdx + 0)) yv
        (arr .<-. (baseIdx + 1)) cbv
        (arr .<-. (baseIdx + 2)) crv

instance ColorSpaceConvertible PixelYCbCr8 PixelRGB8 where
    {-# INLINE convertPixel #-}
    convertPixel (PixelYCbCr8 y_w8 cb_w8 cr_w8) = PixelRGB8 (clampWord8 r) (clampWord8 g) (clampWord8 b)
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

