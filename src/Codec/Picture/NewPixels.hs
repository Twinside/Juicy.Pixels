{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE UndecidableInstances #-}
-- | Module providing the basic types for image manipulation in the library.
-- Defining the types used to store all those _Juicy Pixels_
module Codec.Picture.NewPixels
                          ( -- * Types
                            YA( .. )
                          , RGB( .. )
                          , RGBA( .. )
                          , CMYK( .. )
                          , YCbCr( .. )
                          , YCbCrK( .. )
                          , Palette
                          , DynamicImage( .. )

                          , dynamicMap 
                          , dynamicPixelMap
                          , gammaCorrection
                          , toneMapping
                          ) where

import Control.Monad( liftM, ap )
import Control.DeepSeq( NFData( .. ) )
import Control.Monad.ST( runST )
import Foreign.Storable ( Storable )
import Data.Bits( unsafeShiftL, unsafeShiftR, (.|.), (.&.) )
import Data.Word( Word8, Word16, Word32, Word64 )
import Data.Vector.Storable ( (!) )
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

import Codec.Picture.BaseTypes

-- | Type for the palette used in Gif & PNG files.
type Palette = Image (RGB Pixel8)


instance (Pixel a, Storable a) => TransparentPixel (RGBA a) (RGB a) where
    {-# INLINE dropTransparency #-}
    dropTransparency (RGBA r g b _) = RGB r g b
    setOpacity a (RGBA r g b _) = RGBA r g b a

-- | Image type enumerating all predefined pixel types.
-- It enables loading and use of images of different
-- pixel types.
data DynamicImage =
       -- | A greyscale image.
       ImageY8    (Image Pixel8)
       -- | A greyscale image with 16bit components
     | ImageY16   (Image Pixel16)
       -- | A greyscale HDR image
     | ImageYF    (Image PixelF)
       -- | An image in greyscale with an alpha channel.
     | ImageYA8   (Image (YA Pixel8))
      -- | An image in greyscale with alpha channel on 16 bits.
     | ImageYA16  (Image (YA Pixel16))
       -- | An image in true color.
     | ImageRGB8  (Image (RGB Pixel8))
       -- | An image in true color with 16bit depth.
     | ImageRGB16 (Image (RGB Pixel16))
       -- | An image with HDR pixels
     | ImageRGBF  (Image (RGB PixelF))
       -- | An image in true color and an alpha channel.
     | ImageRGBA8 (Image (RGBA Pixel8))
       -- | A true color image with alpha on 16 bits.
     | ImageRGBA16 (Image (RGBA Pixel16))
       -- | An image in the colorspace used by Jpeg images.
     | ImageYCbCr8 (Image (YCbCr Pixel8))
       -- | An image in the colorspace CMYK
     | ImageCMYK8  (Image (CMYK Pixel8))
       -- | An image in the colorspace CMYK and 16 bits precision
     | ImageCMYK16 (Image (CMYK Pixel16))

-- | Helper function to help extract information from dynamic
-- image. To get the width of a dynamic image, you can use
-- the following snippet:
--
-- > dynWidth :: DynamicImage -> Int
-- > dynWidth img = dynamicMap imageWidth img
--
dynamicMap :: (forall pixel . (Pixel pixel) => Image pixel -> a)
           -> DynamicImage -> a
dynamicMap f (ImageY8    i) = f i
dynamicMap f (ImageY16   i) = f i
dynamicMap f (ImageYF    i) = f i
dynamicMap f (ImageYA8   i) = f i
dynamicMap f (ImageYA16  i) = f i
dynamicMap f (ImageRGB8  i) = f i
dynamicMap f (ImageRGB16 i) = f i
dynamicMap f (ImageRGBF  i) = f i
dynamicMap f (ImageRGBA8 i) = f i
dynamicMap f (ImageRGBA16 i) = f i
dynamicMap f (ImageYCbCr8 i) = f i
dynamicMap f (ImageCMYK8 i) = f i
dynamicMap f (ImageCMYK16 i) = f i

-- | Equivalent of the `pixelMap` function for the dynamic images.
-- You can perform pixel colorspace independant operations with this
-- function.
--
-- For instance, if you want to extract a square crop of any image,
-- without caring about colorspace, you can use the following snippet.
--
-- > dynSquare :: DynamicImage -> DynamicImage
-- > dynSquare = dynamicPixelMap squareImage
-- >
-- > squareImage :: Pixel a => Image a -> Image a
-- > squareImage img = generateImage (\x y -> pixelAt img x y) edge edge
-- >    where edge = min (imageWidth img) (imageHeight img)
--
dynamicPixelMap :: (forall pixel . (Pixel pixel) => Image pixel -> Image pixel)
                -> DynamicImage -> DynamicImage
dynamicPixelMap f = aux
  where
    aux (ImageY8    i) = ImageY8 (f i)
    aux (ImageY16   i) = ImageY16 (f i)
    aux (ImageYF    i) = ImageYF (f i)
    aux (ImageYA8   i) = ImageYA8 (f i)
    aux (ImageYA16  i) = ImageYA16 (f i)
    aux (ImageRGB8  i) = ImageRGB8 (f i)
    aux (ImageRGB16 i) = ImageRGB16 (f i)
    aux (ImageRGBF  i) = ImageRGBF (f i)
    aux (ImageRGBA8 i) = ImageRGBA8 (f i)
    aux (ImageRGBA16 i) = ImageRGBA16 (f i)
    aux (ImageYCbCr8 i) = ImageYCbCr8 (f i)
    aux (ImageCMYK8 i) = ImageCMYK8 (f i)
    aux (ImageCMYK16 i) = ImageCMYK16 (f i)

instance NFData DynamicImage where
    rnf (ImageY8 img)     = rnf img
    rnf (ImageY16 img)    = rnf img
    rnf (ImageYF img)     = rnf img
    rnf (ImageYA8 img)    = rnf img
    rnf (ImageYA16 img)   = rnf img
    rnf (ImageRGB8 img)   = rnf img
    rnf (ImageRGB16 img)  = rnf img
    rnf (ImageRGBF img)   = rnf img
    rnf (ImageRGBA8 img)  = rnf img
    rnf (ImageRGBA16 img) = rnf img
    rnf (ImageYCbCr8 img) = rnf img
    rnf (ImageCMYK8 img)  = rnf img
    rnf (ImageCMYK16 img)  = rnf img

-- | Pixel type Luminance (Y) and alpha (A) information.
-- Values are stored in the following order:
--
--  * Luminance
--
--  * Alpha
--
data YA c = YA !c -- Luminance
               !c  -- Alpha value
  deriving (Eq, Ord, Show)

instance Functor YA where
  fmap f (YA y a) = YA (f y) (f a)

instance Applicative YA where
  pure a = YA a a
  (YA fy fa) <*> (YA y a) = YA (fy y) (fa a)

instance Foldable YA where
  foldr f acc (YA y a) = f y $ f a acc

-- | Classic pixel type storing red, green and blue (RGB) information.
-- Values are stored in the following order:
--
--  * Red
--
--  * Green
--
--  * Blue
--
data RGB c = RGB !c -- Red
                 !c -- Green
                 !c -- Blue
  deriving (Eq, Ord, Show)

instance Functor RGB where
  fmap f (RGB r g b) = RGB (f r) (f g) (f b)

instance Applicative RGB where
  pure a = RGB a a a
  (RGB fr fg fb) <*> (RGB r g b) = RGB (fr r) (fg g) (fb b)

instance Foldable RGB where
  foldr f acc (RGB r g b) = f r . f g $ f b acc

-- | Pixel type storing value for the YCCK color space:
--
-- * Y (Luminance)
--
-- * Cb
--
-- * Cr
--
-- * Black
--
data YCbCrK c = YCbCrK !c !c !c !c
  deriving (Eq, Ord, Show)

instance Functor YCbCrK where
  fmap f (YCbCrK y cb cr k) = YCbCrK (f y) (f cb) (f cr) (f k)

instance Applicative YCbCrK where
  pure a = YCbCrK a a a a
  (YCbCrK fy fcb fcr fk) <*> (YCbCrK y cb cr k) =
      YCbCrK (fy y) (fcb cb) (fcr cr) (fk k)

instance Foldable YCbCrK where
  foldr f acc (YCbCrK y cb cr k) = f y . f cb . f cr $ f k acc

-- | Pixel type storing luminance, blue difference and red difference (YCbCr) information.
-- Values are stored in the following order:
--
--  * Y (luminance)
--
--  * Cb
--
--  * Cr
--
data YCbCr c = YCbCr !c -- Y luminance
                     !c -- Cb blue difference
                     !c -- Cr red difference
  deriving (Eq, Ord, Show)

instance Functor YCbCr where
  fmap f (YCbCr y cb cr) = YCbCr (f y) (f cb) (f cr)

instance Applicative YCbCr where
  pure a = YCbCr a a a
  (YCbCr fy fcb fcr) <*> (YCbCr y cb cr) =
      YCbCr (fy y) (fcb cb) (fcr cr)

instance Foldable YCbCr where
  foldr f acc (YCbCr y cb cr) = f y . f cb $ f cr acc

-- | Pixel type storing cyan, magenta, yellow and black (CMYK) information.
-- Values are stored in the following order:
--
--   * Cyan
--
--   * Magenta
--
--   * Yellow
--
--   * Black
--
data CMYK c = CMYK !c -- Cyan
                   !c -- Magenta
                   !c -- Yellow
                   !c -- Black
  deriving (Eq, Ord, Show)

instance Functor CMYK where
  fmap f (CMYK c m y k) = CMYK (f c) (f m) (f y) (f k)

instance Applicative CMYK where
  pure a = CMYK a a a a
  (CMYK fc fm fy fk) <*> (CMYK c m y k) =
      CMYK (fc c) (fm m) (fy y) (fk k)

instance Foldable CMYK where
  foldr f acc (CMYK c m y k) = f c . f y . f m $ f k acc

-- | Classical pixel type storing red, green, blue and alpha (RGBA) information.
-- Values are stored in the following order:
--
--  * Red
--
--  * Green
--
--  * Blue
--
--  * Alpha
--
data RGBA c = RGBA !c -- Red
                   !c -- Green
                   !c -- Blue
                   !c -- Alpha
  deriving (Eq, Ord, Show)

instance Functor RGBA where
  fmap f (RGBA r g b a) = RGBA (f r) (f g) (f b) (f a)

instance Applicative RGBA where
  pure a = RGBA a a a a
  (RGBA fr fg fb fa) <*> (RGBA r g b a) =
      RGBA (fr r) (fg g) (fb b) (fa a)

instance Foldable RGBA where
  foldr f acc (RGBA r g b a) = f r . f g . f b $ f a acc

instance LumaPlaneExtractable (RGB PixelF) where
    {-# INLINE computeLuma #-}
    computeLuma (RGB r g b) =
        0.3 * r + 0.59 * g + 0.11 * b

instance LumaPlaneExtractable (RGBA Pixel8) where
    {-# INLINE computeLuma #-}
    computeLuma (RGBA r g b _) = floor $ 0.3 * toRational r +
                                             0.59 * toRational g +
                                             0.11 * toRational b

instance (Storable a, Pixel a, BasicComponent (YCbCr a)) =>
        LumaPlaneExtractable (YCbCr a) where
    {-# INLINE computeLuma #-}
    computeLuma (YCbCr y _ _) = y
    extractLumaPlane = extractComponent PlaneLuma

instance (Pixel a, Storable a) => ColorConvertible a (YA a) where
    {-# INLINE promotePixel #-}
    promotePixel c = YA c saturatedPixel

instance (Pixel a, Storable a) => ColorConvertible a (RGB a) where
    {-# INLINE promotePixel #-}
    promotePixel c = RGB c c c

instance (Pixel a, Storable a) => ColorConvertible a (RGBA a) where
    {-# INLINE promotePixel #-}
    promotePixel c = RGBA c c c saturatedPixel

--------------------------------------------------
----            PixelYA instances
--------------------------------------------------
instance (Pixel a, Storable a) => Pixel (YA a) where
    type PixelBaseComponent (YA a) = a

    emptyPixel = YA emptyPixel saturatedPixel
    saturatedPixel = YA saturatedPixel saturatedPixel

    {-# INLINE pixelOpacity #-}
    pixelOpacity (YA _ a) = a

    {-# INLINE mixWith #-}
    mixWith f (YA ya aa) (YA yb ab) =
        YA (f 0 ya yb) (f 1 aa ab)


    {-# INLINE colorMap #-}
    colorMap f (YA y a) = YA (f y) (f a)
    {-# INLINE componentCount #-}
    componentCount _ = 2
    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y =
        YA (arr ! (baseIdx + 0)) (arr ! (baseIdx + 1))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        yv <- arr `M.read` baseIdx
        av <- arr `M.read` (baseIdx + 1)
        return $ YA yv av
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (YA yv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) yv
        (arr `M.write` (baseIdx + 1)) av

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        YA (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1)
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        YA `liftM` M.unsafeRead vec idx `ap` M.unsafeRead vec (idx + 1)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (YA y a) =
        M.unsafeWrite v idx y >> M.unsafeWrite v (idx + 1) a

instance (Storable a, Pixel a) => ColorConvertible (YA a) (RGB a) where
    {-# INLINE promotePixel #-}
    promotePixel (YA y _) = RGB y y y

instance (Pixel a, Storable a) => ColorConvertible (YA a) (RGBA a) where
    {-# INLINE promotePixel #-}
    promotePixel (YA y a) = RGBA y y y a

instance ColorPlane (YA a) PlaneLuma where
    toComponentIndex _ _ = 0

instance ColorPlane (YA a) PlaneAlpha where
    toComponentIndex _ _ = 1

instance (Pixel a, Storable a) => TransparentPixel (YA a) a where
    {-# INLINE dropTransparency #-}
    dropTransparency (YA y _) = y
    setOpacity a (YA y _) = YA y a

instance (Pixel a, Storable a, BasicComponent (YA a)) =>
    LumaPlaneExtractable (YA a) where
    {-# INLINE computeLuma #-}
    computeLuma (YA y _) = y
    extractLumaPlane = extractComponent PlaneLuma

instance ColorPlane (RGB a) PlaneRed where
    toComponentIndex _ _ = 0

instance ColorPlane (RGB a) PlaneGreen where
    toComponentIndex _ _ = 1

instance ColorPlane (RGB a) PlaneBlue where
    toComponentIndex _ _ = 2

instance (Pixel a, Storable a, Integral a, Bounded a) =>
        ColorSpaceConvertible (RGB a) (CMYK a) where
    {-# INLINE convertPixel #-}
    convertPixel (RGB r g b) = integralRGBToCMYK CMYK (r, g, b)

instance (Storable a, Pixel a) => ColorConvertible (RGB a) (RGBA a) where
    {-# INLINE promotePixel #-}
    promotePixel (RGB r g b) = RGBA r g b saturatedPixel

instance LumaPlaneExtractable (RGB Pixel16) where
    {-# INLINE computeLuma #-}
    computeLuma (RGB r g b) =
      floor $ 0.3 * toRational r + 0.59 * toRational g + 0.11 * toRational b

--------------------------------------------------
----            PixelRGB8 instances
--------------------------------------------------
instance (Pixel a, Storable a) => Pixel (RGB a) where
    type PixelBaseComponent (RGB a) = a

    emptyPixel = RGB emptyPixel emptyPixel emptyPixel
    saturatedPixel = RGB saturatedPixel saturatedPixel saturatedPixel

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const saturatedPixel

    {-# INLINE mixWith #-}
    mixWith f (RGB ra ga ba) (RGB rb gb bb) =
        RGB (f 0 ra rb) (f 1 ga gb) (f 2 ba bb)

    {-# INLINE colorMap #-}
    colorMap f (RGB r g b) = RGB (f r) (f g) (f b)

    {-# INLINE componentCount #-}
    componentCount _ = 3

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y =
        RGB (arr ! (baseIdx + 0)) (arr ! (baseIdx + 1)) (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr `M.read` baseIdx
        gv <- arr `M.read` (baseIdx + 1)
        bv <- arr `M.read` (baseIdx + 2)
        return $ RGB rv gv bv
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (RGB rv gv bv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        RGB (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2)
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        RGB `liftM` M.unsafeRead vec idx
                  `ap` M.unsafeRead vec (idx + 1)
                  `ap` M.unsafeRead vec (idx + 2)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (RGB r g b) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b

instance (Storable a, Storable b,
          ColorConvertible a b) => ColorConvertible (RGB a) (RGBA b) where
    {-# INLINE promotePixel #-}
    promotePixel (RGB r g b) = RGBA (promotePixel r)
                                    (promotePixel g)
                                    (promotePixel b)
                                    saturatedPixel

instance (Storable a, Storable b,
          ColorConvertible a b) => ColorConvertible (RGB a) (RGB b) where
    {-# INLINE promotePixel #-}
    promotePixel (RGB r g b) =
        RGB (promotePixel r) (promotePixel g) (promotePixel b)

instance LumaPlaneExtractable (RGB Word8) where
    {-# INLINE computeLuma #-}
    computeLuma (RGB r g b) =
        floor $ 0.3 * toRational r + 0.59 * toRational g + 0.11 * toRational b

--------------------------------------------------
----            PixelRGBA8 instances
--------------------------------------------------
instance (Pixel a, Storable a) => Pixel (RGBA a) where
    type PixelBaseComponent (RGBA a) = a

    emptyPixel =
        RGBA emptyPixel emptyPixel emptyPixel saturatedPixel
    saturatedPixel =
        RGBA saturatedPixel saturatedPixel saturatedPixel saturatedPixel

    {-# INLINE pixelOpacity #-}
    pixelOpacity (RGBA _ _ _ a) = a

    {-# INLINE mixWith #-}
    mixWith f (RGBA ra ga ba aa) (RGBA rb gb bb ab) =
        RGBA (f 0 ra rb) (f 1 ga gb) (f 2 ba bb) (f 3 aa ab)

    {-# INLINE mixWithAlpha #-}
    mixWithAlpha f fa (RGBA ra ga ba aa) (RGBA rb gb bb ab) =
        RGBA (f 0 ra rb) (f 1 ga gb) (f 2 ba bb) (fa aa ab)

    {-# INLINE colorMap #-}
    colorMap f (RGBA r g b a) = RGBA (f r) (f g) (f b) (f a)

    {-# INLINE componentCount #-}
    componentCount _ = 4

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y =
        RGBA (arr ! (baseIdx + 0)) (arr ! (baseIdx + 1))
             (arr ! (baseIdx + 2)) (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
      r <- arr `M.read`  baseIdx     
      g <- arr `M.read` (baseIdx + 1)
      b <- arr `M.read` (baseIdx + 2)
      a <- arr `M.read` (baseIdx + 3)
      return $ RGBA r g b a
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (RGBA rv gv bv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv
        (arr `M.write` (baseIdx + 3)) av

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        RGBA (V.unsafeIndex v idx)
             (V.unsafeIndex v $ idx + 1)
             (V.unsafeIndex v $ idx + 2)
             (V.unsafeIndex v $ idx + 3)

    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx = do
      r <- M.unsafeRead vec idx
      g <- M.unsafeRead vec (idx + 1)
      b <- M.unsafeRead vec (idx + 2)
      a <- M.unsafeRead vec (idx + 3)
      return $ RGBA r g b a

    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (RGBA r g b a) = do
        M.unsafeWrite v idx r
        M.unsafeWrite v (idx + 1) g
        M.unsafeWrite v (idx + 2) b
        M.unsafeWrite v (idx + 3) a


instance ColorPlane (RGBA a) PlaneRed where
    toComponentIndex _ _ = 0

instance ColorPlane (RGBA a) PlaneGreen where
    toComponentIndex _ _ = 1

instance ColorPlane (RGBA a) PlaneBlue where
    toComponentIndex _ _ = 2

instance ColorPlane (RGBA a) PlaneAlpha where
    toComponentIndex _ _ = 3

--------------------------------------------------
----            PixelYCbCr8 instances
--------------------------------------------------
instance (Pixel a, Storable a) => Pixel (YCbCr a) where
    type PixelBaseComponent (YCbCr a) = a

    emptyPixel = YCbCr emptyPixel emptyPixel emptyPixel
    saturatedPixel = YCbCr saturatedPixel saturatedPixel saturatedPixel

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const saturatedPixel

    {-# INLINE mixWith #-}
    mixWith f (YCbCr ya cba cra) (YCbCr yb cbb crb) =
        YCbCr (f 0 ya yb) (f 1 cba cbb) (f 2 cra crb)

    {-# INLINE colorMap #-}
    colorMap f (YCbCr y cb cr) = YCbCr (f y) (f cb) (f cr)
    {-# INLINE componentCount #-}
    componentCount _ = 3
    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y =
      YCbCr (arr ! (baseIdx + 0)) (arr ! (baseIdx + 1)) (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
      yp <- arr `M.read` baseIdx
      cb <- arr `M.read` (baseIdx + 1)
      cr <- arr `M.read` (baseIdx + 2)
      return $ YCbCr yp cb cr
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (YCbCr yv cbv crv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) yv
        (arr `M.write` (baseIdx + 1)) cbv
        (arr `M.write` (baseIdx + 2)) crv

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
      YCbCr (V.unsafeIndex v idx)
            (V.unsafeIndex v $ idx + 1)
            (V.unsafeIndex v $ idx + 2)

    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx = do
      y <- M.unsafeRead vec idx
      cb <- M.unsafeRead vec (idx + 1)
      cr <- M.unsafeRead vec (idx + 2)
      return $ YCbCr y cb cr

    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (YCbCr y cb cr) = do
        M.unsafeWrite v idx y
        M.unsafeWrite v (idx + 1) cb
        M.unsafeWrite v (idx + 2) cr

scaleBits, oneHalf :: Int
scaleBits = 16
oneHalf = 1 `unsafeShiftL` (scaleBits - 1)

fix :: Float -> Int
fix x = floor $ x * fromIntegral ((1 :: Int) `unsafeShiftL` scaleBits) + 0.5


rYTab, gYTab, bYTab, rCbTab, gCbTab, bCbTab, gCrTab, bCrTab :: V.Vector Int
rYTab = V.fromListN 256 [fix 0.29900 * i | i <- [0..255] ]
gYTab = V.fromListN 256 [fix 0.58700 * i | i <- [0..255] ]
bYTab = V.fromListN 256 [fix 0.11400 * i + oneHalf | i <- [0..255] ]
rCbTab = V.fromListN 256 [(- fix 0.16874) * i | i <- [0..255] ]
gCbTab = V.fromListN 256 [(- fix 0.33126) * i | i <- [0..255] ]
bCbTab = V.fromListN 256 [fix 0.5 * i + (128 `unsafeShiftL` scaleBits) + oneHalf - 1| i <- [0..255] ]
gCrTab = V.fromListN 256 [(- fix 0.41869) * i | i <- [0..255] ]
bCrTab = V.fromListN 256 [(- fix 0.08131) * i | i <- [0..255] ]


instance ColorSpaceConvertible (RGB Pixel8) (YCbCr Pixel8) where
    {-# INLINE convertPixel #-}
    convertPixel (RGB r g b) = YCbCr (fromIntegral y) (fromIntegral cb) (fromIntegral cr)
      where ri = fromIntegral r
            gi = fromIntegral g
            bi = fromIntegral b

            y  = (rYTab `V.unsafeIndex` ri + gYTab `V.unsafeIndex` gi + bYTab `V.unsafeIndex` bi) `unsafeShiftR` scaleBits
            cb = (rCbTab `V.unsafeIndex` ri + gCbTab `V.unsafeIndex` gi + bCbTab `V.unsafeIndex` bi) `unsafeShiftR` scaleBits
            cr = (bCbTab `V.unsafeIndex` ri + gCrTab `V.unsafeIndex` gi + bCrTab `V.unsafeIndex` bi) `unsafeShiftR` scaleBits

    convertImage Image { imageWidth = w, imageHeight = h, imageData = d } = Image w h newData
        where maxi = w * h

              rY  = fix 0.29900
              gY  = fix 0.58700
              bY  = fix 0.11400
              rCb = - fix 0.16874
              gCb = - fix 0.33126
              bCb = fix 0.5
              gCr = - fix 0.41869
              bCr = - fix 0.08131

              newData = runST $ do
                block <- M.new $ maxi * 3
                let traductor _ idx | idx >= maxi = return block
                    traductor readIdx idx = do
                        let ri = fromIntegral $ d `V.unsafeIndex` readIdx
                            gi = fromIntegral $ d `V.unsafeIndex` (readIdx + 1)
                            bi = fromIntegral $ d `V.unsafeIndex` (readIdx + 2)

                            y  = (rY * ri + gY * gi + bY * bi + oneHalf) `unsafeShiftR` scaleBits
                            cb = (rCb * ri + gCb * gi + bCb * bi + (128 `unsafeShiftL` scaleBits) + oneHalf - 1) `unsafeShiftR` scaleBits
                            cr = (bCb * ri + (128 `unsafeShiftL` scaleBits) + oneHalf - 1+ gCr * gi + bCr * bi) `unsafeShiftR` scaleBits

                        (block `M.unsafeWrite` (readIdx + 0)) $ fromIntegral y
                        (block `M.unsafeWrite` (readIdx + 1)) $ fromIntegral cb
                        (block `M.unsafeWrite` (readIdx + 2)) $ fromIntegral cr
                        traductor (readIdx + 3) (idx + 1)

                traductor 0 0 >>= V.freeze

crRTab, cbBTab, crGTab, cbGTab :: V.Vector Int
crRTab = V.fromListN 256 [(fix 1.40200 * x + oneHalf) `unsafeShiftR` scaleBits | x <- [-128 .. 127]]
cbBTab = V.fromListN 256 [(fix 1.77200 * x + oneHalf) `unsafeShiftR` scaleBits | x <- [-128 .. 127]]
crGTab = V.fromListN 256 [negate (fix 0.71414) * x | x <- [-128 .. 127]]
cbGTab = V.fromListN 256 [negate (fix 0.34414) * x + oneHalf | x <- [-128 .. 127]]

instance ColorSpaceConvertible (YCbCr Pixel8) (RGB Pixel8) where
    {-# INLINE convertPixel #-}
    convertPixel (YCbCr y cb cr) = RGB (clampWord8 r) (clampWord8 g) (clampWord8 b)
        where clampWord8 = fromIntegral . max 0 . min 255
              yi = fromIntegral y
              cbi = fromIntegral cb
              cri = fromIntegral cr

              r = yi +  crRTab `V.unsafeIndex` cri
              g = yi + (cbGTab `V.unsafeIndex` cbi + crGTab `V.unsafeIndex` cri) `unsafeShiftR` scaleBits
              b = yi +  cbBTab `V.unsafeIndex` cbi

    convertImage Image { imageWidth = w, imageHeight = h, imageData = d } = Image w h newData
        where maxi = w * h
              clampWord8 v | v < 0 = 0
                           | v > 255 = 255
                           | otherwise = fromIntegral v

              newData = runST $ do
                block <- M.new $ maxi * 3
                let traductor _ idx | idx >= maxi = return block
                    traductor readIdx idx = do
                        let yi =  fromIntegral $ d `V.unsafeIndex` readIdx
                            cbi = fromIntegral $ d `V.unsafeIndex` (readIdx + 1)
                            cri = fromIntegral $ d `V.unsafeIndex` (readIdx + 2)

                            r = yi +  crRTab `V.unsafeIndex` cri
                            g = yi + (cbGTab `V.unsafeIndex` cbi + crGTab `V.unsafeIndex` cri) `unsafeShiftR` scaleBits
                            b = yi +  cbBTab `V.unsafeIndex` cbi

                        (block `M.unsafeWrite` (readIdx + 0)) $ clampWord8 r
                        (block `M.unsafeWrite` (readIdx + 1)) $ clampWord8 g
                        (block `M.unsafeWrite` (readIdx + 2)) $ clampWord8 b
                        traductor (readIdx + 3) (idx + 1)

                traductor 0 0 >>= V.freeze

instance ColorPlane (YCbCr a) PlaneLuma where
    toComponentIndex _ _ = 0

instance ColorPlane (YCbCr a) PlaneCb where
    toComponentIndex _ _ = 1

instance ColorPlane (YCbCr a) PlaneCr where
    toComponentIndex _ _ = 2

--------------------------------------------------
----            PixelCMYK8 instances
--------------------------------------------------
instance (Pixel a, Storable a) => Pixel (CMYK a) where
    type PixelBaseComponent (CMYK a) = a

    emptyPixel =
        CMYK emptyPixel emptyPixel emptyPixel emptyPixel
    saturatedPixel =
        CMYK saturatedPixel saturatedPixel saturatedPixel saturatedPixel

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const saturatedPixel

    {-# INLINE mixWith #-}
    mixWith f (CMYK ca ma ya ka) (CMYK cb mb yb kb) =
        CMYK (f 0 ca cb) (f 1 ma mb) (f 2 ya yb) (f 3 ka kb)

    {-# INLINE colorMap #-}
    colorMap f (CMYK c m y k) = CMYK (f c) (f m) (f y) (f k)

    {-# INLINE componentCount #-}
    componentCount _ = 4

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y =
        CMYK (arr ! (baseIdx + 0)) (arr ! (baseIdx + 1))
             (arr ! (baseIdx + 2)) (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
      c <- arr `M.read`  baseIdx
      m <- arr `M.read` (baseIdx + 1)
      yp <- arr `M.read` (baseIdx + 2)
      k <- arr `M.read` (baseIdx + 3)
      return $ CMYK c m yp k
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (CMYK rv gv bv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv
        (arr `M.write` (baseIdx + 3)) av

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        CMYK (V.unsafeIndex v   idx    ) (V.unsafeIndex v $ idx + 1)
             (V.unsafeIndex v $ idx + 2) (V.unsafeIndex v $ idx + 3)

    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx = do
      c <- M.unsafeRead vec  idx
      m <- M.unsafeRead vec (idx + 1)
      y <- M.unsafeRead vec (idx + 2)
      k <- M.unsafeRead vec (idx + 3)
      return $ CMYK c m y k

    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (CMYK r g b a) = do
        M.unsafeWrite v idx r
        M.unsafeWrite v (idx + 1) g
        M.unsafeWrite v (idx + 2) b
        M.unsafeWrite v (idx + 3) a

instance ColorSpaceConvertible (CMYK Word8) (RGB Word8) where
  convertPixel (CMYK c m y k) =
      RGB (clampWord8 r) (clampWord8 g) (clampWord8 b)
    where
      clampWord8 = fromIntegral . max 0 . min 255 . (`div` 255)
      ik :: Int
      ik = 255 - fromIntegral k

      r = (255 - fromIntegral c) * ik
      g = (255 - fromIntegral m) * ik
      b = (255 - fromIntegral y) * ik

--------------------------------------------------
----            PixelYCbCrK8 instances
--------------------------------------------------
instance (Storable a, Pixel a) => Pixel (YCbCrK a) where
    type PixelBaseComponent (YCbCrK a) = a

    emptyPixel =
        YCbCrK emptyPixel emptyPixel emptyPixel emptyPixel
    saturatedPixel =
        YCbCrK saturatedPixel saturatedPixel saturatedPixel saturatedPixel

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const saturatedPixel

    {-# INLINE mixWith #-}
    mixWith f (YCbCrK ya cba cra ka) (YCbCrK yb cbb crb kb) =
      YCbCrK (f 0 ya yb) (f 1 cba cbb) (f 2 cra crb) (f 3 ka kb)

    {-# INLINE colorMap #-}
    colorMap f (YCbCrK y cb cr k) = YCbCrK (f y) (f cb) (f cr) (f k)

    {-# INLINE componentCount #-}
    componentCount _ = 4

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y =
        YCbCrK (arr ! (baseIdx + 0)) (arr ! (baseIdx + 1))
               (arr ! (baseIdx + 2)) (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
      yp <- arr `M.read` baseIdx
      cb <- arr `M.read` (baseIdx + 1)
      cr <- arr `M.read` (baseIdx + 2)
      k <- arr `M.read` (baseIdx + 3)
      return $ YCbCrK yp cb cr k
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (YCbCrK yv cbv crv kv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) yv
        (arr `M.write` (baseIdx + 1)) cbv
        (arr `M.write` (baseIdx + 2)) crv
        (arr `M.write` (baseIdx + 3)) kv

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        YCbCrK (V.unsafeIndex v   idx    ) (V.unsafeIndex v $ idx + 1)
               (V.unsafeIndex v $ idx + 2) (V.unsafeIndex v $ idx + 3)

    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx = do
      y <- M.unsafeRead vec  idx
      cb <- M.unsafeRead vec (idx + 1)
      cr <- M.unsafeRead vec (idx + 2)
      k <- M.unsafeRead vec (idx + 3)
      return $ YCbCrK y cb cr k

    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (YCbCrK y cb cr k) = do
      M.unsafeWrite v idx y
      M.unsafeWrite v (idx + 1) cb
      M.unsafeWrite v (idx + 2) cr
      M.unsafeWrite v (idx + 3) k

instance ColorSpaceConvertible (YCbCrK Pixel8) (RGB Pixel8) where
  convertPixel (YCbCrK y cb cr _k) = RGB (clamp r) (clamp g) (clamp b)
    where
      tof :: Word8 -> Float
      tof = fromIntegral

      clamp :: Float -> Word8
      clamp = floor . max 0 . min 255

      yf = tof y

      r = yf + 1.402 * tof cr - 179.456
      g = yf - 0.3441363 * tof cb - 0.71413636 * tof cr + 135.4589
      b = yf + 1.772 * tof cb - 226.816

instance ColorSpaceConvertible (YCbCrK Pixel8) (CMYK Pixel8) where
  convertPixel (YCbCrK y cb cr k) = CMYK c m ye k
    where
      tof :: Word8 -> Float
      tof = fromIntegral

      clamp :: Float -> Word8
      clamp = floor . max 0 . min 255

      yf = tof y

      r = yf + 1.402 * tof cr - 179.456
      g = yf - 0.3441363 * tof cb - 0.71413636 * tof cr + 135.4589
      b = yf + 1.772 * tof cb - 226.816

      c = clamp $ 255 - r
      m = clamp $ 255 - g
      ye = clamp $ 255 - b

instance ColorPlane (CMYK a) PlaneCyan where
    toComponentIndex _ _ = 0

instance ColorPlane (CMYK a) PlaneMagenta where
    toComponentIndex _ _ = 1

instance ColorPlane (CMYK a) PlaneYellow where
    toComponentIndex _ _ = 2

instance ColorPlane (CMYK a) PlaneBlack where
    toComponentIndex _ _ = 3

--------------------------------------------------
----            Packable pixel
--------------------------------------------------
instance PackeablePixel (RGBA Pixel8) where
    type PackedRepresentation (RGBA Pixel8) = Word32
    {-# INLINE packPixel #-}
    packPixel (RGBA r g b a) =
        (fi r `unsafeShiftL` (0 * bitCount)) .|.
        (fi g `unsafeShiftL` (1 * bitCount)) .|.
        (fi b `unsafeShiftL` (2 * bitCount)) .|.
        (fi a `unsafeShiftL` (3 * bitCount))
      where fi = fromIntegral
            bitCount = 8

    {-# INLINE unpackPixel #-}
    unpackPixel w =
        RGBA (low w)
             (low $ w `unsafeShiftR` bitCount)
             (low $ w `unsafeShiftR` (2 * bitCount))
             (low $ w `unsafeShiftR` (3 * bitCount))
      where
        low v = fromIntegral (v .&. 0xFF)
        bitCount = 8

instance PackeablePixel (RGBA Pixel16) where
    type PackedRepresentation (RGBA Pixel16) = Word64
    {-# INLINE packPixel #-}
    packPixel (RGBA r g b a) =
        (fi r `unsafeShiftL` (0 * bitCount)) .|.
        (fi g `unsafeShiftL` (1 * bitCount)) .|.
        (fi b `unsafeShiftL` (2 * bitCount)) .|.
        (fi a `unsafeShiftL` (3 * bitCount))
      where fi = fromIntegral
            bitCount = 16

    {-# INLINE unpackPixel #-}
    unpackPixel w =
        RGBA (low w)
             (low $ w `unsafeShiftR` bitCount)
             (low $ w `unsafeShiftR` (2 * bitCount))
             (low $ w `unsafeShiftR` (3 * bitCount))
      where
        low v = fromIntegral (v .&. 0xFFFF)
        bitCount = 16

instance PackeablePixel (CMYK Pixel8) where
    type PackedRepresentation (CMYK Pixel8) = Word32
    {-# INLINE packPixel #-}
    packPixel (CMYK c m y k) =
        (fi c `unsafeShiftL` (0 * bitCount)) .|.
        (fi m `unsafeShiftL` (1 * bitCount)) .|.
        (fi y `unsafeShiftL` (2 * bitCount)) .|.
        (fi k `unsafeShiftL` (3 * bitCount))
      where fi = fromIntegral
            bitCount = 8

    {-# INLINE unpackPixel #-}
    unpackPixel w =
        CMYK (low w)
             (low $ w `unsafeShiftR` bitCount)
             (low $ w `unsafeShiftR` (2 * bitCount))
             (low $ w `unsafeShiftR` (3 * bitCount))
      where
        low v = fromIntegral (v .&. 0xFF)
        bitCount = 8

instance PackeablePixel (CMYK Pixel16) where
    type PackedRepresentation (CMYK Pixel16) = Word64
    {-# INLINE packPixel #-}
    packPixel (CMYK c m y k) =
        (fi c `unsafeShiftL` (0 * bitCount)) .|.
        (fi m `unsafeShiftL` (1 * bitCount)) .|.
        (fi y `unsafeShiftL` (2 * bitCount)) .|.
        (fi k `unsafeShiftL` (3 * bitCount))
      where fi = fromIntegral
            bitCount = 16

    {-# INLINE unpackPixel #-}
    unpackPixel w =
        CMYK (low w)
             (low $ w `unsafeShiftR` bitCount)
             (low $ w `unsafeShiftR` (2 * bitCount))
             (low $ w `unsafeShiftR` (3 * bitCount))
      where
        low v = fromIntegral (v .&. 0xFFFF)
        bitCount = 16

instance PackeablePixel (YA Pixel16) where
    type PackedRepresentation (YA Pixel16) = Word32
    {-# INLINE packPixel #-}
    packPixel (YA y a) =
        (fi y `unsafeShiftL` (0 * bitCount)) .|.
        (fi a `unsafeShiftL` (1 * bitCount))
      where fi = fromIntegral
            bitCount = 16

    {-# INLINE unpackPixel #-}
    unpackPixel w = YA (low w) (low $ w `unsafeShiftR` bitCount)
      where
        low v = fromIntegral (v .&. 0xFFFF)
        bitCount = 16

instance PackeablePixel (YA Pixel8) where
    type PackedRepresentation (YA Pixel8) = Word16
    {-# INLINE packPixel #-}
    packPixel (YA y a) =
        (fi y `unsafeShiftL` (0 * bitCount)) .|.
        (fi a `unsafeShiftL` (1 * bitCount))
      where fi = fromIntegral
            bitCount = 8

    {-# INLINE unpackPixel #-}
    unpackPixel w = YA (low w) (low $ w `unsafeShiftR` bitCount)
      where
        low v = fromIntegral (v .&. 0xFF)
        bitCount = 8

-- | Perform a gamma correction for an image with HDR pixels.
gammaCorrection :: (Pixel (f PixelF), Functor f)
                => PixelF          -- ^ Gamma value, should be between 0.5 and 3.0
                -> Image (f PixelF) -- ^ Image to treat.
                -> Image (f PixelF)
gammaCorrection gammaVal = pixelMap (fmap fixVal)
  where gammaExponent = 1.0 / gammaVal
        fixVal v = v ** gammaExponent

-- | Perform a tone mapping operation on an High dynamic range image.
toneMapping :: PixelF          -- ^ Exposure parameter
            -> Image (RGB PixelF) -- ^ Image to treat.
            -> Image (RGB PixelF)
toneMapping exposure img = Image (imageWidth img) (imageHeight img) scaledData
 where coeff = exposure * (exposure / maxBrightness + 1.0) / (exposure + 1.0);
       maxBrightness = pixelFold (\luma _ _ px -> max luma $ computeLuma px) 0 img
       scaledData = V.map (* coeff) $ imageData img

{-# ANN module "HLint: ignore Reduce duplication" #-}

