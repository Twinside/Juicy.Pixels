{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
module Codec.Picture.OldPixels  where

import Control.Monad( liftM, ap )
import Control.DeepSeq( NFData( .. ) )
import Control.Monad.ST( runST )
import Data.Bits( unsafeShiftL, unsafeShiftR, (.|.), (.&.) )
import Data.Word( Word8, Word16, Word32, Word64 )
import Data.Vector.Storable ( (!) )
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import Codec.Picture.BaseTypes

instance TransparentPixel PixelRGBA8 PixelRGB8 where
    {-# INLINE dropTransparency #-}
    dropTransparency (PixelRGBA8 r g b _) = PixelRGB8 r g b

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
     | ImageYA8   (Image PixelYA8)
      -- | An image in greyscale with alpha channel on 16 bits.
     | ImageYA16  (Image PixelYA16)
       -- | An image in true color.
     | ImageRGB8  (Image PixelRGB8)
       -- | An image in true color with 16bit depth.
     | ImageRGB16 (Image PixelRGB16)
       -- | An image with HDR pixels
     | ImageRGBF  (Image PixelRGBF)
       -- | An image in true color and an alpha channel.
     | ImageRGBA8 (Image PixelRGBA8)
       -- | A true color image with alpha on 16 bits.
     | ImageRGBA16 (Image PixelRGBA16)
       -- | An image in the colorspace used by Jpeg images.
     | ImageYCbCr8 (Image PixelYCbCr8)
       -- | An image in the colorspace CMYK
     | ImageCMYK8  (Image PixelCMYK8)
       -- | An image in the colorspace CMYK and 16 bits precision
     | ImageCMYK16 (Image PixelCMYK16)

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

-- | Pixel type storing 8bit Luminance (Y) and alpha (A) information.
-- Values are stored in the following order:
--
--  * Luminance
--
--  * Alpha
--
{-# DEPRECATED PixelYA8
        "Old style JuicyPixel Pixel style, use (YA Pixel8) type instead" #-}
data PixelYA8 = PixelYA8 {-# UNPACK #-} !Pixel8  -- Luminance
                         {-# UNPACK #-} !Pixel8  -- Alpha value
              deriving (Eq, Ord, Show)

-- | Pixel type storing 16bit Luminance (Y) and alpha (A) information.
-- Values are stored in the following order:
--
--  * Luminance
--
--  * Alpha
--
{-# DEPRECATED PixelYA16
        "Old style JuicyPixel Pixel style, use (YA Pixel16) type instead" #-}
data PixelYA16 = PixelYA16 {-# UNPACK #-} !Pixel16  -- Luminance
                           {-# UNPACK #-} !Pixel16  -- Alpha value
              deriving (Eq, Ord, Show)

-- | Classic pixel type storing 8bit red, green and blue (RGB) information.
-- Values are stored in the following order:
--
--  * Red
--
--  * Green
--
--  * Blue
--
{-# DEPRECATED PixelRGB8
        "Old style JuicyPixel Pixel style, use (RGB Pixel8) type instead" #-}
data PixelRGB8 = PixelRGB8 {-# UNPACK #-} !Pixel8 -- Red
                           {-# UNPACK #-} !Pixel8 -- Green
                           {-# UNPACK #-} !Pixel8 -- Blue
               deriving (Eq, Ord, Show)

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
{-# DEPRECATED PixelYCbCrK8
        "Old style JuicyPixel Pixel style, use (YCbCrK Pixel8) type instead" #-}
data PixelYCbCrK8 = PixelYCbCrK8 {-# UNPACK #-} !Pixel8
                                 {-# UNPACK #-} !Pixel8
                                 {-# UNPACK #-} !Pixel8
                                 {-# UNPACK #-} !Pixel8
               deriving (Eq, Ord, Show)

-- | Pixel type storing 16bit red, green and blue (RGB) information.
-- Values are stored in the following order:
--
--  * Red
--
--  * Green
--
--  * Blue
--
{-# DEPRECATED PixelRGB16
        "Old style JuicyPixel Pixel style, use (RGB Pixel16) type instead" #-}
data PixelRGB16 = PixelRGB16 {-# UNPACK #-} !Pixel16 -- Red
                             {-# UNPACK #-} !Pixel16 -- Green
                             {-# UNPACK #-} !Pixel16 -- Blue
               deriving (Eq, Ord, Show)

-- | HDR pixel type storing floating point 32bit red, green and blue (RGB) information.
-- Same value range and comments apply as for 'PixelF'.
-- Values are stored in the following order:
--
--  * Red
--
--  * Green
--
--  * Blue
--
{-# DEPRECATED PixelRGBF
        "Old style JuicyPixel Pixel style, use (RGB PixelF) type instead" #-}
data PixelRGBF = PixelRGBF {-# UNPACK #-} !PixelF -- Red
                           {-# UNPACK #-} !PixelF -- Green
                           {-# UNPACK #-} !PixelF -- Blue
               deriving (Eq, Ord, Show)

-- | Pixel type storing 8bit luminance, blue difference and red difference (YCbCr) information.
-- Values are stored in the following order:
--
--  * Y (luminance)
--
--  * Cb
--
--  * Cr
--
{-# DEPRECATED PixelYCbCr8
        "Old style JuicyPixel Pixel style, use (YCbCr Pixel8) type instead" #-}
data PixelYCbCr8 = PixelYCbCr8 {-# UNPACK #-} !Pixel8 -- Y luminance
                               {-# UNPACK #-} !Pixel8 -- Cb blue difference
                               {-# UNPACK #-} !Pixel8 -- Cr red difference
                 deriving (Eq, Ord, Show)

-- | Pixel type storing 8bit cyan, magenta, yellow and black (CMYK) information.
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
{-# DEPRECATED PixelCMYK8
        "Old style JuicyPixel Pixel style, use (CMYK Pixel8) type instead" #-}
data PixelCMYK8 = PixelCMYK8 {-# UNPACK #-} !Pixel8 -- Cyan
                             {-# UNPACK #-} !Pixel8 -- Magenta
                             {-# UNPACK #-} !Pixel8 -- Yellow
                             {-# UNPACK #-} !Pixel8 -- Black
                 deriving (Eq, Ord, Show)

-- | Pixel type storing 16bit cyan, magenta, yellow and black (CMYK) information.
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
{-# DEPRECATED PixelCMYK16
        "Old style JuicyPixel Pixel style, use (CMYK Pixel16) type instead" #-}
data PixelCMYK16 = PixelCMYK16 {-# UNPACK #-} !Pixel16 -- Cyan
                               {-# UNPACK #-} !Pixel16 -- Magenta
                               {-# UNPACK #-} !Pixel16 -- Yellow
                               {-# UNPACK #-} !Pixel16 -- Black
                 deriving (Eq, Ord, Show)


-- | Classical pixel type storing 8bit red, green, blue and alpha (RGBA) information.
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
{-# DEPRECATED PixelRGBA8
        "Old style JuicyPixel Pixel style, use (RGBA Pixel8) type instead" #-}
data PixelRGBA8 = PixelRGBA8 {-# UNPACK #-} !Pixel8 -- Red
                             {-# UNPACK #-} !Pixel8 -- Green
                             {-# UNPACK #-} !Pixel8 -- Blue
                             {-# UNPACK #-} !Pixel8 -- Alpha
                deriving (Eq, Ord, Show)

-- | Pixel type storing 16bit red, green, blue and alpha (RGBA) information.
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
{-# DEPRECATED PixelRGBA16
        "Old style JuicyPixel Pixel style, use (RGBA Pixel16) type instead" #-}
data PixelRGBA16 = PixelRGBA16 {-# UNPACK #-} !Pixel16 -- Red
                               {-# UNPACK #-} !Pixel16 -- Green
                               {-# UNPACK #-} !Pixel16 -- Blue
                               {-# UNPACK #-} !Pixel16 -- Alpha
                deriving (Eq, Ord, Show)


instance LumaPlaneExtractable PixelRGBF where
    {-# INLINE computeLuma #-}
    computeLuma (PixelRGBF r g b) =
        0.3 * r + 0.59 * g + 0.11 * b

instance LumaPlaneExtractable PixelRGBA8 where
    {-# INLINE computeLuma #-}
    computeLuma (PixelRGBA8 r g b _) = floor $ 0.3 * toRational r +
                                             0.59 * toRational g +
                                             0.11 * toRational b

instance LumaPlaneExtractable PixelYCbCr8 where
    {-# INLINE computeLuma #-}
    computeLuma (PixelYCbCr8 y _ _) = y
    extractLumaPlane = extractComponent PlaneLuma

--------------------------------------------------
----            Pixel8 instances
--------------------------------------------------
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
----            Pixel16 instances
--------------------------------------------------
instance ColorConvertible Pixel16 PixelYA16 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelYA16 c maxBound

instance ColorConvertible Pixel16 PixelRGB16 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelRGB16 c c c

instance ColorConvertible Pixel16 PixelRGBA16 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelRGBA16 c c c maxBound

--------------------------------------------------
----            Pixel32 instances
--------------------------------------------------
--------------------------------------------------
----            PixelF instances
--------------------------------------------------
instance ColorConvertible PixelF PixelRGBF where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelRGBF c c c-- (c / 0.3) (c / 0.59)  (c / 0.11)

--------------------------------------------------
----            PixelYA8 instances
--------------------------------------------------
instance Pixel PixelYA8 where
    type PixelBaseComponent PixelYA8 = Word8

    {-# INLINE pixelOpacity #-}
    pixelOpacity (PixelYA8 _ a) = a

    {-# INLINE mixWith #-}
    mixWith f (PixelYA8 ya aa) (PixelYA8 yb ab) =
        PixelYA8 (f 0 ya yb) (f 1 aa ab)


    {-# INLINE colorMap #-}
    colorMap f (PixelYA8 y a) = PixelYA8 (f y) (f a)
    {-# INLINE componentCount #-}
    componentCount _ = 2
    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y =
        PixelYA8 (arr ! (baseIdx + 0)) (arr ! (baseIdx + 1))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        yv <- arr `M.read` baseIdx
        av <- arr `M.read` (baseIdx + 1)
        return $ PixelYA8 yv av
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelYA8 yv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) yv
        (arr `M.write` (baseIdx + 1)) av

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelYA8 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1)
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelYA8 `liftM` M.unsafeRead vec idx `ap` M.unsafeRead vec (idx + 1)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelYA8 y a) =
        M.unsafeWrite v idx y >> M.unsafeWrite v (idx + 1) a

instance ColorConvertible PixelYA8 PixelRGB8 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelYA8 y _) = PixelRGB8 y y y

instance ColorConvertible PixelYA8 PixelRGBA8 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelYA8 y a) = PixelRGBA8 y y y a

instance ColorPlane PixelYA8 PlaneLuma where
    toComponentIndex _ _ = 0

instance ColorPlane PixelYA8 PlaneAlpha where
    toComponentIndex _ _ = 1

instance TransparentPixel PixelYA8 Pixel8 where
    {-# INLINE dropTransparency #-}
    dropTransparency (PixelYA8 y _) = y

instance LumaPlaneExtractable PixelYA8 where
    {-# INLINE computeLuma #-}
    computeLuma (PixelYA8 y _) = y
    extractLumaPlane = extractComponent PlaneLuma

--------------------------------------------------
----            PixelYA16 instances
--------------------------------------------------
instance Pixel PixelYA16 where
    type PixelBaseComponent PixelYA16 = Word16

    {-# INLINE pixelOpacity #-}
    pixelOpacity (PixelYA16 _ a) = a

    {-# INLINE mixWith #-}
    mixWith f (PixelYA16 ya aa) (PixelYA16 yb ab) =
        PixelYA16 (f 0 ya yb) (f 1 aa ab)

    {-# INLINE mixWithAlpha #-}
    mixWithAlpha f fa (PixelYA16 ya aa) (PixelYA16 yb ab) =
        PixelYA16 (f 0 ya yb) (fa aa ab)

    {-# INLINE colorMap #-}
    colorMap f (PixelYA16 y a) = PixelYA16 (f y) (f a)
    {-# INLINE componentCount #-}
    componentCount _ = 2
    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y = PixelYA16 (arr ! (baseIdx + 0))
                                                              (arr ! (baseIdx + 1))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        yv <- arr `M.read` baseIdx
        av <- arr `M.read` (baseIdx + 1)
        return $ PixelYA16 yv av
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelYA16 yv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) yv
        (arr `M.write` (baseIdx + 1)) av

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelYA16 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1)
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelYA16 `liftM` M.unsafeRead vec idx `ap` M.unsafeRead vec (idx + 1)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelYA16 y a) =
        M.unsafeWrite v idx y >> M.unsafeWrite v (idx + 1) a

instance ColorConvertible PixelYA16 PixelRGBA16 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelYA16 y a) = PixelRGBA16 y y y a

instance ColorPlane PixelYA16 PlaneLuma where
    toComponentIndex _ _ = 0

instance ColorPlane PixelYA16 PlaneAlpha where
    toComponentIndex _ _ = 1

instance TransparentPixel PixelYA16 Pixel16 where
    {-# INLINE dropTransparency #-}
    dropTransparency (PixelYA16 y _) = y

--------------------------------------------------
----            PixelRGBF instances
--------------------------------------------------
instance Pixel PixelRGBF where
    type PixelBaseComponent PixelRGBF = PixelF

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const 1.0

    {-# INLINE mixWith #-}
    mixWith f (PixelRGBF ra ga ba) (PixelRGBF rb gb bb) =
        PixelRGBF (f 0 ra rb) (f 1 ga gb) (f 2 ba bb)

    {-# INLINE colorMap #-}
    colorMap f (PixelRGBF r g b) = PixelRGBF (f r) (f g) (f b)

    {-# INLINE componentCount #-}
    componentCount _ = 3

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y = PixelRGBF (arr ! (baseIdx + 0))
                                                              (arr ! (baseIdx + 1))
                                                              (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr `M.read` baseIdx
        gv <- arr `M.read` (baseIdx + 1)
        bv <- arr `M.read` (baseIdx + 2)
        return $ PixelRGBF rv gv bv
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGBF rv gv bv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelRGBF (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2)
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelRGBF `liftM` M.unsafeRead vec idx
                  `ap` M.unsafeRead vec (idx + 1)
                  `ap` M.unsafeRead vec (idx + 2)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelRGBF r g b) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b

instance ColorPlane PixelRGBF PlaneRed where
    toComponentIndex _ _ = 0

instance ColorPlane PixelRGBF PlaneGreen where
    toComponentIndex _ _ = 1

instance ColorPlane PixelRGBF PlaneBlue where
    toComponentIndex _ _ = 2

--------------------------------------------------
----            PixelRGB16 instances
--------------------------------------------------
instance Pixel PixelRGB16 where
    type PixelBaseComponent PixelRGB16 = Pixel16

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f (PixelRGB16 ra ga ba) (PixelRGB16 rb gb bb) =
        PixelRGB16 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb)

    {-# INLINE colorMap #-}
    colorMap f (PixelRGB16 r g b) = PixelRGB16 (f r) (f g) (f b)

    {-# INLINE componentCount #-}
    componentCount _ = 3

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y = PixelRGB16 (arr ! (baseIdx + 0))
                                                               (arr ! (baseIdx + 1))
                                                               (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr `M.read` baseIdx
        gv <- arr `M.read` (baseIdx + 1)
        bv <- arr `M.read` (baseIdx + 2)
        return $ PixelRGB16 rv gv bv
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGB16 rv gv bv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelRGB16 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2)
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelRGB16 `liftM` M.unsafeRead vec idx
                   `ap` M.unsafeRead vec (idx + 1)
                   `ap` M.unsafeRead vec (idx + 2)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelRGB16 r g b) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b

instance ColorPlane PixelRGB16 PlaneRed where
    toComponentIndex _ _ = 0

instance ColorPlane PixelRGB16 PlaneGreen where
    toComponentIndex _ _ = 1

instance ColorPlane PixelRGB16 PlaneBlue where
    toComponentIndex _ _ = 2

instance ColorSpaceConvertible PixelRGB16 PixelCMYK16 where
    {-# INLINE convertPixel #-}
    convertPixel (PixelRGB16 r g b) = integralRGBToCMYK PixelCMYK16 (r, g, b)

instance ColorConvertible PixelRGB16 PixelRGBA16 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelRGB16 r g b) = PixelRGBA16 r g b maxBound

instance LumaPlaneExtractable PixelRGB16 where
    {-# INLINE computeLuma #-}
    computeLuma (PixelRGB16 r g b) = floor $ 0.3 * toRational r +
                                             0.59 * toRational g +
                                             0.11 * toRational b
--------------------------------------------------
----            PixelRGB8 instances
--------------------------------------------------
instance Pixel PixelRGB8 where
    type PixelBaseComponent PixelRGB8 = Word8

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f (PixelRGB8 ra ga ba) (PixelRGB8 rb gb bb) =
        PixelRGB8 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb)

    {-# INLINE colorMap #-}
    colorMap f (PixelRGB8 r g b) = PixelRGB8 (f r) (f g) (f b)

    {-# INLINE componentCount #-}
    componentCount _ = 3

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y = PixelRGB8 (arr ! (baseIdx + 0))
                                                              (arr ! (baseIdx + 1))
                                                              (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr `M.read` baseIdx
        gv <- arr `M.read` (baseIdx + 1)
        bv <- arr `M.read` (baseIdx + 2)
        return $ PixelRGB8 rv gv bv
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGB8 rv gv bv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelRGB8 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2)
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelRGB8 `liftM` M.unsafeRead vec idx
                  `ap` M.unsafeRead vec (idx + 1)
                  `ap` M.unsafeRead vec (idx + 2)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelRGB8 r g b) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b

instance ColorConvertible PixelRGB8 PixelRGBA8 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelRGB8 r g b) = PixelRGBA8 r g b maxBound

instance ColorConvertible PixelRGB8 PixelRGBF where
    {-# INLINE promotePixel #-}
    promotePixel (PixelRGB8 r g b) = PixelRGBF (toF r) (toF g) (toF b)
        where toF v = fromIntegral v / 255.0

instance ColorConvertible PixelRGB8 PixelRGB16 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelRGB8 r g b) = PixelRGB16 (promotePixel r) (promotePixel g) (promotePixel b)

instance ColorConvertible PixelRGB8 PixelRGBA16 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelRGB8 r g b) = PixelRGBA16 (promotePixel r) (promotePixel g) (promotePixel b) maxBound

instance ColorPlane PixelRGB8 PlaneRed where
    toComponentIndex _ _ = 0

instance ColorPlane PixelRGB8 PlaneGreen where
    toComponentIndex _ _ = 1

instance ColorPlane PixelRGB8 PlaneBlue where
    toComponentIndex _ _ = 2

instance LumaPlaneExtractable PixelRGB8 where
    {-# INLINE computeLuma #-}
    computeLuma (PixelRGB8 r g b) = floor $ 0.3 * toRational r +
                                            0.59 * toRational g +
                                            0.11 * toRational b

--------------------------------------------------
----            PixelRGBA8 instances
--------------------------------------------------
instance Pixel PixelRGBA8 where
    type PixelBaseComponent PixelRGBA8 = Word8

    {-# INLINE pixelOpacity #-}
    pixelOpacity (PixelRGBA8 _ _ _ a) = a

    {-# INLINE mixWith #-}
    mixWith f (PixelRGBA8 ra ga ba aa) (PixelRGBA8 rb gb bb ab) =
        PixelRGBA8 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb) (f 3 aa ab)

    {-# INLINE mixWithAlpha #-}
    mixWithAlpha f fa (PixelRGBA8 ra ga ba aa) (PixelRGBA8 rb gb bb ab) =
        PixelRGBA8 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb) (fa aa ab)

    {-# INLINE colorMap #-}
    colorMap f (PixelRGBA8 r g b a) = PixelRGBA8 (f r) (f g) (f b) (f a)

    {-# INLINE componentCount #-}
    componentCount _ = 4

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y = PixelRGBA8 (arr ! (baseIdx + 0))
                                                               (arr ! (baseIdx + 1))
                                                               (arr ! (baseIdx + 2))
                                                               (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr `M.read` baseIdx
        gv <- arr `M.read` (baseIdx + 1)
        bv <- arr `M.read` (baseIdx + 2)
        av <- arr `M.read` (baseIdx + 3)
        return $ PixelRGBA8 rv gv bv av
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGBA8 rv gv bv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv
        (arr `M.write` (baseIdx + 3)) av

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelRGBA8 (V.unsafeIndex v idx)
                   (V.unsafeIndex v $ idx + 1)
                   (V.unsafeIndex v $ idx + 2)
                   (V.unsafeIndex v $ idx + 3)

    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelRGBA8 `liftM` M.unsafeRead vec idx
                   `ap` M.unsafeRead vec (idx + 1)
                   `ap` M.unsafeRead vec (idx + 2)
                   `ap` M.unsafeRead vec (idx + 3)

    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelRGBA8 r g b a) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b
                              >> M.unsafeWrite v (idx + 3) a

instance ColorConvertible PixelRGBA8 PixelRGBA16 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelRGBA8 r g b a) = PixelRGBA16 (promotePixel r) (promotePixel g) (promotePixel b) (promotePixel a)

instance ColorPlane PixelRGBA8 PlaneRed where
    toComponentIndex _ _ = 0

instance ColorPlane PixelRGBA8 PlaneGreen where
    toComponentIndex _ _ = 1

instance ColorPlane PixelRGBA8 PlaneBlue where
    toComponentIndex _ _ = 2

instance ColorPlane PixelRGBA8 PlaneAlpha where
    toComponentIndex _ _ = 3

--------------------------------------------------
----            PixelRGBA16 instances
--------------------------------------------------
instance Pixel PixelRGBA16 where
    type PixelBaseComponent PixelRGBA16 = Pixel16

    {-# INLINE pixelOpacity #-}
    pixelOpacity (PixelRGBA16 _ _ _ a) = a

    {-# INLINE mixWith #-}
    mixWith f (PixelRGBA16 ra ga ba aa) (PixelRGBA16 rb gb bb ab) =
        PixelRGBA16 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb) (f 3 aa ab)

    {-# INLINE mixWithAlpha #-}
    mixWithAlpha f fa (PixelRGBA16 ra ga ba aa) (PixelRGBA16 rb gb bb ab) =
        PixelRGBA16 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb) (fa aa ab)

    {-# INLINE colorMap #-}
    colorMap f (PixelRGBA16 r g b a) = PixelRGBA16 (f r) (f g) (f b) (f a)

    {-# INLINE componentCount #-}
    componentCount _ = 4

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y =
                PixelRGBA16 (arr ! (baseIdx + 0)) (arr ! (baseIdx + 1))
                            (arr ! (baseIdx + 2)) (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr `M.read` baseIdx
        gv <- arr `M.read` (baseIdx + 1)
        bv <- arr `M.read` (baseIdx + 2)
        av <- arr `M.read` (baseIdx + 3)
        return $ PixelRGBA16 rv gv bv av
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGBA16 rv gv bv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv
        (arr `M.write` (baseIdx + 3)) av

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelRGBA16 (V.unsafeIndex v idx)
                    (V.unsafeIndex v $ idx + 1)
                    (V.unsafeIndex v $ idx + 2)
                    (V.unsafeIndex v $ idx + 3)
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelRGBA16 `liftM` M.unsafeRead vec idx
                    `ap` M.unsafeRead vec (idx + 1)
                    `ap` M.unsafeRead vec (idx + 2)
                    `ap` M.unsafeRead vec (idx + 3)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelRGBA16 r g b a) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b
                              >> M.unsafeWrite v (idx + 3) a


instance TransparentPixel PixelRGBA16 PixelRGB16 where
    {-# INLINE dropTransparency #-}
    dropTransparency (PixelRGBA16 r g b _) = PixelRGB16 r g b

instance ColorPlane PixelRGBA16 PlaneRed where
    toComponentIndex _ _ = 0

instance ColorPlane PixelRGBA16 PlaneGreen where
    toComponentIndex _ _ = 1

instance ColorPlane PixelRGBA16 PlaneBlue where
    toComponentIndex _ _ = 2

instance ColorPlane PixelRGBA16 PlaneAlpha where
    toComponentIndex _ _ = 3

--------------------------------------------------
----            PixelYCbCr8 instances
--------------------------------------------------
instance Pixel PixelYCbCr8 where
    type PixelBaseComponent PixelYCbCr8 = Word8

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f (PixelYCbCr8 ya cba cra) (PixelYCbCr8 yb cbb crb) =
        PixelYCbCr8 (f 0 ya yb) (f 1 cba cbb) (f 2 cra crb)

    {-# INLINE colorMap #-}
    colorMap f (PixelYCbCr8 y cb cr) = PixelYCbCr8 (f y) (f cb) (f cr)
    {-# INLINE componentCount #-}
    componentCount _ = 3
    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y = PixelYCbCr8 (arr ! (baseIdx + 0))
                                                                (arr ! (baseIdx + 1))
                                                                (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        yv <- arr `M.read` baseIdx
        cbv <- arr `M.read` (baseIdx + 1)
        crv <- arr `M.read` (baseIdx + 2)
        return $ PixelYCbCr8 yv cbv crv
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelYCbCr8 yv cbv crv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) yv
        (arr `M.write` (baseIdx + 1)) cbv
        (arr `M.write` (baseIdx + 2)) crv

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelYCbCr8 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2)
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelYCbCr8 `liftM` M.unsafeRead vec idx
                    `ap` M.unsafeRead vec (idx + 1)
                    `ap` M.unsafeRead vec (idx + 2)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelYCbCr8 y cb cr) =
        M.unsafeWrite v idx y >> M.unsafeWrite v (idx + 1) cb
                              >> M.unsafeWrite v (idx + 2) cr

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


instance ColorSpaceConvertible PixelRGB8 PixelYCbCr8 where
    {-# INLINE convertPixel #-}
    convertPixel (PixelRGB8 r g b) = PixelYCbCr8 (fromIntegral y) (fromIntegral cb) (fromIntegral cr)
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

instance ColorSpaceConvertible PixelYCbCr8 PixelRGB8 where
    {-# INLINE convertPixel #-}
    convertPixel (PixelYCbCr8 y cb cr) = PixelRGB8 (clampWord8 r) (clampWord8 g) (clampWord8 b)
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

instance ColorPlane PixelYCbCr8 PlaneLuma where
    toComponentIndex _ _ = 0

instance ColorPlane PixelYCbCr8 PlaneCb where
    toComponentIndex _ _ = 1

instance ColorPlane PixelYCbCr8 PlaneCr where
    toComponentIndex _ _ = 2

--------------------------------------------------
----            PixelCMYK8 instances
--------------------------------------------------
instance Pixel PixelCMYK8 where
    type PixelBaseComponent PixelCMYK8 = Word8

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f (PixelCMYK8 ca ma ya ka) (PixelCMYK8 cb mb yb kb) =
        PixelCMYK8 (f 0 ca cb) (f 1 ma mb) (f 2 ya yb) (f 3 ka kb)

    {-# INLINE colorMap #-}
    colorMap f (PixelCMYK8 c m y k) = PixelCMYK8 (f c) (f m) (f y) (f k)

    {-# INLINE componentCount #-}
    componentCount _ = 4

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y = PixelCMYK8 (arr ! (baseIdx + 0))
                                                               (arr ! (baseIdx + 1))
                                                               (arr ! (baseIdx + 2))
                                                               (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr `M.read` baseIdx
        gv <- arr `M.read` (baseIdx + 1)
        bv <- arr `M.read` (baseIdx + 2)
        av <- arr `M.read` (baseIdx + 3)
        return $ PixelCMYK8 rv gv bv av
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelCMYK8 rv gv bv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv
        (arr `M.write` (baseIdx + 3)) av

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelCMYK8 (V.unsafeIndex v idx)
                   (V.unsafeIndex v $ idx + 1)
                   (V.unsafeIndex v $ idx + 2)
                   (V.unsafeIndex v $ idx + 3)

    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelCMYK8 `liftM` M.unsafeRead vec idx
                   `ap` M.unsafeRead vec (idx + 1)
                   `ap` M.unsafeRead vec (idx + 2)
                   `ap` M.unsafeRead vec (idx + 3)

    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelCMYK8 r g b a) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b
                              >> M.unsafeWrite v (idx + 3) a

instance ColorSpaceConvertible PixelCMYK8 PixelRGB8 where
  convertPixel (PixelCMYK8 c m y k) =
      PixelRGB8 (clampWord8 r) (clampWord8 g) (clampWord8 b)
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
instance Pixel PixelYCbCrK8 where
    type PixelBaseComponent PixelYCbCrK8 = Word8

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f (PixelYCbCrK8 ya cba cra ka) (PixelYCbCrK8 yb cbb crb kb) =
        PixelYCbCrK8 (f 0 ya yb) (f 1 cba cbb) (f 2 cra crb) (f 3 ka kb)

    {-# INLINE colorMap #-}
    colorMap f (PixelYCbCrK8 y cb cr k) = PixelYCbCrK8 (f y) (f cb) (f cr) (f k)

    {-# INLINE componentCount #-}
    componentCount _ = 4

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y =
        PixelYCbCrK8 (arr ! (baseIdx + 0)) (arr ! (baseIdx + 1))
                     (arr ! (baseIdx + 2)) (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        yv <- arr `M.read` baseIdx
        cbv <- arr `M.read` (baseIdx + 1)
        crv <- arr `M.read` (baseIdx + 2)
        kv <- arr `M.read` (baseIdx + 3)
        return $ PixelYCbCrK8 yv cbv crv kv
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelYCbCrK8 yv cbv crv kv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) yv
        (arr `M.write` (baseIdx + 1)) cbv
        (arr `M.write` (baseIdx + 2)) crv
        (arr `M.write` (baseIdx + 3)) kv

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelYCbCrK8 (V.unsafeIndex v idx)
                     (V.unsafeIndex v $ idx + 1)
                     (V.unsafeIndex v $ idx + 2)
                     (V.unsafeIndex v $ idx + 3)

    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
      PixelYCbCrK8 `liftM` M.unsafeRead vec idx
                   `ap` M.unsafeRead vec (idx + 1)
                   `ap` M.unsafeRead vec (idx + 2)
                   `ap` M.unsafeRead vec (idx + 3)

    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelYCbCrK8 y cb cr k) =
        M.unsafeWrite v idx y >> M.unsafeWrite v (idx + 1) cb
                              >> M.unsafeWrite v (idx + 2) cr
                              >> M.unsafeWrite v (idx + 3) k

instance ColorSpaceConvertible PixelYCbCrK8 PixelRGB8 where
  convertPixel (PixelYCbCrK8 y cb cr _k) = PixelRGB8 (clamp r) (clamp g) (clamp b)
    where
      tof :: Word8 -> Float
      tof = fromIntegral

      clamp :: Float -> Word8
      clamp = floor . max 0 . min 255

      yf = tof y

      r = yf + 1.402 * tof cr - 179.456
      g = yf - 0.3441363 * tof cb - 0.71413636 * tof cr + 135.4589
      b = yf + 1.772 * tof cb - 226.816

instance ColorSpaceConvertible PixelYCbCrK8 PixelCMYK8 where
  convertPixel (PixelYCbCrK8 y cb cr k) = PixelCMYK8 c m ye k
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

instance ColorSpaceConvertible PixelRGB8 PixelCMYK8 where
  convertPixel (PixelRGB8 r g b) = integralRGBToCMYK PixelCMYK8 (r, g, b)

instance ColorPlane PixelCMYK8 PlaneCyan where
    toComponentIndex _ _ = 0

instance ColorPlane PixelCMYK8 PlaneMagenta where
    toComponentIndex _ _ = 1

instance ColorPlane PixelCMYK8 PlaneYellow where
    toComponentIndex _ _ = 2

instance ColorPlane PixelCMYK8 PlaneBlack where
    toComponentIndex _ _ = 3

--------------------------------------------------
----            PixelCMYK16 instances
--------------------------------------------------
instance Pixel PixelCMYK16 where
    type PixelBaseComponent PixelCMYK16 = Word16

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f (PixelCMYK16 ca ma ya ka) (PixelCMYK16 cb mb yb kb) =
        PixelCMYK16 (f 0 ca cb) (f 1 ma mb) (f 2 ya yb) (f 3 ka kb)

    {-# INLINE colorMap #-}
    colorMap f (PixelCMYK16 c m y k) = PixelCMYK16 (f c) (f m) (f y) (f k)

    {-# INLINE componentCount #-}
    componentCount _ = 4

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y = PixelCMYK16 (arr ! (baseIdx + 0))
                                                               (arr ! (baseIdx + 1))
                                                               (arr ! (baseIdx + 2))
                                                               (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr `M.read` baseIdx
        gv <- arr `M.read` (baseIdx + 1)
        bv <- arr `M.read` (baseIdx + 2)
        av <- arr `M.read` (baseIdx + 3)
        return $ PixelCMYK16 rv gv bv av
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelCMYK16 rv gv bv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv
        (arr `M.write` (baseIdx + 3)) av

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelCMYK16 (V.unsafeIndex v idx)
                   (V.unsafeIndex v $ idx + 1)
                   (V.unsafeIndex v $ idx + 2)
                   (V.unsafeIndex v $ idx + 3)

    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelCMYK16 `liftM` M.unsafeRead vec idx
                   `ap` M.unsafeRead vec (idx + 1)
                   `ap` M.unsafeRead vec (idx + 2)
                   `ap` M.unsafeRead vec (idx + 3)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelCMYK16 r g b a) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b
                              >> M.unsafeWrite v (idx + 3) a

instance ColorSpaceConvertible PixelCMYK16 PixelRGB16 where
  convertPixel (PixelCMYK16 c m y k) =
      PixelRGB16 (clampWord16 r) (clampWord16 g) (clampWord16 b)
    where
          clampWord16 = fromIntegral . (`unsafeShiftR` 16)
          ik :: Int
          ik = 65535 - fromIntegral k

          r = (65535 - fromIntegral c) * ik
          g = (65535 - fromIntegral m) * ik
          b = (65535 - fromIntegral y) * ik

instance ColorPlane PixelCMYK16 PlaneCyan where
    toComponentIndex _ _ = 0

instance ColorPlane PixelCMYK16 PlaneMagenta where
    toComponentIndex _ _ = 1

instance ColorPlane PixelCMYK16 PlaneYellow where
    toComponentIndex _ _ = 2

instance ColorPlane PixelCMYK16 PlaneBlack where
    toComponentIndex _ _ = 3

-- | Perform a gamma correction for an image with HDR pixels.
gammaCorrection :: PixelF          -- ^ Gamma value, should be between 0.5 and 3.0
                -> Image PixelRGBF -- ^ Image to treat.
                -> Image PixelRGBF
gammaCorrection gammaVal = pixelMap gammaCorrector
  where gammaExponent = 1.0 / gammaVal
        fixVal v = v ** gammaExponent
        gammaCorrector (PixelRGBF r g b) =
            PixelRGBF (fixVal r) (fixVal g) (fixVal b)

-- | Perform a tone mapping operation on an High dynamic range image.
toneMapping :: PixelF          -- ^ Exposure parameter
            -> Image PixelRGBF -- ^ Image to treat.
            -> Image PixelRGBF
toneMapping exposure img = Image (imageWidth img) (imageHeight img) scaledData
 where coeff = exposure * (exposure / maxBrightness + 1.0) / (exposure + 1.0);
       maxBrightness = pixelFold (\luma _ _ px -> max luma $ computeLuma px) 0 img
       scaledData = V.map (* coeff) $ imageData img

--------------------------------------------------
----            Packable pixel
--------------------------------------------------
instance PackeablePixel PixelRGBA8 where
    type PackedRepresentation PixelRGBA8 = Word32
    {-# INLINE packPixel #-}
    packPixel (PixelRGBA8 r g b a) =
        (fi r `unsafeShiftL` (0 * bitCount)) .|.
        (fi g `unsafeShiftL` (1 * bitCount)) .|.
        (fi b `unsafeShiftL` (2 * bitCount)) .|.
        (fi a `unsafeShiftL` (3 * bitCount))
      where fi = fromIntegral
            bitCount = 8

    {-# INLINE unpackPixel #-}
    unpackPixel w =
        PixelRGBA8 (low w)
                   (low $ w `unsafeShiftR` bitCount)
                   (low $ w `unsafeShiftR` (2 * bitCount))
                   (low $ w `unsafeShiftR` (3 * bitCount))
      where
        low v = fromIntegral (v .&. 0xFF)
        bitCount = 8

instance PackeablePixel PixelRGBA16 where
    type PackedRepresentation PixelRGBA16 = Word64
    {-# INLINE packPixel #-}
    packPixel (PixelRGBA16 r g b a) =
        (fi r `unsafeShiftL` (0 * bitCount)) .|.
        (fi g `unsafeShiftL` (1 * bitCount)) .|.
        (fi b `unsafeShiftL` (2 * bitCount)) .|.
        (fi a `unsafeShiftL` (3 * bitCount))
      where fi = fromIntegral
            bitCount = 16

    {-# INLINE unpackPixel #-}
    unpackPixel w =
        PixelRGBA16 (low w)
                    (low $ w `unsafeShiftR` bitCount)
                    (low $ w `unsafeShiftR` (2 * bitCount))
                    (low $ w `unsafeShiftR` (3 * bitCount))
      where
        low v = fromIntegral (v .&. 0xFFFF)
        bitCount = 16

instance PackeablePixel PixelCMYK8 where
    type PackedRepresentation PixelCMYK8 = Word32
    {-# INLINE packPixel #-}
    packPixel (PixelCMYK8 c m y k) =
        (fi c `unsafeShiftL` (0 * bitCount)) .|.
        (fi m `unsafeShiftL` (1 * bitCount)) .|.
        (fi y `unsafeShiftL` (2 * bitCount)) .|.
        (fi k `unsafeShiftL` (3 * bitCount))
      where fi = fromIntegral
            bitCount = 8

    {-# INLINE unpackPixel #-}
    unpackPixel w =
        PixelCMYK8 (low w)
                   (low $ w `unsafeShiftR` bitCount)
                   (low $ w `unsafeShiftR` (2 * bitCount))
                   (low $ w `unsafeShiftR` (3 * bitCount))
      where
        low v = fromIntegral (v .&. 0xFF)
        bitCount = 8

instance PackeablePixel PixelCMYK16 where
    type PackedRepresentation PixelCMYK16 = Word64
    {-# INLINE packPixel #-}
    packPixel (PixelCMYK16 c m y k) =
        (fi c `unsafeShiftL` (0 * bitCount)) .|.
        (fi m `unsafeShiftL` (1 * bitCount)) .|.
        (fi y `unsafeShiftL` (2 * bitCount)) .|.
        (fi k `unsafeShiftL` (3 * bitCount))
      where fi = fromIntegral
            bitCount = 16

    {-# INLINE unpackPixel #-}
    unpackPixel w =
        PixelCMYK16 (low w)
                    (low $ w `unsafeShiftR` bitCount)
                    (low $ w `unsafeShiftR` (2 * bitCount))
                    (low $ w `unsafeShiftR` (3 * bitCount))
      where
        low v = fromIntegral (v .&. 0xFFFF)
        bitCount = 16

instance PackeablePixel PixelYA16 where
    type PackedRepresentation PixelYA16 = Word32
    {-# INLINE packPixel #-}
    packPixel (PixelYA16 y a) =
        (fi y `unsafeShiftL` (0 * bitCount)) .|.
        (fi a `unsafeShiftL` (1 * bitCount))
      where fi = fromIntegral
            bitCount = 16

    {-# INLINE unpackPixel #-}
    unpackPixel w = PixelYA16 (low w) (low $ w `unsafeShiftR` bitCount)
      where
        low v = fromIntegral (v .&. 0xFFFF)
        bitCount = 16

instance PackeablePixel PixelYA8 where
    type PackedRepresentation PixelYA8 = Word16
    {-# INLINE packPixel #-}
    packPixel (PixelYA8 y a) =
        (fi y `unsafeShiftL` (0 * bitCount)) .|.
        (fi a `unsafeShiftL` (1 * bitCount))
      where fi = fromIntegral
            bitCount = 8

    {-# INLINE unpackPixel #-}
    unpackPixel w = PixelYA8 (low w) (low $ w `unsafeShiftR` bitCount)
      where
        low v = fromIntegral (v .&. 0xFF)
        bitCount = 8

{-# ANN module "HLint: ignore Reduce duplication" #-}

