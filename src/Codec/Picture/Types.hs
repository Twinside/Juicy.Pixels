{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
-- | Module providing the basic types for image manipulation in the library.
-- Defining the types used to store all those _Juicy Pixels_
module Codec.Picture.Types( -- * Types
                            -- ** Image types
                            Image( .. )
                          , MutableImage( .. )
                          , DynamicImage( .. )
                          , Palette

                            -- ** Image functions
                          , createMutableImage
                          , freezeImage
                          , unsafeFreezeImage
                          , thawImage
                          , unsafeThawImage

                            -- ** Pixel types
                          , Pixel8
                          , Pixel16
                          , Pixel32
                          , PixelF
                          , PixelYA8( .. )
                          , PixelYA16( .. )
                          , PixelRGB8( .. )
                          , PixelRGB16( .. )
                          , PixelRGBF( .. )
                          , PixelRGBA8( .. )
                          , PixelRGBA16( .. )
                          , PixelCMYK8( .. )
                          , PixelCMYK16( .. )
                          , PixelYCbCr8( .. )

                          -- * Type classes
                          , ColorConvertible( .. )
                          , Pixel(..)
                          -- $graph
                          , ColorSpaceConvertible( .. )
                          , LumaPlaneExtractable( .. )
                          , TransparentPixel( .. )

                            -- * Helper functions
                          , pixelMap
                          , pixelMapXY
                          , pixelFold
                          , dynamicMap
                          , dynamicPixelMap
                          , dropAlphaLayer
                          , withImage
                          , zipPixelComponent3
                          , generateImage
                          , generateFoldImage
                          , gammaCorrection
                          , toneMapping

                            -- * Color plane extraction
                          , ColorPlane ( )

                          , PlaneRed( .. )
                          , PlaneGreen( .. )
                          , PlaneBlue( .. )
                          , PlaneAlpha( .. )
                          , PlaneLuma( .. )
                          , PlaneCr( .. )
                          , PlaneCb( .. )
                          , PlaneCyan( .. )
                          , PlaneMagenta( .. )
                          , PlaneYellow( .. )
                          , PlaneBlack( .. )

                          , extractComponent
                          , unsafeExtractComponent
                          ) where

import Control.Monad( forM_, foldM, liftM, ap )
import Control.DeepSeq( NFData( .. ) )
import Control.Monad.ST( runST )
import Control.Monad.Primitive ( PrimMonad, PrimState )
import Foreign.Storable ( Storable )
import Data.Bits( unsafeShiftL, unsafeShiftR )
import Data.Word( Word8, Word16, Word32 )
import Data.List( foldl' )
import Data.Vector.Storable ( (!) )
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

#include "ConvGraph.hs"

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
    , imageData   :: V.Vector (PixelBaseComponent a)
    }

-- | Type for the palette used in Gif & PNG files.
type Palette = Image PixelRGB8

{-# INLINE (!!!) #-}
(!!!) :: (Storable e) => V.Vector e -> Int -> e
(!!!) = V.unsafeIndex

-- | Class used to describle plane present in the pixel
-- type. If a pixel has a plane description associated,
-- you can use the plane name to extract planes independently.
class ColorPlane pixel planeToken where
    -- | Retrieve the index of the component in the
    -- given pixel type.
    toComponentIndex :: pixel -> planeToken -> Int

-- | Define the plane for the red color component
data PlaneRed = PlaneRed

-- | Define the plane for the green color component
data PlaneGreen = PlaneGreen

-- | Define the plane for the blue color component
data PlaneBlue = PlaneBlue

-- | Define the plane for the alpha (transparency) component
data PlaneAlpha = PlaneAlpha

-- | Define the plane for the luma component
data PlaneLuma = PlaneLuma

-- | Define the plane for the Cr component
data PlaneCr = PlaneCr

-- | Define the plane for the Cb component
data PlaneCb = PlaneCb

-- | Define plane for the cyan component of the
-- CMYK color space.
data PlaneCyan = PlaneCyan

-- | Define plane for the magenta component of the
-- CMYK color space.
data PlaneMagenta = PlaneMagenta

-- | Define plane for the yellow component of the
-- CMYK color space.
data PlaneYellow = PlaneYellow

-- | Define plane for the black component of
-- the CMYK color space.
data PlaneBlack = PlaneBlack

-- | Extract a color plane from an image given a present plane in the image
-- examples :
--
-- @
--  extractRedPlane :: Image PixelRGB8-> Image Pixel8
--  extractRedPlane = extractComponent PlaneRed
-- @
--
extractComponent :: forall px plane. ( Pixel px
                                     , Pixel (PixelBaseComponent px)
                                     , PixelBaseComponent (PixelBaseComponent px)
                                                    ~ PixelBaseComponent px
                                     , ColorPlane px plane )
                 => plane -> Image px -> Image (PixelBaseComponent px)
extractComponent plane = unsafeExtractComponent idx
    where idx = toComponentIndex (undefined :: px) plane

-- | Extract an image plane of an image, returning an image which
-- can be represented by a gray scale image.
-- If you ask a component out of bound, the `error` function will
-- be called
unsafeExtractComponent :: forall a
                        . ( Pixel a
                          , Pixel (PixelBaseComponent a)
                          , PixelBaseComponent (PixelBaseComponent a)
                                              ~ PixelBaseComponent a)
                       => Int     -- ^ The component index, beginning at 0 ending at (componentCount - 1)
                       -> Image a -- ^ Source image
                       -> Image (PixelBaseComponent a)
unsafeExtractComponent comp img@(Image { imageWidth = w, imageHeight = h })
  | comp >= padd = error $ "extractComponent : invalid component index ("
                         ++ show comp ++ ", max:" ++ show padd ++ ")"
  | otherwise = Image { imageWidth = w, imageHeight = h, imageData = plane }
      where plane = stride img 1 padd comp
            padd = componentCount (undefined :: a)

-- | For any image with an alpha component (transparency),
-- drop it, returning a pure opaque image.
dropAlphaLayer :: (TransparentPixel a b) => Image a -> Image b
dropAlphaLayer = pixelMap dropTransparency

-- | Class modeling transparent pixel, should provide a method
-- to combine transparent pixels
class (Pixel a, Pixel b) => TransparentPixel a b | a -> b where
    -- | Just return the opaque pixel value
    dropTransparency :: a -> b

    -- | access the transparency (alpha layer) of a given
    -- transparent pixel type.
    -- DEPRECATED, you should use `pixelOpacity`
    getTransparency :: a -> PixelBaseComponent a

instance TransparentPixel PixelRGBA8 PixelRGB8 where
    {-# INLINE dropTransparency #-}
    dropTransparency (PixelRGBA8 r g b _) = PixelRGB8 r g b
    {-# INLINE getTransparency #-}
    getTransparency (PixelRGBA8 _ _ _ a) = a

stride :: (Storable (PixelBaseComponent a))
       => Image a -> Int -> Int -> Int -> V.Vector (PixelBaseComponent a)
stride Image { imageWidth = w, imageHeight = h, imageData = array }
        run padd firstComponent = runST $ do
    let cell_count = w * h * run
    outArray <- M.new cell_count

    let strideWrite write_idx _ | write_idx == cell_count = return ()
        strideWrite write_idx read_idx = do
            forM_ [0 .. run - 1] $ \i ->
                (outArray .<-. (write_idx + i)) $ array !!! (read_idx + i)
            strideWrite (write_idx + run) (read_idx + padd)

    strideWrite 0 firstComponent
    V.unsafeFreeze outArray

instance NFData (Image a) where
    rnf (Image width height dat) = width  `seq`
                                   height `seq`
                                   dat    `seq`
                                   ()

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
    , mutableImageData   :: M.STVector s (PixelBaseComponent a)
    }

-- | `O(n)` Yield an immutable copy of an image by making a copy of it
freezeImage :: (Storable (PixelBaseComponent px), PrimMonad m)
            => MutableImage (PrimState m) px -> m (Image px)
freezeImage (MutableImage w h d) = Image w h `liftM` V.freeze d

-- | `O(n)` Yield a mutable copy of an image by making a copy of it.
thawImage :: (Storable (PixelBaseComponent px), PrimMonad m)
          => Image px -> m (MutableImage (PrimState m) px)
thawImage (Image w h d) = MutableImage w h `liftM` V.thaw d

-- | `O(1)` Unsafe convert an imutable image to an mutable one without copying.
-- The source image shouldn't be used after this operation.
unsafeThawImage :: (Storable (PixelBaseComponent px), PrimMonad m)
                => Image px -> m (MutableImage (PrimState m) px)
unsafeThawImage (Image w h d) = MutableImage w h `liftM` V.unsafeThaw d

-- | `O(1)` Unsafe convert a mutable image to an immutable one without copying.
-- The mutable image may not be used after this operation.
unsafeFreezeImage ::  (Storable (PixelBaseComponent a), PrimMonad m)
                  => MutableImage (PrimState m) a -> m (Image a)
unsafeFreezeImage (MutableImage w h d) = Image w h `liftM` V.unsafeFreeze d

-- | Create a mutable image, filled with the given background color.
createMutableImage :: (Pixel px, PrimMonad m)
                   => Int -- ^ Width
                   -> Int -- ^ Height
                   -> px  -- ^ Background color
                   -> m (MutableImage (PrimState m) px)
createMutableImage width height background =
   unsafeThawImage $ generateImage (\_ _ -> background) width height

instance NFData (MutableImage s a) where
    rnf (MutableImage width height dat) = width  `seq`
                                          height `seq`
                                          dat    `seq`
                                          ()

-- | Type allowing the loading of an image with different pixel
-- structures
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
       -- | An image in the colorspace CMYK and 16 bots precision
     | ImageCMYK16 (Image PixelCMYK16)

-- | Helper function to help extract information from dynamic
-- image. To get the width of a dynamic image, you can use
-- the following snippet :
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
-- > dynSquare = dynMap squareImage
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

-- | Simple alias for greyscale value in 8 bits.
type Pixel8 = Word8

-- | Simple alias for greyscale value in 16 bits.
type Pixel16 = Word16

-- | Simple alias for greyscale value in 16 bits.
type Pixel32 = Word32

-- | Floating greyscale value, the 0 to 255 8 bit range maps
-- to 0 to 1 in this floating version
type PixelF = Float

-- | Pixel type storing Luminance (Y) and alpha information
-- on 8 bits.
-- Value are stored in the following order :
--
--  * Luminance
--
--  * Alpha
--
data PixelYA8 = PixelYA8 {-# UNPACK #-} !Pixel8  -- Luminance
                         {-# UNPACK #-} !Pixel8  -- Alpha value
              deriving (Eq, Ord, Show)

-- | Pixel type storing Luminance (Y) and alpha information
-- on 16 bits.
-- Value are stored in the following order :
--
--  * Luminance
--
--  * Alpha
--
data PixelYA16 = PixelYA16 {-# UNPACK #-} !Pixel16  -- Luminance
                           {-# UNPACK #-} !Pixel16  -- Alpha value
              deriving (Eq, Ord, Show)

-- | Pixel type storing classic pixel on 8 bits
-- Value are stored in the following order :
--
--  * Red
--
--  * Green
--
--  * Blue
--
data PixelRGB8 = PixelRGB8 {-# UNPACK #-} !Pixel8 -- Red
                           {-# UNPACK #-} !Pixel8 -- Green
                           {-# UNPACK #-} !Pixel8 -- Blue
               deriving (Eq, Ord, Show)

-- | Pixel type storing pixels on 16 bits
-- Value are stored in the following order :
--
--  * Red
--
--  * Green
--
--  * Blue
--
data PixelRGB16 = PixelRGB16 {-# UNPACK #-} !Pixel16 -- Red
                             {-# UNPACK #-} !Pixel16 -- Green
                             {-# UNPACK #-} !Pixel16 -- Blue
               deriving (Eq, Ord, Show)

-- | Pixel type storing HDR pixel on 32 bits float
-- Values are stored in the following order :
--
--  * Red
--
--  * Green
--
--  * Blue
--
data PixelRGBF = PixelRGBF {-# UNPACK #-} !PixelF -- Red
                           {-# UNPACK #-} !PixelF -- Green
                           {-# UNPACK #-} !PixelF -- Blue
               deriving (Eq, Ord, Show)

-- | Pixel storing data in the YCbCr colorspace,
-- values are stored in the following order :
--
--  * Y (luminance)
--
--  * Cr
--
--  * Cb
--
data PixelYCbCr8 = PixelYCbCr8 {-# UNPACK #-} !Pixel8 -- Y luminance
                               {-# UNPACK #-} !Pixel8 -- Cr red difference
                               {-# UNPACK #-} !Pixel8 -- Cb blue difference
                 deriving (Eq, Ord, Show)

-- | Pixel storing data in the CMYK colorspace. Values
-- are stored in the following order :
--
--   * Cyan
--
--   * Magenta
--
--   * Yellow
--
--   * Black
--
data PixelCMYK8 = PixelCMYK8 {-# UNPACK #-} !Pixel8 -- Cyan
                             {-# UNPACK #-} !Pixel8 -- Magenta
                             {-# UNPACK #-} !Pixel8 -- Yellow
                             {-# UNPACK #-} !Pixel8 -- Black
                 deriving (Eq, Ord, Show)

-- | Pixel storing data in the CMYK colorspace. Values
-- are stored in the following order :
--
--   * Cyan
--
--   * Magenta
--
--   * Yellow
--
--   * Black
--
data PixelCMYK16 = PixelCMYK16 {-# UNPACK #-} !Pixel16 -- Cyan
                               {-# UNPACK #-} !Pixel16 -- Magenta
                               {-# UNPACK #-} !Pixel16 -- Yellow
                               {-# UNPACK #-} !Pixel16 -- Black
                 deriving (Eq, Ord, Show)


-- | Pixel type storing a classic pixel, with an alpha component.
-- Values are stored in the following order
--
--  * Red
--
--  * Green
--
--  * Blue
--
--  * Alpha
--
data PixelRGBA8 = PixelRGBA8 {-# UNPACK #-} !Pixel8 -- Red
                             {-# UNPACK #-} !Pixel8 -- Green
                             {-# UNPACK #-} !Pixel8 -- Blue
                             {-# UNPACK #-} !Pixel8 -- Alpha
                deriving (Eq, Ord, Show)

-- | Pixel type storing a RGB information with an alpha
-- channel on 16 bits.
-- Values are stored in the following order
--
--  * Red
--
--  * Green
--
--  * Blue
--
--  * Alpha
--
data PixelRGBA16 = PixelRGBA16 {-# UNPACK #-} !Pixel16 -- Red
                               {-# UNPACK #-} !Pixel16 -- Green
                               {-# UNPACK #-} !Pixel16 -- Blue
                               {-# UNPACK #-} !Pixel16 -- Alpha
                deriving (Eq, Ord, Show)

-- | Definition of pixels used in images. Each pixel has a color space, and a representative
-- component (Word8 or Float).
class ( Storable (PixelBaseComponent a)
      , Num (PixelBaseComponent a), Eq a ) => Pixel a where
    -- | Type of the pixel component, "classical" images
    -- would have Word8 type as their PixelBaseComponent,
    -- HDR image would have Float for instance
    type PixelBaseComponent a :: *

    -- | Call the function for every component of the pixels.
    -- For example for RGB pixels mixWith is declared like this :
    --
    -- > mixWith f (PixelRGB8 ra ga ba) (PixelRGB8 rb gb bb) =
    -- >    PixelRGB8 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb)
    --
    mixWith :: (Int -> PixelBaseComponent a -> PixelBaseComponent a -> PixelBaseComponent a)
            -> a -> a -> a

    -- | Return the opacity of a pixel, if the pixel has an
    -- alpha layer, return the alpha value. If the pixel
    -- doesn't have an alpha value, return a value
    -- representing the opaqueness.
    pixelOpacity :: a -> PixelBaseComponent a

    -- | Return the number of component of the pixel
    componentCount :: a -> Int

    -- | Apply a function to all color component of a pixel.
    colorMap :: (PixelBaseComponent a -> PixelBaseComponent a) -> a -> a

    -- | Calculate the index for the begining of the pixel
    pixelBaseIndex :: Image a -> Int -> Int -> Int
    pixelBaseIndex (Image { imageWidth = w }) x y =
            (x + y * w) * componentCount (undefined :: a)

    -- | Calculate theindex for the begining of the pixel at position x y
    mutablePixelBaseIndex :: MutableImage s a -> Int -> Int -> Int
    mutablePixelBaseIndex (MutableImage { mutableImageWidth = w }) x y =
            (x + y * w) * componentCount (undefined :: a)

    -- | Extract a pixel at a given position, (x, y), the origin
    -- is assumed to be at the corner top left, positive y to the
    -- bottom of the image
    pixelAt :: Image a -> Int -> Int -> a

    -- | Same as pixelAt but for mutable images.
    readPixel :: PrimMonad m => MutableImage (PrimState m) a -> Int -> Int -> m a

    -- | Write a pixel in a mutable image at position x y
    writePixel :: PrimMonad m => MutableImage (PrimState m) a -> Int -> Int -> a -> m ()

    -- | Unsafe version of pixelAt, read a pixel at the given
    -- index without bound checking (if possible).
    -- The index is expressed in number (PixelBaseComponent a)
    unsafePixelAt :: V.Vector (PixelBaseComponent a) -> Int -> a

    -- | Unsafe version of readPixel,  read a pixel at the given
    -- position without bound checking (if possible). The index
    -- is expressed in number (PixelBaseComponent a)
    unsafeReadPixel :: PrimMonad m => M.STVector (PrimState m) (PixelBaseComponent a) -> Int -> m a

    -- | Unsafe version of writePixel, write a pixel at the
    -- given position without bound checking. This can be _really_ unsafe.
    -- The index is expressed in number (PixelBaseComponent a)
    unsafeWritePixel :: PrimMonad m => M.STVector (PrimState m) (PixelBaseComponent a) -> Int -> a -> m ()


-- | Implement upcasting for pixel types.
-- Minimal declaration of `promotePixel`.
-- It is strongly recommended to overload promoteImage to keep
-- performance acceptable
class (Pixel a, Pixel b) => ColorConvertible a b where
    -- | Convert a pixel type to another pixel type. This
    -- operation should never loss any data.
    promotePixel :: a -> b

    -- | Change the underlying pixel type of an image by performing a full copy
    -- of it.
    promoteImage :: Image a -> Image b
    promoteImage = pixelMap promotePixel

-- | This class abstract colorspace conversion. This
-- conversion can be lossy, which ColorConvertible cannot
class (Pixel a, Pixel b) => ColorSpaceConvertible a b where
    -- | Pass a pixel from a colorspace (say RGB) to the second one
    -- (say YCbCr)
    convertPixel :: a -> b

    -- | Helper function to convert a whole image by taking a
    -- copy it.
    convertImage :: Image a -> Image b
    convertImage = pixelMap convertPixel

-- | Create an image given a function to generate pixels.
-- The function will receive value from 0 to width-1 for the x parameter
-- and 0 to height-1 for the y parameter. The coordinate 0,0 is the upper
-- left corner of the image, and (width-1, height-1) the lower right corner.
--
-- for example, to create a small gradient image :
--
-- > imageCreator :: String -> IO ()
-- > imageCreator path = writePng path $ generateImage pixelRenderer 250 300
-- >    where pixelRenderer x y = PixelRGB8 x y 128
--
generateImage :: forall a. (Pixel a)
              => (Int -> Int -> a)  -- ^ Generating function, with `x` and `y` params.
              -> Int        -- ^ Width in pixels
              -> Int        -- ^ Height in pixels
              -> Image a
generateImage f w h = Image { imageWidth = w, imageHeight = h, imageData = generated }
  where compCount = componentCount (undefined :: a)
        generated = runST $ do
            arr <- M.new (w * h * compCount)
            let lineGenerator _ y | y >= h = return ()
                lineGenerator lineIdx y = column lineIdx 0
                  where column idx x | x >= w = lineGenerator idx $ y + 1
                        column idx x = do
                            unsafeWritePixel arr idx $ f x y
                            column (idx + compCount) $ x + 1

            lineGenerator 0 0
            V.unsafeFreeze arr

-- | Create an image using a monadic initializer function.
-- The function will receive value from 0 to width-1 for the x parameter
-- and 0 to height-1 for the y parameter. The coordinate 0,0 is the upper
-- left corner of the image, and (width-1, height-1) the lower right corner.
--
-- The function is called for each pixel in the line from left to right (0 to width - 1)
-- and for each line (0 to height - 1).
withImage :: forall m pixel. (Pixel pixel, PrimMonad m)
          => Int                     -- ^ Image width
          -> Int                     -- ^ Image height
          -> (Int -> Int -> m pixel) -- ^ Generating functions
          -> m (Image pixel)
withImage width height pixelGenerator = do
  let pixelComponentCount = componentCount (undefined :: pixel)
  arr <- M.new (width * height * pixelComponentCount)
  let mutImage = MutableImage
        { mutableImageWidth = width
        , mutableImageHeight = height
        , mutableImageData = arr
        }

  let pixelPositions = [(x, y) | y <- [0 .. height-1], x <- [0..width-1]]
  sequence_ [pixelGenerator x y >>= unsafeWritePixel arr idx
                        | ((x,y), idx) <- zip pixelPositions [0, pixelComponentCount ..]]
  unsafeFreezeImage mutImage

-- | Create an image given a function to generate pixels.
-- The function will receive value from 0 to width-1 for the x parameter
-- and 0 to height-1 for the y parameter. The coordinate 0,0 is the upper
-- left corner of the image, and (width-1, height-1) the lower right corner.
--
-- the acc parameter is a user defined one.
--
-- The function is called for each pixel in the line from left to right (0 to width - 1)
-- and for each line (0 to height - 1).
generateFoldImage :: forall a acc. (Pixel a)
                  => (acc -> Int -> Int -> (acc, a)) -- ^ Function taking the state, x and y
                  -> acc        -- ^ Initial state
                  -> Int        -- ^ Width in pixels
                  -> Int        -- ^ Height in pixels
                  -> (acc, Image a)
generateFoldImage f intialAcc w h =
 (finalState, Image { imageWidth = w, imageHeight = h, imageData = generated })
  where compCount = componentCount (undefined :: a)
        (finalState, generated) = runST $ do
            arr <- M.new (w * h * compCount)
            let mutImage = MutableImage {
                                mutableImageWidth = w,
                                mutableImageHeight = h,
                                mutableImageData = arr }
            foldResult <- foldM (\acc (x,y) -> do
                    let (acc', px) = f acc x y
                    writePixel mutImage x y px
                    return acc') intialAcc [(x,y) | y <- [0 .. h-1], x <- [0 .. w-1]]

            frozen <- V.unsafeFreeze arr
            return (foldResult, frozen)

-- | Fold over the pixel of an image with a raster scan order :
-- from top to bottom, left to right
{-# INLINE pixelFold #-}
pixelFold :: (Pixel pixel)
          => (acc -> Int -> Int -> pixel -> acc) -> acc -> Image pixel -> acc
pixelFold f initialAccumulator img@(Image { imageWidth = w, imageHeight = h }) =
  lineFold
    where pixelFolder y acc x = f acc x y $ pixelAt img x y
          columnFold lineAcc y = foldl' (pixelFolder y) lineAcc [0 .. w - 1]
          lineFold = foldl' columnFold initialAccumulator [0 .. h - 1]

-- | `map` equivalent for an image, working at the pixel level.
-- Little example : a brightness function for an rgb image
--
-- > brightnessRGB8 :: Int -> Image PixelRGB8 -> Image PixelRGB8
-- > brightnessRGB8 add = pixelMap brightFunction
-- >      where up v = fromIntegral (fromIntegral v + add)
-- >            brightFunction (PixelRGB8 r g b) =
-- >                    PixelRGB8 (up r) (up g) (up b)
--
pixelMap :: forall a b. (Pixel a, Pixel b)
         => (a -> b) -> Image a -> Image b
{-# RULES "pixelMap fusion" forall g f. pixelMap g . pixelMap f = pixelMap (g . f) #-}
{-# SPECIALIZE INLINE pixelMap :: (PixelYCbCr8 -> PixelRGB8) -> Image PixelYCbCr8 -> Image PixelRGB8 #-}
{-# SPECIALIZE INLINE pixelMap :: (PixelRGB8 -> PixelYCbCr8) -> Image PixelRGB8 -> Image PixelYCbCr8 #-}
{-# SPECIALIZE INLINE pixelMap :: (PixelRGB8 -> PixelRGB8) -> Image PixelRGB8 -> Image PixelRGB8 #-}
{-# SPECIALIZE INLINE pixelMap :: (PixelRGB8 -> PixelRGBA8) -> Image PixelRGB8 -> Image PixelRGBA8 #-}
{-# SPECIALIZE INLINE pixelMap :: (PixelRGBA8 -> PixelRGBA8) -> Image PixelRGBA8 -> Image PixelRGBA8 #-}
{-# SPECIALIZE INLINE pixelMap :: (Pixel8 -> PixelRGB8) -> Image Pixel8 -> Image PixelRGB8 #-}
pixelMap f Image { imageWidth = w, imageHeight = h, imageData = vec } =
  Image w h pixels
    where sourceComponentCount = componentCount (undefined :: a)
          destComponentCount = componentCount (undefined :: b)

          pixels = runST $ do
            newArr <- M.new (w * h * destComponentCount)
            let lineMapper _ _ y | y >= h = return ()
                lineMapper readIdxLine writeIdxLine y = colMapper readIdxLine writeIdxLine 0
                  where colMapper readIdx writeIdx x
                            | x >= w = lineMapper readIdx writeIdx $ y + 1
                            | otherwise = do
                                unsafeWritePixel newArr writeIdx . f $ unsafePixelAt vec readIdx
                                colMapper (readIdx + sourceComponentCount)
                                          (writeIdx + destComponentCount)
                                          (x + 1)
            lineMapper 0 0 0

            -- unsafeFreeze avoids making a second copy and it will be
            -- safe because newArray can't be referenced as a mutable array
            -- outside of this where block
            V.unsafeFreeze newArr

-- | Just like `pixelMap` only the function takes the pixel coordinates as
--   additional parameters.
pixelMapXY :: forall a b. (Pixel a, Pixel b)
           => (Int -> Int -> a -> b) -> Image a -> Image b
{-# SPECIALIZE INLINE pixelMapXY :: (Int -> Int -> PixelYCbCr8 -> PixelRGB8)
                                 -> Image PixelYCbCr8 -> Image PixelRGB8 #-}
{-# SPECIALIZE INLINE pixelMapXY :: (Int -> Int -> PixelRGB8 -> PixelYCbCr8)
                                 -> Image PixelRGB8 -> Image PixelYCbCr8 #-}
{-# SPECIALIZE INLINE pixelMapXY :: (Int -> Int -> PixelRGB8 -> PixelRGB8)
                                 -> Image PixelRGB8 -> Image PixelRGB8 #-}
{-# SPECIALIZE INLINE pixelMapXY :: (Int -> Int -> PixelRGB8 -> PixelRGBA8)
                                 -> Image PixelRGB8 -> Image PixelRGBA8 #-}
{-# SPECIALIZE INLINE pixelMapXY :: (Int -> Int -> PixelRGBA8 -> PixelRGBA8)
                                 -> Image PixelRGBA8 -> Image PixelRGBA8 #-}
{-# SPECIALIZE INLINE pixelMapXY :: (Int -> Int -> Pixel8 -> PixelRGB8)
                                 -> Image Pixel8 -> Image PixelRGB8 #-}
pixelMapXY f Image { imageWidth = w, imageHeight = h, imageData = vec } =
  Image w h pixels
    where sourceComponentCount = componentCount (undefined :: a)
          destComponentCount = componentCount (undefined :: b)

          pixels = runST $ do
            newArr <- M.new (w * h * destComponentCount)
            let lineMapper _ _ y | y >= h = return ()
                lineMapper readIdxLine writeIdxLine y = colMapper readIdxLine writeIdxLine 0
                  where colMapper readIdx writeIdx x
                            | x >= w = lineMapper readIdx writeIdx $ y + 1
                            | otherwise = do
                                unsafeWritePixel newArr writeIdx . f x y $ unsafePixelAt vec readIdx
                                colMapper (readIdx + sourceComponentCount)
                                          (writeIdx + destComponentCount)
                                          (x + 1)
            lineMapper 0 0 0

            -- unsafeFreeze avoids making a second copy and it will be
            -- safe because newArray can't be referenced as a mutable array
            -- outside of this where block
            V.unsafeFreeze newArr

-- | Combine, pixel by pixel and component by component
-- the values of 3 different images. Usage example:
--
-- > averageBrightNess c1 c2 c3 = clamp $ toInt c1 + toInt c2 + toInt c3
-- >   where clamp = fromIntegral . min 0 . max 255
-- >         toInt :: a -> Int
-- >         toInt = fromIntegral
-- > ziPixelComponent3 averageBrightNess img1 img2 img3
--
zipPixelComponent3
    :: forall px. ( V.Storable (PixelBaseComponent px))
    => (PixelBaseComponent px -> PixelBaseComponent px -> PixelBaseComponent px
            -> PixelBaseComponent px)
    -> Image px -> Image px -> Image px -> Image px
{-# INLINE zipPixelComponent3 #-}
zipPixelComponent3 f i1@(Image { imageWidth = w, imageHeight = h }) i2 i3
  | not isDimensionEqual = error "Different image size zipPairwisePixelComponent"
  | otherwise = Image { imageWidth = w
                      , imageHeight = h
                      , imageData = V.zipWith3 f data1 data2 data3
                      }
       where data1 = imageData i1
             data2 = imageData i2
             data3 = imageData i3

             isDimensionEqual =
                 w == imageWidth i2 && w == imageWidth i3 &&
                     h == imageHeight i2 && h == imageHeight i3

-- | Helper class to help extract a luma plane out
-- of an image or a pixel
class (Pixel a, Pixel (PixelBaseComponent a)) => LumaPlaneExtractable a where
    -- | Compute the luminance part of a pixel
    computeLuma      :: a -> (PixelBaseComponent a)

    -- | Extract a luma plane out of an image. This
    -- method is in the typeclass to help performant
    -- implementation.
    --
    -- > jpegToGrayScale :: FilePath -> FilePath -> IO ()
    -- > jpegToGrayScale source dest
    extractLumaPlane :: Image a -> Image (PixelBaseComponent a)
    extractLumaPlane = pixelMap computeLuma

instance LumaPlaneExtractable Pixel8 where
    {-# INLINE computeLuma #-}
    computeLuma = id
    extractLumaPlane = id

instance LumaPlaneExtractable Pixel16 where
    {-# INLINE computeLuma #-}
    computeLuma = id
    extractLumaPlane = id

instance LumaPlaneExtractable Pixel32 where
    {-# INLINE computeLuma #-}
    computeLuma = id
    extractLumaPlane = id

instance LumaPlaneExtractable PixelF where
    {-# INLINE computeLuma #-}
    computeLuma = id
    extractLumaPlane = id

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
    type PixelBaseComponent Pixel8 = Word8

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f = f 0

    {-# INLINE colorMap #-}
    colorMap f = f

    componentCount _ = 1
    pixelAt (Image { imageWidth = w, imageData = arr }) x y = arr ! (x + y * w)

    readPixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.read` mutablePixelBaseIndex image x y

    writePixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.write` mutablePixelBaseIndex image x y

    unsafePixelAt = V.unsafeIndex
    unsafeReadPixel = M.unsafeRead
    unsafeWritePixel = M.unsafeWrite

instance ColorConvertible Pixel8 PixelYA8 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelYA8 c 255

instance ColorConvertible Pixel8 PixelF where
    {-# INLINE promotePixel #-}
    promotePixel c = fromIntegral c / 255.0

instance ColorConvertible Pixel8 Pixel16 where
    {-# INLINE promotePixel #-}
    promotePixel c = fromIntegral c `unsafeShiftL` 8

instance ColorConvertible Pixel8 PixelRGB8 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelRGB8 c c c

instance ColorConvertible Pixel8 PixelRGBA8 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelRGBA8 c c c 255

--------------------------------------------------
----            Pixel16 instances
--------------------------------------------------
instance Pixel Pixel16 where
    type PixelBaseComponent Pixel16 = Word16

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f = f 0

    {-# INLINE colorMap #-}
    colorMap f = f

    componentCount _ = 1
    pixelAt (Image { imageWidth = w, imageData = arr }) x y = arr ! (x + y * w)

    readPixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.read` mutablePixelBaseIndex image x y

    writePixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.write` mutablePixelBaseIndex image x y

    unsafePixelAt = V.unsafeIndex
    unsafeReadPixel = M.unsafeRead
    unsafeWritePixel = M.unsafeWrite

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
instance Pixel Pixel32 where
    type PixelBaseComponent Pixel32 = Word32

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f = f 0

    {-# INLINE colorMap #-}
    colorMap f = f

    componentCount _ = 1
    pixelAt (Image { imageWidth = w, imageData = arr }) x y = arr ! (x + y * w)

    readPixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.read` mutablePixelBaseIndex image x y

    writePixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.write` mutablePixelBaseIndex image x y

    unsafePixelAt = V.unsafeIndex
    unsafeReadPixel = M.unsafeRead
    unsafeWritePixel = M.unsafeWrite

--------------------------------------------------
----            PixelF instances
--------------------------------------------------
instance Pixel PixelF where
    type PixelBaseComponent PixelF = Float

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const 1.0

    {-# INLINE mixWith #-}
    mixWith f = f 0

    {-# INLINE colorMap #-}
    colorMap f = f
    componentCount _ = 1
    pixelAt (Image { imageWidth = w, imageData = arr }) x y = arr ! (x + y * w)

    readPixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.read` mutablePixelBaseIndex image x y

    writePixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.write` mutablePixelBaseIndex image x y

    unsafePixelAt = V.unsafeIndex
    unsafeReadPixel = M.unsafeRead
    unsafeWritePixel = M.unsafeWrite

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

    unsafePixelAt v idx =
        PixelYA8 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1)
    unsafeReadPixel vec idx =
        PixelYA8 `liftM` M.unsafeRead vec idx `ap` M.unsafeRead vec (idx + 1)
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
    {-# INLINE getTransparency #-}
    getTransparency (PixelYA8 _ a) = a

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

    {-# INLINE colorMap #-}
    colorMap f (PixelYA16 y a) = PixelYA16 (f y) (f a)
    componentCount _ = 2
    pixelAt image@(Image { imageData = arr }) x y = PixelYA16 (arr ! (baseIdx + 0))
                                                              (arr ! (baseIdx + 1))
        where baseIdx = pixelBaseIndex image x y

    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        yv <- arr .!!!. baseIdx
        av <- arr .!!!. (baseIdx + 1)
        return $ PixelYA16 yv av
        where baseIdx = mutablePixelBaseIndex image x y

    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelYA16 yv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr .<-. (baseIdx + 0)) yv
        (arr .<-. (baseIdx + 1)) av

    unsafePixelAt v idx =
        PixelYA16 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1)
    unsafeReadPixel vec idx =
        PixelYA16 `liftM` M.unsafeRead vec idx `ap` M.unsafeRead vec (idx + 1)
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
    {-# INLINE getTransparency #-}
    getTransparency (PixelYA16 _ a) = a

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

    componentCount _ = 3

    pixelAt image@(Image { imageData = arr }) x y = PixelRGBF (arr ! (baseIdx + 0))
                                                              (arr ! (baseIdx + 1))
                                                              (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr .!!!. baseIdx
        gv <- arr .!!!. (baseIdx + 1)
        bv <- arr .!!!. (baseIdx + 2)
        return $ PixelRGBF rv gv bv
        where baseIdx = mutablePixelBaseIndex image x y

    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGBF rv gv bv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr .<-. (baseIdx + 0)) rv
        (arr .<-. (baseIdx + 1)) gv
        (arr .<-. (baseIdx + 2)) bv

    unsafePixelAt v idx =
        PixelRGBF (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2)
    unsafeReadPixel vec idx =
        PixelRGBF `liftM` M.unsafeRead vec idx
                  `ap` M.unsafeRead vec (idx + 1)
                  `ap` M.unsafeRead vec (idx + 2)
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

    componentCount _ = 3

    pixelAt image@(Image { imageData = arr }) x y = PixelRGB16 (arr ! (baseIdx + 0))
                                                               (arr ! (baseIdx + 1))
                                                               (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr .!!!. baseIdx
        gv <- arr .!!!. (baseIdx + 1)
        bv <- arr .!!!. (baseIdx + 2)
        return $ PixelRGB16 rv gv bv
        where baseIdx = mutablePixelBaseIndex image x y

    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGB16 rv gv bv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr .<-. (baseIdx + 0)) rv
        (arr .<-. (baseIdx + 1)) gv
        (arr .<-. (baseIdx + 2)) bv

    unsafePixelAt v idx =
        PixelRGB16 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2)
    unsafeReadPixel vec idx =
        PixelRGB16 `liftM` M.unsafeRead vec idx
                   `ap` M.unsafeRead vec (idx + 1)
                   `ap` M.unsafeRead vec (idx + 2)
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

    componentCount _ = 3

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

    unsafePixelAt v idx =
        PixelRGB8 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2)
    unsafeReadPixel vec idx =
        PixelRGB8 `liftM` M.unsafeRead vec idx
                  `ap` M.unsafeRead vec (idx + 1)
                  `ap` M.unsafeRead vec (idx + 2)
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

    {-# INLINE colorMap #-}
    colorMap f (PixelRGBA8 r g b a) = PixelRGBA8 (f r) (f g) (f b) (f a)

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

    unsafePixelAt v idx =
        PixelRGBA8 (V.unsafeIndex v idx)
                   (V.unsafeIndex v $ idx + 1)
                   (V.unsafeIndex v $ idx + 2)
                   (V.unsafeIndex v $ idx + 3)
    unsafeReadPixel vec idx =
        PixelRGBA8 `liftM` M.unsafeRead vec idx
                   `ap` M.unsafeRead vec (idx + 1)
                   `ap` M.unsafeRead vec (idx + 2)
                   `ap` M.unsafeRead vec (idx + 3)
    unsafeWritePixel v idx (PixelRGBA8 r g b a) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b
                              >> M.unsafeWrite v (idx + 3) a

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

    {-# INLINE colorMap #-}
    colorMap f (PixelRGBA16 r g b a) = PixelRGBA16 (f r) (f g) (f b) (f a)

    componentCount _ = 4

    pixelAt image@(Image { imageData = arr }) x y =
                PixelRGBA16 (arr ! (baseIdx + 0)) (arr ! (baseIdx + 1))
                            (arr ! (baseIdx + 2)) (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr .!!!. baseIdx
        gv <- arr .!!!. (baseIdx + 1)
        bv <- arr .!!!. (baseIdx + 2)
        av <- arr .!!!. (baseIdx + 3)
        return $ PixelRGBA16 rv gv bv av
        where baseIdx = mutablePixelBaseIndex image x y

    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGBA16 rv gv bv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr .<-. (baseIdx + 0)) rv
        (arr .<-. (baseIdx + 1)) gv
        (arr .<-. (baseIdx + 2)) bv
        (arr .<-. (baseIdx + 3)) av

    unsafePixelAt v idx =
        PixelRGBA16 (V.unsafeIndex v idx)
                    (V.unsafeIndex v $ idx + 1)
                    (V.unsafeIndex v $ idx + 2)
                    (V.unsafeIndex v $ idx + 3)
    unsafeReadPixel vec idx =
        PixelRGBA16 `liftM` M.unsafeRead vec idx
                    `ap` M.unsafeRead vec (idx + 1)
                    `ap` M.unsafeRead vec (idx + 2)
                    `ap` M.unsafeRead vec (idx + 3)
    unsafeWritePixel v idx (PixelRGBA16 r g b a) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b
                              >> M.unsafeWrite v (idx + 3) a


instance TransparentPixel PixelRGBA16 PixelRGB16 where
    {-# INLINE dropTransparency #-}
    dropTransparency (PixelRGBA16 r g b _) = PixelRGB16 r g b
    {-# INLINE getTransparency #-}
    getTransparency (PixelRGBA16 _ _ _ a) = a

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

    unsafePixelAt v idx =
        PixelYCbCr8 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2)
    unsafeReadPixel vec idx =
        PixelYCbCr8 `liftM` M.unsafeRead vec idx
                    `ap` M.unsafeRead vec (idx + 1)
                    `ap` M.unsafeRead vec (idx + 2)
    unsafeWritePixel v idx (PixelYCbCr8 y cb cr) =
        M.unsafeWrite v idx y >> M.unsafeWrite v (idx + 1) cb
                              >> M.unsafeWrite v (idx + 2) cr

instance (Pixel a) => ColorSpaceConvertible a a where
    convertPixel = id
    convertImage = id

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
              rCb = (- fix 0.16874)
              gCb = (- fix 0.33126)
              bCb = fix 0.5
              gCr = (- fix 0.41869)
              bCr = (- fix 0.08131)

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

    componentCount _ = 4

    pixelAt image@(Image { imageData = arr }) x y = PixelCMYK8 (arr ! (baseIdx + 0))
                                                               (arr ! (baseIdx + 1))
                                                               (arr ! (baseIdx + 2))
                                                               (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr .!!!. baseIdx
        gv <- arr .!!!. (baseIdx + 1)
        bv <- arr .!!!. (baseIdx + 2)
        av <- arr .!!!. (baseIdx + 3)
        return $ PixelCMYK8 rv gv bv av
        where baseIdx = mutablePixelBaseIndex image x y

    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelCMYK8 rv gv bv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr .<-. (baseIdx + 0)) rv
        (arr .<-. (baseIdx + 1)) gv
        (arr .<-. (baseIdx + 2)) bv
        (arr .<-. (baseIdx + 3)) av

    unsafePixelAt v idx =
        PixelCMYK8 (V.unsafeIndex v idx)
                   (V.unsafeIndex v $ idx + 1)
                   (V.unsafeIndex v $ idx + 2)
                   (V.unsafeIndex v $ idx + 3)
    unsafeReadPixel vec idx =
        PixelCMYK8 `liftM` M.unsafeRead vec idx
                   `ap` M.unsafeRead vec (idx + 1)
                   `ap` M.unsafeRead vec (idx + 2)
                   `ap` M.unsafeRead vec (idx + 3)
    unsafeWritePixel v idx (PixelCMYK8 r g b a) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b
                              >> M.unsafeWrite v (idx + 3) a

instance ColorSpaceConvertible PixelCMYK8 PixelRGB8 where
  convertPixel (PixelCMYK8 c m y k) =
      PixelRGB8 (clampWord8 r) (clampWord8 g) (clampWord8 b)
    where
          clampWord8 = fromIntegral . (`unsafeShiftR` 8)
          ik :: Int
          ik = 255 - fromIntegral k

          r = (255 - fromIntegral c) * ik
          g = (255 - fromIntegral m) * ik
          b = (255 - fromIntegral y) * ik


{-# SPECIALIZE integralRGBToCMYK :: (Word8 -> Word8 -> Word8 -> Word8 -> b)
                                 -> (Word8, Word8, Word8) -> b #-}
{-# SPECIALIZE integralRGBToCMYK :: (Word16 -> Word16 -> Word16 -> Word16 -> b)
                                 -> (Word16, Word16, Word16) -> b #-}
integralRGBToCMYK :: (Bounded a, Integral a)
                  => (a -> a -> a -> a -> b)    -- ^ Pixel building function
                  -> (a, a, a)                  -- ^ RGB sample
                  -> b                          -- ^ Resulting sample
integralRGBToCMYK build (r, g, b) =
  build (clamp c) (clamp m) (clamp y) (fromIntegral kInt)
    where maxi = maxBound

          ir = fromIntegral $ maxi - r :: Int
          ig = fromIntegral $ maxi - g
          ib = fromIntegral $ maxi - b

          kInt = minimum [ir, ig, ib]
          ik = fromIntegral maxi - kInt

          c = (ir - kInt) `div` ik
          m = (ig - kInt) `div` ik
          y = (ib - kInt) `div` ik

          clamp = fromIntegral . (max 0)

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

    componentCount _ = 4

    pixelAt image@(Image { imageData = arr }) x y = PixelCMYK16 (arr ! (baseIdx + 0))
                                                               (arr ! (baseIdx + 1))
                                                               (arr ! (baseIdx + 2))
                                                               (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr .!!!. baseIdx
        gv <- arr .!!!. (baseIdx + 1)
        bv <- arr .!!!. (baseIdx + 2)
        av <- arr .!!!. (baseIdx + 3)
        return $ PixelCMYK16 rv gv bv av
        where baseIdx = mutablePixelBaseIndex image x y

    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelCMYK16 rv gv bv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr .<-. (baseIdx + 0)) rv
        (arr .<-. (baseIdx + 1)) gv
        (arr .<-. (baseIdx + 2)) bv
        (arr .<-. (baseIdx + 3)) av

    unsafePixelAt v idx =
        PixelCMYK16 (V.unsafeIndex v idx)
                   (V.unsafeIndex v $ idx + 1)
                   (V.unsafeIndex v $ idx + 2)
                   (V.unsafeIndex v $ idx + 3)
    unsafeReadPixel vec idx =
        PixelCMYK16 `liftM` M.unsafeRead vec idx
                   `ap` M.unsafeRead vec (idx + 1)
                   `ap` M.unsafeRead vec (idx + 2)
                   `ap` M.unsafeRead vec (idx + 3)
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
