{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
module Codec.Picture.BaseTypes ( Image( .. )
                               , MutableImage( .. )
                               , ColorPlane( .. )
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
                               , BasicPixel
                               , BasicComponent
                               , extractComponent
                               , dropAlphaLayer
                               , TransparentPixel( .. )
                               , lineFold
                               , stride
                               , freezeImage
                               , thawImage
                               , unsafeThawImage
                               , unsafeFreezeImage
                               , createMutableImage
                               , newMutableImage
                               , Pixel8
                               , Pixel16
                               , Pixel32
                               , PixelF
                               , Pixel( .. )
                               , ColorSpaceConvertible( .. )
                               , generateImage
                               , withImage
                               , generateFoldImage
                               , pixelFold
                               , pixelFoldM
                               , pixelFoldMap
                               , pixelMap
                               , imagePixels
                               , imageIPixels
                               , pixelMapXY
                               , zipPixelComponent3
                               , LumaPlaneExtractable( .. )
                               , ColorConvertible( .. )
                               , integralRGBToCMYK
                               , PackeablePixel( .. )
                               , fillImageWith
                               , unsafeWritePixelBetweenAt
                               , readPackedPixelAt
                               , writePackedPixelAt
                               , Traversal
                               , unsafeExtractComponent
                               ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( Monoid, mempty )
import Control.Applicative( Applicative, pure, (<*>), (<$>) )
#endif

import Data.Monoid( (<>) )
import Control.Monad( foldM, liftM )
import Control.DeepSeq( NFData( .. ) )
import Control.Monad.ST( ST, runST )
import Control.Monad.Primitive ( PrimMonad, PrimState )
import Foreign.ForeignPtr( castForeignPtr )
import Foreign.Storable ( Storable )
import Data.Word( Word8, Word16, Word32 )
import Data.Vector.Storable ( (!) )
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

-- | The main type of this package, one that most
-- functions work on, is Image.
--
-- Parameterized by the underlying pixel format it
-- forms a rigid type. If you wish to store images
-- of different or unknown pixel formats use 'DynamicImage'.
--
-- Image is essentially a rectangular pixel buffer
-- of specified width and height. The coordinates are
-- assumed to start from the upper-left corner
-- of the image, with the horizontal position first
-- and vertical second.
data Image a = Image
    { -- | Width of the image in pixels
      imageWidth  :: {-# UNPACK #-} !Int
      -- | Height of the image in pixels.
    , imageHeight :: {-# UNPACK #-} !Int

      -- | Image pixel data. To extract pixels at a given position
      -- you should use the helper functions.
      --
      -- Internally pixel data is stored as consecutively packed
      -- lines from top to bottom, scanned from left to right
      -- within individual lines, from first to last color
      -- component within each pixel.
    , imageData   :: V.Vector (PixelBaseComponent a)
    }

type BasicPixel a = PixelBaseComponent a ~ a
type BasicComponent a = BasicPixel (PixelBaseComponent a)

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
-- examples:
--
-- @
--  extractRedPlane :: Image PixelRGB8 -> Image Pixel8
--  extractRedPlane = extractComponent PlaneRed
-- @
--
extractComponent :: forall px plane. ( Pixel px
                               , Pixel (PixelBaseComponent px)
                               , BasicComponent px 
                               , ColorPlane px plane )
                 => plane -> Image px -> Image (PixelBaseComponent px)
extractComponent plane = unsafeExtractComponent idx
    where idx = toComponentIndex (undefined :: px) plane

-- | Extract a plane of an image. Returns the requested color
-- component as a greyscale image.
--
-- If you ask for a component out of bound, the `error` function will
-- be called.
unsafeExtractComponent :: forall a
                        . ( Pixel a
                          , Pixel (PixelBaseComponent a)
                          , BasicComponent a
                          )
                       => Int     -- ^ The component index, beginning at 0 ending at (componentCount - 1)
                       -> Image a -- ^ Source image
                       -> Image (PixelBaseComponent a)
unsafeExtractComponent comp img@(Image { imageWidth = w, imageHeight = h })
  | comp >= padd = error $ "extractComponent : invalid component index ("
                         ++ show comp ++ ", max:" ++ show padd ++ ")"
  | otherwise = Image { imageWidth = w, imageHeight = h, imageData = plane }
      where plane = stride img padd comp
            padd = componentCount (undefined :: a)

-- | For any image with an alpha component (transparency),
-- drop it, returning a pure opaque image.
dropAlphaLayer :: (TransparentPixel a b, Pixel b) => Image a -> Image b
dropAlphaLayer = pixelMap dropTransparency

-- | Class modeling transparent pixel, should provide a method
-- to combine transparent pixels
class (Pixel a) => TransparentPixel a b | a -> b where
    -- | Just return the opaque pixel value
    dropTransparency :: a -> b

    -- | Set alpha component of the pixel
    setOpacity :: (PixelBaseComponent a) -> a -> a


lineFold :: (Monad m) => a -> Int -> (a -> Int -> m a) -> m a
{-# INLINE lineFold #-}
lineFold initial count f = go 0 initial
  where go n acc | n >= count = return acc
        go n acc = f acc n >>= go (n + 1)

stride :: (Storable (PixelBaseComponent a))
       => Image a -> Int -> Int -> V.Vector (PixelBaseComponent a)
stride Image { imageWidth = w, imageHeight = h, imageData = array }
        padd firstComponent = runST $ do
    let cell_count = w * h
    outArray <- M.new cell_count

    let go writeIndex _ | writeIndex >= cell_count = return ()
        go writeIndex readIndex = do
          (outArray `M.unsafeWrite` writeIndex) $ array `V.unsafeIndex` readIndex
          go (writeIndex + 1) $ readIndex + padd

    go 0 firstComponent
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
{-# INLINE createMutableImage #-}
createMutableImage width height background =
   unsafeThawImage $ generateImage (\_ _ -> background) width height

-- | Create a mutable image with garbage as content. All data
-- is uninitialized.
newMutableImage :: forall px m. (Pixel px, PrimMonad m)
                => Int -- ^ Width
                -> Int -- ^ Height
                -> m (MutableImage (PrimState m) px)
newMutableImage w h = MutableImage w h `liftM` M.new (w * h * compCount)
  where compCount = componentCount (undefined :: px)

instance NFData (MutableImage s a) where
    rnf (MutableImage width height dat) = width  `seq`
                                          height `seq`
                                          dat    `seq`
                                          ()

-- | Type alias for 8bit greyscale pixels. For simplicity,
-- greyscale pixels use plain numbers instead of a separate type.
type Pixel8 = Word8

-- | Type alias for 16bit greyscale pixels.
type Pixel16 = Word16

-- | Type alias for 32bit greyscale pixels.
type Pixel32 = Word32

-- | Type alias for 32bit floating point greyscale pixels. The standard
-- bounded value range is mapped to the closed interval [0,1] i.e.
--
-- > map promotePixel [0, 1 .. 255 :: Pixel8] == [0/255, 1/255 .. 1.0 :: PixelF]
type PixelF = Float

-- | Definition of pixels used in images. Each pixel has a color space, and a representative
-- component (Word8 or Float).
class ( Storable (PixelBaseComponent a) ) => Pixel a where
    -- | Type of the pixel component, "classical" images
    -- would have Word8 type as their PixelBaseComponent,
    -- HDR image would have Float for instance
    type PixelBaseComponent a :: *

    -- | Call the function for every component of the pixels.
    -- For example for RGB pixels mixWith is declared like this:
    --
    -- > mixWith f (PixelRGB8 ra ga ba) (PixelRGB8 rb gb bb) =
    -- >    PixelRGB8 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb)
    --
    mixWith :: (Int -> PixelBaseComponent a -> PixelBaseComponent a -> PixelBaseComponent a)
            -> a -> a -> a

    -- | Extension of the `mixWith` which separate the treatment
    -- of the color components of the alpha value (transparency component).
    -- For pixel without alpha components, it is equivalent to mixWith.
    --
    -- > mixWithAlpha f fa (PixelRGBA8 ra ga ba aa) (PixelRGB8 rb gb bb ab) =
    -- >    PixelRGBA8 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb) (fa aa ab)
    --
    mixWithAlpha :: (Int -> PixelBaseComponent a -> PixelBaseComponent a
                         -> PixelBaseComponent a)  -- ^ Function for color component
                 -> (PixelBaseComponent a -> PixelBaseComponent a
                         -> PixelBaseComponent a) -- ^ Function for alpha component
                 -> a -> a -> a
    {-# INLINE mixWithAlpha #-}
    mixWithAlpha f _ = mixWith f

    -- | Opaque pixel with all values empty
    emptyPixel :: a

    -- | Opaque full pixel value in normal display range
    saturatedPixel :: a

    -- | Return the opacity of a pixel, if the pixel has an
    -- alpha layer, return the alpha value. If the pixel
    -- doesn't have an alpha value, return a value
    -- representing the opaqueness.
    pixelOpacity :: a -> PixelBaseComponent a

    -- | Return the number of components of the pixel
    componentCount :: a -> Int

    -- | Apply a function to each component of a pixel.
    -- If the color type possess an alpha (transparency channel),
    -- it is treated like the other color components.
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
    -- operation should never lose any data.
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
-- The function will receive values from 0 to width-1 for the x parameter
-- and 0 to height-1 for the y parameter. The coordinates 0,0 are the upper
-- left corner of the image, and (width-1, height-1) the lower right corner.
--
-- for example, to create a small gradient image:
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
{-# INLINE generateImage #-}
generateImage f w h = Image { imageWidth = w, imageHeight = h, imageData = generated }
  where compCount = componentCount (undefined :: a)
        generated = runST $ do
            arr <- M.new (w * h * compCount)
            let lineGenerator _ !y | y >= h = return ()
                lineGenerator !lineIdx y = column lineIdx 0
                  where column !idx !x | x >= w = lineGenerator idx $ y + 1
                        column idx x = do
                            unsafeWritePixel arr idx $ f x y
                            column (idx + compCount) $ x + 1

            lineGenerator 0 0
            V.unsafeFreeze arr

-- | Create an image using a monadic initializer function.
-- The function will receive values from 0 to width-1 for the x parameter
-- and 0 to height-1 for the y parameter. The coordinates 0,0 are the upper
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
-- The function will receive values from 0 to width-1 for the x parameter
-- and 0 to height-1 for the y parameter. The coordinates 0,0 are the upper
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

-- | Fold over the pixel of an image with a raster scan order:
-- from top to bottom, left to right
{-# INLINE pixelFold #-}
pixelFold :: forall acc pixel. (Pixel pixel)
          => (acc -> Int -> Int -> pixel -> acc) -> acc -> Image pixel -> acc
pixelFold f initialAccumulator img@(Image { imageWidth = w, imageHeight = h }) =
  columnFold 0 initialAccumulator 0
    where
      !compCount = componentCount (undefined :: pixel)
      !vec = imageData img

      lfold !y acc !x !idx
        | x >= w = columnFold (y + 1) acc idx
        | otherwise = 
            lfold y (f acc x y $ unsafePixelAt vec idx) (x + 1) (idx + compCount)

      columnFold !y lineAcc !readIdx
        | y >= h = lineAcc
        | otherwise = lfold y lineAcc 0 readIdx

-- | Fold over the pixel of an image with a raster scan order:
-- from top to bottom, left to right, carrying out a state
pixelFoldM :: (Pixel pixel, Monad m)
           => (acc -> Int -> Int -> pixel -> m acc) -- ^ monadic mapping function
           -> acc                              -- ^ Initial state
           -> Image pixel                       -- ^ Image to fold over
           -> m acc
{-# INLINE pixelFoldM  #-}
pixelFoldM action initialAccumulator img@(Image { imageWidth = w, imageHeight = h }) =
  lineFold initialAccumulator h columnFold
    where
      pixelFolder y acc x = action acc x y $ pixelAt img x y
      columnFold lineAcc y = lineFold lineAcc w (pixelFolder y)


-- | Fold over the pixel of an image with a raster scan order:
-- from top to bottom, left to right. This functions is analog
-- to the foldMap from the 'Foldable' typeclass, but due to the
-- Pixel constraint, Image cannot be made an instance of it.
pixelFoldMap :: forall m px. (Pixel px, Monoid m) => (px -> m) -> Image px -> m
pixelFoldMap f Image { imageWidth = w, imageHeight = h, imageData = vec } = folder 0
  where
    compCount = componentCount (undefined :: px)
    maxi = w * h * compCount

    folder idx | idx >= maxi = mempty
    folder idx = f (unsafePixelAt vec idx) <> folder (idx + compCount)

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


-- | Helpers to embed a rankNTypes inside an Applicative
newtype GenST a = GenST { genAction :: forall s. ST s (M.STVector s a) }

-- | Traversal type matching the definition in the Lens package.
type Traversal s t a b =
    forall f. Applicative f => (a -> f b) -> s -> f t 

writePx :: Pixel px
        => Int -> GenST (PixelBaseComponent px) -> px -> GenST (PixelBaseComponent px)
{-# INLINE writePx #-}
writePx idx act px = GenST $ do
   vec <- genAction act
   unsafeWritePixel vec idx px
   return vec

freezeGenST :: Pixel px
            => Int -> Int -> GenST (PixelBaseComponent px) -> Image px
freezeGenST w h act =
  Image w h (runST (genAction act >>= V.unsafeFreeze))

-- | Traversal in "raster" order, from left to right the top to bottom.
-- This traversal is matching pixelMap in spirit.
--
-- Since 3.2.4
imagePixels :: forall pxa pxb. (Pixel pxa, Pixel pxb)
            => Traversal (Image pxa) (Image pxb) pxa pxb
{-# INLINE imagePixels #-}
imagePixels f Image { imageWidth = w, imageHeight = h, imageData = vec } =
  freezeGenST w h <$> pixels
  where
    sourceComponentCount = componentCount (undefined :: pxa)
    destComponentCount = componentCount (undefined :: pxb)

    maxi = w * h * sourceComponentCount
    pixels =
      go (pure $ GenST $ M.new (w * h * destComponentCount)) 0 0

    go act readIdx _ | readIdx >= maxi = act
    go act readIdx writeIdx =
      go newAct (readIdx + sourceComponentCount) (writeIdx + destComponentCount)
      where
        px = f (unsafePixelAt vec readIdx)
        newAct = writePx writeIdx <$> act <*> px

-- | Traversal providing the pixel position with it's value.
-- The traversal in raster order, from lef to right, then top
-- to bottom. The traversal match pixelMapXY in spirit.
--
-- Since 3.2.4
imageIPixels :: forall pxa pxb. (Pixel pxa, Pixel pxb)
             => Traversal (Image pxa) (Image pxb) (Int, Int, pxa) pxb
{-# INLINE imageIPixels #-}
imageIPixels f Image { imageWidth = w, imageHeight = h, imageData = vec } =
  freezeGenST w h <$> pixels
  where
    sourceComponentCount = componentCount (undefined :: pxa)
    destComponentCount = componentCount (undefined :: pxb)

    pixels =
      lineMapper (pure $ GenST $ M.new (w * h * destComponentCount)) 0 0 0

    lineMapper act _ _ y | y >= h = act
    lineMapper act readIdxLine writeIdxLine y =
        go act readIdxLine writeIdxLine 0
      where
        go cact readIdx writeIdx x
          | x >= w = lineMapper cact readIdx writeIdx $ y + 1
          | otherwise = do
             let px = f (x, y, unsafePixelAt vec readIdx)
             go (writePx writeIdx <$> cact <*> px)
                (readIdx + sourceComponentCount)
                (writeIdx + destComponentCount)
                (x + 1)

-- | Just like `pixelMap` only the function takes the pixel coordinates as
--   additional parameters.
pixelMapXY :: forall a b. (Pixel a, Pixel b)
           => (Int -> Int -> a -> b) -> Image a -> Image b
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
    computeLuma      :: a -> PixelBaseComponent a

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

-- | Free promotion for identic pixel types
instance (Pixel a) => ColorConvertible a a where
    {-# INLINE promotePixel #-}
    promotePixel = id

    {-# INLINE promoteImage #-}
    promoteImage = id

--------------------------------------------------
----            Pixel8 instances
--------------------------------------------------
instance Pixel Pixel8 where
    type PixelBaseComponent Pixel8 = Word8

    {-# INLINE emptyPixel #-}
    emptyPixel = 0

    {-# INLINE saturatedPixel #-}
    saturatedPixel = 0xFF

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f = f 0

    {-# INLINE colorMap #-}
    colorMap f = f

    {-# INLINE componentCount #-}
    componentCount _ = 1

    {-# INLINE pixelAt #-}
    pixelAt (Image { imageWidth = w, imageData = arr }) x y = arr ! (x + y * w)

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.read` mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.write` mutablePixelBaseIndex image x y

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt = V.unsafeIndex
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel = M.unsafeRead
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel = M.unsafeWrite

instance ColorConvertible Pixel8 PixelF where
    {-# INLINE promotePixel #-}
    promotePixel c = fromIntegral c / 255.0

instance ColorConvertible Pixel8 Pixel16 where
    {-# INLINE promotePixel #-}
    promotePixel c = fromIntegral c * 257

--------------------------------------------------
----            Pixel16 instances
--------------------------------------------------
instance Pixel Pixel16 where
    type PixelBaseComponent Pixel16 = Word16

    {-# INLINE emptyPixel #-}
    emptyPixel = 0

    {-# INLINE saturatedPixel #-}
    saturatedPixel = 0xFFFF

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f = f 0

    {-# INLINE colorMap #-}
    colorMap f = f

    {-# INLINE componentCount #-}
    componentCount _ = 1
    {-# INLINE pixelAt #-}
    pixelAt (Image { imageWidth = w, imageData = arr }) x y = arr ! (x + y * w)

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.read` mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.write` mutablePixelBaseIndex image x y

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt = V.unsafeIndex
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel = M.unsafeRead
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel = M.unsafeWrite

--------------------------------------------------
----            Pixel32 instances
--------------------------------------------------
instance Pixel Pixel32 where
    type PixelBaseComponent Pixel32 = Word32

    {-# INLINE emptyPixel #-}
    emptyPixel = 0

    {-# INLINE saturatedPixel #-}
    saturatedPixel = 0xFFFFFFFF

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f = f 0

    {-# INLINE colorMap #-}
    colorMap f = f

    {-# INLINE componentCount #-}
    componentCount _ = 1

    {-# INLINE pixelAt #-}
    pixelAt (Image { imageWidth = w, imageData = arr }) x y = arr ! (x + y * w)

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.read` mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.write` mutablePixelBaseIndex image x y

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt = V.unsafeIndex
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel = M.unsafeRead
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel = M.unsafeWrite

--------------------------------------------------
----            PixelF instances
--------------------------------------------------
instance Pixel PixelF where
    type PixelBaseComponent PixelF = Float

    {-# INLINE emptyPixel #-}
    emptyPixel = 0

    {-# INLINE saturatedPixel #-}
    saturatedPixel = 1

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const 1.0

    {-# INLINE mixWith #-}
    mixWith f = f 0

    {-# INLINE colorMap #-}
    colorMap f = f
    {-# INLINE componentCount #-}
    componentCount _ = 1
    {-# INLINE pixelAt #-}
    pixelAt (Image { imageWidth = w, imageData = arr }) x y =
        arr ! (x + y * w)

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.read` mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.write` mutablePixelBaseIndex image x y

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt = V.unsafeIndex
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel = M.unsafeRead
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel = M.unsafeWrite

instance (Pixel a) => ColorSpaceConvertible a a where
    convertPixel = id
    convertImage = id

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

          clamp = fromIntegral . max 0

--------------------------------------------------
----            Packable pixel
--------------------------------------------------

-- | This typeclass exist for performance reason, it allow
-- to pack a pixel value to a simpler "primitive" data
-- type to allow faster writing to moemory.
class PackeablePixel a where
    -- | Primitive type asociated to the current pixel
    -- It's Word32 for PixelRGBA8 for instance
    type PackedRepresentation a

    -- | The packing function, allowing to transform
    -- to a primitive.
    packPixel :: a -> PackedRepresentation a

    -- | Inverse transformation, to speed up
    -- reading
    unpackPixel :: PackedRepresentation a -> a

instance PackeablePixel Pixel8 where
    type PackedRepresentation Pixel8 = Pixel8
    packPixel = id
    {-# INLINE packPixel #-}
    unpackPixel = id
    {-# INLINE unpackPixel #-}

instance PackeablePixel Pixel16 where
    type PackedRepresentation Pixel16 = Pixel16
    packPixel = id
    {-# INLINE packPixel #-}
    unpackPixel = id
    {-# INLINE unpackPixel #-}

instance PackeablePixel Pixel32 where
    type PackedRepresentation Pixel32 = Pixel32
    packPixel = id
    {-# INLINE packPixel #-}
    unpackPixel = id
    {-# INLINE unpackPixel #-}

instance PackeablePixel PixelF where
    type PackedRepresentation PixelF = PixelF
    packPixel = id
    {-# INLINE packPixel #-}
    unpackPixel = id
    {-# INLINE unpackPixel #-}


-- | This function will fill an image with a simple packeable
-- pixel. It will be faster than any unsafeWritePixel.
fillImageWith :: ( Pixel px, PackeablePixel px
                 , PrimMonad m
                 , M.Storable (PackedRepresentation px))
              => MutableImage (PrimState m) px -> px -> m ()
fillImageWith img px = M.set converted $ packPixel px
  where
    (ptr, s, s2) = M.unsafeToForeignPtr $ mutableImageData img
    !packedPtr = castForeignPtr ptr
    !converted =
        M.unsafeFromForeignPtr packedPtr s (s2 `div` componentCount px)

-- | Fill a packeable pixel between two bounds.
unsafeWritePixelBetweenAt
    :: ( PrimMonad m
       , Pixel px, PackeablePixel px
       , M.Storable (PackedRepresentation px))
    => MutableImage (PrimState m) px -- ^ Image to write into
    -> px                -- ^ Pixel to write
    -> Int               -- ^ Start index in pixel base component
    -> Int               -- ^ pixel count of pixel to write
    -> m ()
unsafeWritePixelBetweenAt img px start count = M.set converted packed
  where
    !packed = packPixel px
    !pixelData = mutableImageData img

    !toSet = M.slice start count pixelData
    (ptr, s, s2) = M.unsafeToForeignPtr toSet
    !packedPtr = castForeignPtr ptr
    !converted =
        M.unsafeFromForeignPtr packedPtr s s2

-- | Read a packeable pixel from an image. Equivalent to
-- unsafeReadPixel
readPackedPixelAt :: forall m px.
                     ( Pixel px, PackeablePixel px
                     , M.Storable (PackedRepresentation px)
                     , PrimMonad m
                     )
                  => MutableImage (PrimState m) px -- ^ Image to read from
                  -> Int  -- ^ Index in (PixelBaseComponent px) count
                  -> m px
{-# INLINE readPackedPixelAt #-}
readPackedPixelAt img idx = do
    unpacked <- M.unsafeRead converted (idx `div` compCount)
    return $ unpackPixel unpacked
    where
    !compCount = componentCount (undefined :: px)
    (ptr, s, s2) = M.unsafeToForeignPtr $ mutableImageData img
    !packedPtr = castForeignPtr ptr
    !converted =
        M.unsafeFromForeignPtr packedPtr s s2


-- | Write a packeable pixel into an image. equivalent to unsafeWritePixel.
writePackedPixelAt :: ( Pixel px, PackeablePixel px
                      , M.Storable (PackedRepresentation px)
                      , PrimMonad m
                      )
                   => MutableImage (PrimState m) px -- ^ Image to write into
                   -> Int  -- ^ Index in (PixelBaseComponent px) count
                   -> px   -- ^ Pixel to write
                   -> m ()
{-# INLINE writePackedPixelAt #-}
writePackedPixelAt img idx px =
    M.unsafeWrite converted (idx `div` compCount) packed
  where
    !packed = packPixel px
    !compCount = componentCount px

    (ptr, s, s2) = M.unsafeToForeignPtr $ mutableImageData img
    !packedPtr = castForeignPtr ptr
    !converted =
        M.unsafeFromForeignPtr packedPtr s s2

{-# ANN module "HLint: ignore Reduce duplication" #-}

