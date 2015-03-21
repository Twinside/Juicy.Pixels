{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
-- | This module provide some color quantisation algorithm
-- in order to help in the creation of paletted images.
-- The most important function is `palettize` which will
-- make everything to create a nice color indexed image
-- with its palette.
module Codec.Picture.ColorQuant
    ( palettize
    , defaultPaletteOptions
    , PaletteCreationMethod(..)
    , PaletteOptions( .. )
    ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative (Applicative (..), (<$>))
#endif

import           Data.Bits           (unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import           Data.List           (elemIndex)
import           Data.Maybe          (fromMaybe)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Word           (Word32)

import           Data.Vector         (Vector, (!))
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import           Codec.Picture.Types

-------------------------------------------------------------------------------
----            Palette Creation and Dithering
-------------------------------------------------------------------------------

-- | Define which palette creation method is used.
data PaletteCreationMethod =
      -- | MedianMeanCut method, provide the best results (visualy)
      -- at the cost of increased calculations.
      MedianMeanCut
      -- | Very fast algorithm (one pass), doesn't provide good
      -- looking results.
    | Uniform

-- | To specify how the palette will be created.
data PaletteOptions = PaletteOptions
    { -- | Algorithm used to find the palette
      paletteCreationMethod :: PaletteCreationMethod

      -- | Do we want to apply the dithering to the
      -- image. Enabling it often reduce compression
      -- ratio but enhance the perceived quality
      -- of the final image.
    , enableImageDithering  :: Bool

      -- | Maximum number of color we want in the
      -- palette
    , paletteColorCount     :: Int
    }

-- | Default palette option, which aim at the best quality
-- and maximum possible colors (256)
defaultPaletteOptions :: PaletteOptions
defaultPaletteOptions = PaletteOptions
    { paletteCreationMethod = MedianMeanCut
    , enableImageDithering  = True
    , paletteColorCount     = 256
    }

-- | Reduces an image to a color palette according to `PaletteOpts` and
--   returns the /indices image/ along with its `Palette`.
palettize :: PaletteOptions -> Image PixelRGB8 -> (Image Pixel8, Palette)
palettize opts@PaletteOptions { paletteCreationMethod = method } =
  case method of
    MedianMeanCut -> medianMeanCutQuantization opts
    Uniform       -> uniformQuantization opts

-- | Modified median cut algorithm with optional ordered dithering. Returns an
-- image of `Pixel8` that acts as a matrix of indices into the `Palette`.
medianMeanCutQuantization :: PaletteOptions -> Image PixelRGB8
                          -> (Image Pixel8, Palette)
medianMeanCutQuantization opts img
  | isBelow =
      (pixelMap okPaletteIndex img, vecToPalette okPaletteVec)
  | enableImageDithering opts = (pixelMap paletteIndex dImg, palette)
  | otherwise = (pixelMap paletteIndex img, palette)
  where
    maxColorCount = paletteColorCount opts
    (okPalette, isBelow) = isColorCountBelow maxColorCount img
    okPaletteVec = V.fromList $ Set.toList okPalette
    okPaletteIndex p = nearestColorIdx p okPaletteVec

    palette = vecToPalette paletteVec
    paletteIndex p = nearestColorIdx p paletteVec
    paletteVec = mkPaletteVec cs
    cs =  Set.toList . clusters maxColorCount $ img
    dImg = pixelMapXY dither img

-- | A naive one pass Color Quantiation algorithm - Uniform Quantization.
-- Simply take the most significant bits. The maxCols parameter is rounded
-- down to the nearest power of 2, and the bits are divided among the three
-- color channels with priority order green, red, blue. Returns an
-- image of `Pixel8` that acts as a matrix of indices into the `Palette`.
uniformQuantization :: PaletteOptions -> Image PixelRGB8 -> (Image Pixel8, Palette)
uniformQuantization opts img
  -- -| colorCount img <= maxCols = colorQuantExact img
  | enableImageDithering opts =
        (pixelMap paletteIndex (pixelMapXY dither img), palette)
  | otherwise = (pixelMap paletteIndex img, palette)
  where
    maxCols = paletteColorCount opts
    palette = listToPalette paletteList
    paletteList = [PixelRGB8 r g b | r <- [0,dr..255]
                                   , g <- [0,dg..255]
                                   , b <- [0,db..255]]
    (bg, br, bb) = bitDiv3 maxCols
    (dr, dg, db) = (2^(8-br), 2^(8-bg), 2^(8-bb))
    paletteIndex (PixelRGB8 r g b) = fromIntegral $ fromMaybe 0 (elemIndex
      (PixelRGB8 (r .&. (255 - dr)) (g .&. (255 - dg)) (b .&. (255 - db)))
      paletteList)

isColorCountBelow :: Int -> Image PixelRGB8 -> (Set.Set PixelRGB8, Bool)
isColorCountBelow maxColorCount img = go 0 Set.empty
  where rawData = imageData img
        maxIndex = VS.length rawData
        
        go !idx !allColors
            | Set.size allColors > maxColorCount = (Set.empty, False)
            | idx >= maxIndex - 2 = (allColors, True)
            | otherwise = go (idx + 3) $ Set.insert px allColors
                where px = unsafePixelAt rawData idx 

vecToPalette :: Vector PixelRGB8 -> Palette
vecToPalette ps = generateImage (\x _ -> ps ! x) (V.length ps) 1

listToPalette :: [PixelRGB8] -> Palette
listToPalette ps = generateImage (\x _ -> ps !! x) (length ps) 1

bitDiv3 :: Int -> (Int, Int, Int)
bitDiv3 n = case r of
            0 -> (q, q, q)
            1 -> (q+1, q, q)
            _ -> (q+1, q+1, q)
  where
    r = m `mod` 3
    q = m `div` 3
    m = floor . logBase (2 :: Double) $ fromIntegral n

-------------------------------------------------------------------------------
----            Dithering
-------------------------------------------------------------------------------

-- Add a dither mask to an image for ordered dithering.
-- Uses a small, spatially stable dithering algorithm based on magic numbers
-- and arithmetic inspired by the /a dither/ algorithm of Øyvind Kolås,
-- pippin@gimp.org, 2013. See, http://pippin.gimp.org/a_dither/.
dither :: Int -> Int -> PixelRGB8 -> PixelRGB8
dither x y (PixelRGB8 r g b) = PixelRGB8 (fromIntegral r')
                                         (fromIntegral g')
                                         (fromIntegral b')
  where
    -- Should view 16 as a parameter that can be optimized for best looking
    -- results
    r' = min 255 (fromIntegral r + (x' + y') .&. 16)
    g' = min 255 (fromIntegral g + (x' + y' + 7973) .&. 16)
    b' = min 255 (fromIntegral b + (x' + y' + 15946) .&. 16)
    x' = 119 * x
    y' = 28084 * y

-------------------------------------------------------------------------------
----            Small modification of foldl package by Gabriel Gonzalez
-------------------------------------------------------------------------------

-- Modification to Control.foldl by Gabriel Gonzalez copyright 2013, BSD3.
-- http://hackage.haskell.org/package/foldl-1.0.1/docs/Control-Foldl.html

{-| Efficient representation of a left fold that preserves the fold's step
    function, initial accumulator, and extraction function

    This allows the 'Applicative' instance to assemble derived folds that
    traverse the container only once
-}
data Fold a b = forall x . Fold (x -> a -> x) x (x -> b)

{-| Apply a strict left 'Fold' to a 'Foldable' container

    Much slower than 'fold' on lists because 'Foldable' operations currently do
    not trigger @build/foldr@ fusion
-}
fold :: Fold PackedRGB b -> VU.Vector PackedRGB -> b
fold (Fold step begin done) = done . VU.foldl' step begin
{-# INLINE fold #-}

{-
F.foldr :: (a -> b -> b) -> b -> t a -> b

fold :: (Foldable f) => Fold a b -> f a -> b
fold (Fold step begin done) as = F.foldr step' done as begin
  where step' x k z = k $! step z x
-}

data Pair a b = Pair !a !b

instance Functor (Fold a) where
    fmap f (Fold step begin done) = Fold step begin (f . done)
    {-# INLINABLE fmap #-}

instance Applicative (Fold a) where
    pure b    = Fold (\() _ -> ()) () (\() -> b)
    {-# INLINABLE pure #-}
    (Fold stepL beginL doneL) <*> (Fold stepR beginR doneR) =
        let step (Pair xL xR) a = Pair (stepL xL a) (stepR xR a)
            begin = Pair beginL beginR
            done (Pair xL xR) = doneL xL $ doneR xR
        in  Fold step begin done
    {-# INLINABLE (<*>) #-}

{- | Like 'length', except with a more general 'Num' return value -}
intLength :: Fold a Int
intLength = Fold (\n _ -> n + 1) 0 id

-------------------------------------------------------------------------------
----            Modified Median Cut Algorithm
-------------------------------------------------------------------------------

-- Based on the OCaml implementation:
-- http://rosettacode.org/wiki/Color_quantization
-- which is in turn based on: www.leptonica.com/papers/mediancut.pdf.
-- We use the product of volume and population to determine the next cluster
-- to split and determine the placement of each color by compating it to the
-- mean of the parent cluster. So median cut is a bit of a misnomer, since one
-- of the modifiations is to use the mean.

mkPaletteVec :: [Cluster] -> Vector PixelRGB8
mkPaletteVec  = V.fromList . map (toRGB8 . meanColor)

type PackedRGB = Word32

data Cluster = Cluster
    { value       :: {-# UNPACK #-} !Float
    , meanColor   :: !PixelRGBF
    , dims        :: !PixelRGBF
    , colors      :: VU.Vector PackedRGB
    }

instance Eq Cluster where
    a == b =
        (value a, meanColor a, dims a) == (value b, meanColor b, dims b)

instance Ord Cluster where
    compare a b =
        compare (value a, meanColor a, dims a) (value b, meanColor b, dims b)

data Axis = RAxis | GAxis | BAxis

inf :: Float
inf = read "Infinity"

fromRGB8 :: PixelRGB8 -> PixelRGBF
fromRGB8 (PixelRGB8 r g b) =
  PixelRGBF (fromIntegral r) (fromIntegral g) (fromIntegral b)

toRGB8 :: PixelRGBF -> PixelRGB8
toRGB8 (PixelRGBF r g b) =
  PixelRGB8 (round r) (round g) (round b)

meanRGB :: Fold PixelRGBF PixelRGBF
meanRGB = mean <$> intLength <*> pixelSum
  where
    pixelSum = Fold (mixWith $ const (+)) (PixelRGBF 0 0 0) id
    mean n = colorMap (/ nf)
      where nf = fromIntegral n

minimal :: Fold PixelRGBF PixelRGBF
minimal = Fold mini (PixelRGBF inf inf inf) id
  where mini = mixWith $ const min

maximal :: Fold PixelRGBF PixelRGBF
maximal = Fold maxi (PixelRGBF (-inf) (-inf) (-inf)) id
  where maxi = mixWith $ const max

extrems :: Fold PixelRGBF (PixelRGBF, PixelRGBF)
extrems = (,) <$> minimal <*> maximal

volAndDims :: Fold PixelRGBF (Float, PixelRGBF)
volAndDims = deltify <$> extrems
  where deltify (mini, maxi) = (dr * dg * db, delta)
          where delta@(PixelRGBF dr dg db) =
                        mixWith (const (-)) maxi mini

unpackFold :: Fold PixelRGBF a -> Fold PackedRGB a
unpackFold (Fold step start done) = Fold (\acc -> step acc . transform) start done
  where transform = fromRGB8 . rgbIntUnpack

mkCluster :: VU.Vector PackedRGB -> Cluster
mkCluster ps = Cluster
    { value = v * fromIntegral l
    , meanColor = m
    , dims = ds
    , colors = ps
    }
  where
    worker = (,,) <$> volAndDims <*> meanRGB <*> intLength
    ((v, ds), m, l) = fold (unpackFold worker) ps

maxAxis :: PixelRGBF -> Axis
maxAxis (PixelRGBF r g b) =
  case (r `compare` g, r `compare` b, g `compare` b) of
    (GT, GT, _)  -> RAxis
    (LT, GT, _)  -> GAxis
    (GT, LT, _)  -> BAxis
    (LT, LT, GT) -> GAxis
    (EQ, GT, _)  -> RAxis
    (_,  _,  _)  -> BAxis

-- Split a cluster about its largest axis using the mean to divide up the
-- pixels.
subdivide :: Cluster -> (Cluster, Cluster)
subdivide cluster = (mkCluster px1, mkCluster px2)
  where
    (PixelRGBF mr mg mb) = meanColor cluster
    (px1, px2) = VU.partition (cond . rgbIntUnpack) $ colors cluster
    cond = case maxAxis $ dims cluster of
      RAxis -> \(PixelRGB8 r _ _) -> fromIntegral r < mr
      GAxis -> \(PixelRGB8 _ g _) -> fromIntegral g < mg
      BAxis -> \(PixelRGB8 _ _ b) -> fromIntegral b < mb

rgbIntPack :: PixelRGB8 -> PackedRGB
rgbIntPack (PixelRGB8 r g b) =
    wr `unsafeShiftL` (2 * 8) .|. wg `unsafeShiftL` 8 .|. wb
  where wr = fromIntegral r
        wg = fromIntegral g
        wb = fromIntegral b

rgbIntUnpack :: PackedRGB -> PixelRGB8
rgbIntUnpack v = PixelRGB8 r g b
  where
    r = fromIntegral $ v `unsafeShiftR` (2 * 8)
    g = fromIntegral $ v `unsafeShiftR` 8
    b = fromIntegral v

initCluster :: Image PixelRGB8 -> Cluster
initCluster img = mkCluster $ VU.generate ((w * h) `div` subSampling) packer
  where samplingFactor = 3
        subSampling = samplingFactor * samplingFactor
        compCount = componentCount (undefined :: PixelRGB8)
        w = imageWidth img
        h = imageHeight img
        rawData = imageData img
        packer ix =
            rgbIntPack . unsafePixelAt rawData $ ix * subSampling * compCount

-- Take the cluster with the largest value = (volume * population) and remove it
-- from the priority queue. Then subdivide it about its largest axis and put the
-- two new clusters on the queue.
split :: Set Cluster -> Set Cluster
split cs = Set.insert c1 . Set.insert c2  $ cs'
  where
    (c, cs') = Set.deleteFindMax cs
    (c1, c2) = subdivide c

-- Keep splitting the initial cluster until there are 256 clusters, then return
-- a priority queue containing all 256.
clusters :: Int -> Image PixelRGB8 -> Set Cluster
clusters maxCols img = clusters' (maxCols - 1)
  where
    clusters' :: Int -> Set Cluster
    clusters' 0 = Set.singleton c
    clusters' n = split (clusters' (n-1))
    c = initCluster img

-- Euclidean distance squared, between two pixels.
dist2Px :: PixelRGB8 -> PixelRGB8 -> Int
dist2Px (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = dr*dr + dg*dg + db*db
  where
    (dr, dg, db) =
      ( fromIntegral r1 - fromIntegral r2
      , fromIntegral g1 - fromIntegral g2
      , fromIntegral b1 - fromIntegral b2 )

nearestColorIdx :: PixelRGB8 -> Vector PixelRGB8 -> Pixel8
nearestColorIdx p ps  = fromIntegral $ V.minIndex (V.map (`dist2Px` p) ps)
