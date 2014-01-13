-----------------------------------------------------------------------------
-- |
-- Module      :  Picture.ColorQuant
-- Copyright   :  (c) 2013 Jeffrey Rosenbluth
-- License     :  BSD3
--
-- Palette generation and dithering.
--
-----------------------------------------------------------------------------

module Codec.Picture.ColorQuant (
                                  PaletteCreationMethod(..)
                                , PaletteOpts(..)
                                , palettize
                                , withPalette
                                ) where

import           Codec.Picture.Types
import           Data.Bits                 ((.&.))
import           Data.List                 (foldl', foldl1', partition)
import           Data.Map.Lazy             (Map)
import qualified Data.Map.Lazy as L
import           Data.Set                  (Set)
import qualified Data.Set as Set
import           Data.Vector               (Vector, minimumBy, (!))
import qualified Data.Vector as V

-------------------------------------------------------------------------------
----            Palette Creation and Dithering
-------------------------------------------------------------------------------

-- | Use either a modified median cut two pass algorithm or a uniform
--   quantization one pass algorithm. The medain cut algorithm produces much
--   better results, but the unfiorm qunatization algorithm is faster.
data PaletteCreationMethod = ModMedianCut | Uniform

-- | To specify how the palette will be created.
data PaletteOpts =
  PaletteOpts { createMethod :: PaletteCreationMethod
              , dithering    :: Bool
              , maxColors    :: Int
              }

-- A version of `pixelFold` that does not depend on the pixel coordinates.
foldPx :: Pixel pixel => (acc -> pixel -> acc) -> acc -> Image pixel -> acc
foldPx f acc = pixelFold g acc
  where g ps _ _ p = f ps p

palettize :: PaletteOpts -> Image PixelRGB8 -> (Image PixelRGB8, Palette)
palettize opts img =
  case createMethod opts of
    ModMedianCut -> colorQuantMMC (dithering opts) (maxColors opts) img
    Uniform      -> colorQuantUQ  (dithering opts) (maxColors opts) img

withPalette :: Palette -> Bool -> Image PixelRGB8 -> Image PixelRGB8
withPalette palette ditherOn img = pixelMap paletteFunc img'
  where
    paletteFunc p = L.findWithDefault (PixelRGB8 0 0 0) p paletteDict
    paletteDict = toColorDict img' (V.fromList paletteList)
    paletteList = foldPx (flip (:)) [] palette
    img' = if ditherOn then pixelMapXY dither img else img

-- Modified median cut algorithm with optional ordered dithering.
colorQuantMMC :: Bool -> Int -> Image PixelRGB8 -> (Image PixelRGB8, Palette)
colorQuantMMC ditherOn maxCols img
  | colorCount img <= maxCols = (img, fullPalette img)
  | ditherOn = (pixelMap paletteFunc dImg, palette)
  | otherwise = (pixelMap paletteFunc img, palette)
  where
    paletteFunc p = L.findWithDefault (PixelRGB8 0 0 0) p paletteDict
    paletteDict = if ditherOn
                  then toColorDict dImg paletteVec
                  else colorDict cs
    paletteVec = mkPaletteVec cs
    palette = vecToPalette paletteVec
    cs =  Set.toList . clusters maxCols $ img
    dImg = pixelMapXY dither img

-- A naive one pass Color Quantiation algorithm - Uniform Quantization.
-- Simply take the most significant bits.
colorQuantUQ :: Bool -> Int -> Image PixelRGB8 -> (Image PixelRGB8, Palette)
colorQuantUQ ditherOn maxCols img
  | colorCount img <= maxCols = (img, fullPalette img)
  | ditherOn = (pixelMap paletteFunc (pixelMapXY dither img), palette)
  | otherwise = (pixelMap paletteFunc img, palette)
  where
    palette = listToPalette paletteList
    paletteList = [PixelRGB8 r g b | r <- [0,dr..255], g <- [0,dg..255], b <- [0,db..255]]
    (bg, br, bb) = bitDiv3 maxCols -- give the most bits to green, least to blue
    (dr, dg, db) = (2^(8-br), 2^(8-bg), 2^(8-bb))
    paletteFunc (PixelRGB8 r g b) =
      PixelRGB8 (r .&. (256 - dr))
                (g .&. (256 - dg))
                (b .&. (256 - db))

toColorDict :: Image PixelRGB8 ->  Vector PixelRGB8 -> Map PixelRGB8 PixelRGB8
toColorDict img pal = foldPx f L.empty img
  where
    f xs p = L.insert p (nearestColor p pal) xs

colorCount :: Image PixelRGB8 -> Int
colorCount img = Set.size s
  where
    s = foldPx f Set.empty img
    f xs p = Set.insert p xs

vecToPalette :: Vector PixelRGB8 -> Palette
vecToPalette ps = generateImage (\x _ -> ps ! x) (V.length ps) 1

listToPalette :: [PixelRGB8] -> Palette
listToPalette ps = generateImage (\x _ -> ps !! x) (length ps) 1

fullPalette :: Image PixelRGB8 -> Palette
fullPalette img = listToPalette cs
  where
    cs = foldPx (\ps p -> p : ps) [] img

bitDiv3 :: Int -> (Int, Int, Int)
bitDiv3 n = case r of
            0 -> (q, q, q)
            1 -> (q+1, q, q)
            _ -> (q+1, q+1, q)
  where
    r = m `mod` 3
    q = m `div` 3
    m = floor . logBase 2 $ fromIntegral n

-------------------------------------------------------------------------------
----            Dithering
-------------------------------------------------------------------------------

-- Add a dither mask to an image for ordered dithering.
-- Using a small, spatially stable dithering algorithm based on magic numbers
-- and arithmetic inspired by the *a dither* algorithm of Øyvind Kolås,
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
----            Modified Median Cut Algorithm
-------------------------------------------------------------------------------

-- Based on the OCaml implementation:
-- http://rosettacode.org/wiki/Color_quantization
-- which is in turn based on: www.leptonica.com/papers/mediancut.pdf.
-- We use the product of volume and population to determine the next cluster
-- to split and determine the placement of each color by compating it to the
-- mean of the parent cluster. So median cut is a bit of a misnomer, since one
-- of the modifiations is to use the mean.

toAList :: Cluster -> [(PixelRGB8, PixelRGB8)]
toAList (Cluster _ m _ cs) = foldr f [] cs
  where
    f p = ((toRGB8 p, m') :)
    m' = toRGB8 m

colorDict :: [Cluster] -> Map PixelRGB8 PixelRGB8
colorDict cs = L.fromList aList
  where
    aList = concatMap toAList cs

mkPaletteVec :: [Cluster] -> Vector PixelRGB8
mkPaletteVec  = V.fromList . map (toRGB8 . meanColor)


data RGBdbl = RGBdbl !Double !Double !Double deriving (Eq, Ord)

data Cluster = Cluster { value     :: Double
                       , meanColor :: RGBdbl
                       , dims      :: RGBdbl
                       , colors    ::[RGBdbl]
                       } deriving (Eq, Ord)

data Axis = RAxis | GAxis | BAxis

inf :: Double
inf = read "Infinity"

fromRGB8 :: PixelRGB8 -> RGBdbl
fromRGB8 (PixelRGB8 r g b) =
  RGBdbl (fromIntegral r) (fromIntegral g) (fromIntegral b)

toRGB8 :: RGBdbl -> PixelRGB8
toRGB8 (RGBdbl r g b) =
  PixelRGB8 (round r) (round g) (round b)

addRGB :: RGBdbl -> RGBdbl -> RGBdbl
addRGB (RGBdbl r1 g1 b1) (RGBdbl r2 g2 b2) =
  RGBdbl (r1 + r2) (g1 + g2) (b1 + b2)

meanRGB :: [RGBdbl] -> RGBdbl
meanRGB ps = RGBdbl r g b
  where
    n = fromIntegral $ length ps
    RGBdbl rs gs bs = foldl1' addRGB ps
    (r, g, b) = (rs / n, gs / n, bs / n)

maxRGB :: RGBdbl -> RGBdbl -> RGBdbl
maxRGB (RGBdbl r1 g1 b1) (RGBdbl r2 g2 b2) =
  RGBdbl (max r1 r2) (max g1 g2) (max b1 b2)

minRGB :: RGBdbl -> RGBdbl -> RGBdbl
minRGB (RGBdbl r1 g1 b1) (RGBdbl r2 g2 b2) =
  RGBdbl (min r1 r2) (min g1 g2) (min b1 b2)

extrems :: [RGBdbl] -> (RGBdbl, RGBdbl)
extrems ps = (s, b)
  where
    s = foldl' (\sp p -> minRGB sp p) (RGBdbl inf inf inf) ps
    b = foldl' (\bp p -> maxRGB bp p) (RGBdbl (-inf) (-inf) (-inf)) ps

volAndDims :: [RGBdbl] -> (Double, RGBdbl)
volAndDims ps = (dr * dg * db, RGBdbl dr dg db)
  where
    (RGBdbl sr sg sb, RGBdbl br bg bb) = extrems ps
    (dr, dg, db) = (br - sr, bg - sg, bb - sb)

mkCluster :: [RGBdbl] -> Cluster
mkCluster ps = Cluster (v * l) m ds ps
  where
    (v, ds) = volAndDims ps
    m = meanRGB ps
    l = fromIntegral $ length ps

maxAxis :: RGBdbl -> Axis
maxAxis (RGBdbl r g b) =
  case (r `compare` g, r `compare` b, g `compare` b) of
    (GT, GT, _)  -> RAxis
    (LT, GT, _)  -> GAxis
    (GT, LT, _)  -> BAxis
    (LT, LT, GT) -> GAxis
    (_,  _,  _)  -> BAxis

-- Split a cluster about its largest axis using the mean to divide up the
-- pixels.
subdivide :: Cluster -> (Cluster, Cluster)
subdivide (Cluster _ (RGBdbl mr mg mb) vol ps) = (mkCluster px1, mkCluster px2)
  where
    (px1, px2) = partition cond ps
    cond = case maxAxis vol of
      RAxis -> (\(RGBdbl r _ _) -> r < mr)
      GAxis -> (\(RGBdbl _ g _) -> g < mg)
      BAxis -> (\(RGBdbl _ _ b) -> b < mb)

-- Put 1/9th of the pixels in the initial cluster.
initCluster :: Image PixelRGB8 -> Cluster
initCluster img = mkCluster $ pixelFoldSample f 3 [] img
  where
    f xs p = fromRGB8 p : xs

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

dist2Px :: PixelRGB8 -> PixelRGB8 -> Int
dist2Px (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = dr*dr + dg*dg + db*db
  where
    (dr, dg, db) =
      ( fromIntegral r1 - fromIntegral r2
      , fromIntegral g1 - fromIntegral g2
      , fromIntegral b1 - fromIntegral b2 )

nearestColor :: PixelRGB8 -> Vector PixelRGB8 -> PixelRGB8
nearestColor p ps = snd $ minimumBy comp ds
  where
    ds = V.map (\px -> (dist2Px px p, px)) ps
    comp a b = fst a `compare` fst b