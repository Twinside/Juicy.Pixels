module Codec.Picture.ColorQuant where

import           Codec.Picture.Types
import           Data.Bits            ((.&.))
import           Data.List            (foldl', foldl1', partition, minimumBy)
import           Data.Map.Lazy        (Map)
import qualified Data.Map.Lazy as L

-------------------------------------------------------------------------------
----            Single Pass Algorithm
-------------------------------------------------------------------------------

-- | A naive one pass Color Quantiation algorithm. Simply take the 3 most
--   significant bits of red and green. Take the 2 most significant bits of
--   blue.
onePassCQ :: Image PixelRGB8 -> Image PixelRGB8
onePassCQ = pixelMap maskFunction
  where maskFunction (PixelRGB8 r g b) =
          PixelRGB8 (r .&. 224)
                    (g .&. 224)
                    (b .&. 192)

-------------------------------------------------------------------------------
----            Modified Median Cut Algorithm
-------------------------------------------------------------------------------

--  Based on the OCaml implementation:
--  http://rosettacode.org/wiki/Color_quantization
--  which was in turn based on: www.leptonica.com/papers/mediancut.pdf.
--  We use the product of volume and population to determine the next cluster
--  to split and assign the color of each cluster to its mean color.

-- | Modified median cut algorithm without dithering.
mmcCQ :: Image PixelRGB8 -> Image PixelRGB8
mmcCQ img = pixelMap paletteFunc img
  where
    paletteFunc p = case L.lookup p pm of
                      Just px -> px
                      Nothing -> PixelRGB8 0 0 0
    pm = paletteMap . clusters $ img

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

-- | Modified median cut algorithm with ordered dithering.
dmmcCQ :: Image PixelRGB8 -> Image PixelRGB8
dmmcCQ img = pixelMap paletteFunc dImg
  where
    paletteFunc p = paletteColor p pal
    pal = mkPalette . clusters $ img
    dImg = pixelMapXY dither img

-- Much room for optimization / better implementation, this is a pretty direct
-- port. E.g. use Vectors not lists and perhaps a priority queue to choose the
-- next cluster to split.

data RGBdbl = RGBdbl !Double !Double !Double deriving Eq

data Cluster = Cluster { meanColor :: RGBdbl
                       , value     :: Double
                       , dims      :: RGBdbl
                       , colors    ::[RGBdbl]
                       } deriving Eq

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
mkCluster ps = Cluster m (v * l) ds ps
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

subdivide :: Cluster -> (Cluster, Cluster)
subdivide (Cluster (RGBdbl mr mg mb) _ vol ps) = (mkCluster px1, mkCluster px2)
  where
    (px1, px2) = partition cond ps
    cond = case maxAxis vol of
      RAxis -> (\(RGBdbl r _ _) -> r < mr)
      GAxis -> (\(RGBdbl _ g _) -> g < mg)
      BAxis -> (\(RGBdbl _ _ b) -> b < mb)

initCluster :: Image PixelRGB8 -> Cluster
initCluster img = mkCluster $ pixelFold f [] img
  where
    f xs _ _ p = fromRGB8 p : xs

split :: [Cluster] -> [Cluster]
split cs = cl1 : cl2 : cs'
  where
    (cl1, cl2) = subdivide c
    c = foldl' select unused cs
    select c1 c2 = if value c1 > value c2 then c1 else c2
    unused = Cluster dumb (-inf) dumb []
    dumb = RGBdbl 0 0 0
    cs' = filter (/= c) cs

clusters :: Image PixelRGB8 -> [Cluster]
clusters img = cs !! 255
  where
    cs = iterate split [c]
    c = initCluster img

toAList :: Cluster -> [(PixelRGB8, PixelRGB8)]
toAList (Cluster m _ _ cs) = foldr f [] cs
  where
    f p = ((toRGB8 p, m') :)
    m' = toRGB8 m

paletteMap :: [Cluster] -> Map PixelRGB8 PixelRGB8
paletteMap cs = L.fromList aList
  where
    aList = concatMap toAList cs

mkPalette :: [Cluster] -> [PixelRGB8]
mkPalette  = map (toRGB8 . meanColor)

dist2Px :: PixelRGB8 -> PixelRGB8 -> Int
dist2Px (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = dr*dr + dg*dg + db*db
  where
    (dr, dg, db) =
      ( fromIntegral r1 - fromIntegral r2
      , fromIntegral g1 - fromIntegral g2
      , fromIntegral b1 - fromIntegral b2 )

paletteColor :: PixelRGB8 -> [PixelRGB8] -> PixelRGB8
paletteColor p ps = snd $ minimumBy comp ds
  where
    ds = map (\px -> (dist2Px px p, px)) ps
    comp a b = fst a `compare` fst b
