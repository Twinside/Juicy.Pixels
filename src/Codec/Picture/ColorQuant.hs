module Codec.Picture.ColorQuant where

import           Codec.Picture.Types
import           Data.Bits                         ((.&.))
import           Data.Word                         (Word8)
import           Data.List                         (foldl', foldl1', partition)
import           Data.Map.Lazy                     (Map)
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

-- | A port of the OCaml implementation:
--   http://rosettacode.org/wiki/Color_quantization
--   which was in turn based on: www.leptonica.com/papers/mediancut.pdf.

modMedianCutCQ :: Image PixelRGB8 -> Image PixelRGB8
modMedianCutCQ img = pixelMap paletteFunc img
  where
    paletteFunc p = case L.lookup p pm of
                      Just px -> px
                      Nothing -> PixelRGB8 0 0 0
    pm = paletteMap . clusters $ img

-- Much room for optimization / better implementation, this is a pretty direct
-- port. E.g. use Vectors not lists and perhaps a priority queue to choose the
-- next cluster to split.

data RGBdbl = RGBdbl !Double !Double !Double deriving Eq

data Cluster = Cluster { meanColor :: RGBdbl
                       , value     :: Double
                       , dims      :: RGBdbl
                       , pixels    ::[RGBdbl]
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
extrems ps = foldl' (\(sp, bp) p -> (minRGB sp p, maxRGB bp p))
                    (RGBdbl inf inf inf, RGBdbl (-inf) (-inf) (-inf)) ps

volAndDims :: [RGBdbl] -> (Double, RGBdbl)
volAndDims ps = (dr * dg * db, RGBdbl dr dg db)
  where
    (RGBdbl sr sg sb, RGBdbl br bg bb) = extrems ps
    (dr, dg, db) = (br - sr, bg - sg, bb - sb)

mkCluster :: [RGBdbl] -> Cluster
mkCluster ps = Cluster m v ds ps
  where
    (v, ds) = volAndDims ps
    m = meanRGB ps

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
    select c1@(Cluster _ v1 _ _) c2@(Cluster _ v2 _ _) =
      if v1 > v2 then c1 else c2
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

-------------------------------------------------------------------------------
----            Dithering
-------------------------------------------------------------------------------

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
