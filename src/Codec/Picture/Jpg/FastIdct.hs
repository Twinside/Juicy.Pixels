{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Module providing a 'fast' implementation of IDCT
--
-- inverse two dimensional DCT, Chen-Wang algorithm       
-- (cf. IEEE ASSP-32, pp. 803-816, Aug. 1984)             
-- 32-bit integer arithmetic (8 bit coefficients)         
-- 11 mults, 29 adds per DCT                              
--                                      sE, 18.8.91       
--
-- coefficients extended to 12 bit for IEEE1180-1990      
-- compliance                           sE,  2.1.94       
--
-- this code assumes >> to be a two's-complement arithmetic
-- right shift: (-2)>>1 == -1 , (-3)>>1 == -2               
module Codec.Picture.Jpg.FastIdct( MutableMacroBlock
                                 , fastIdct
                                 , mutableLevelShift
                                 , createEmptyMutableMacroBlock
                                 ) where

import qualified Data.Vector.Storable as V
import Control.Monad.ST( ST )
import Data.Bits( unsafeShiftL, unsafeShiftR )
import Data.Int( Int16 )
import qualified Data.Vector.Storable.Mutable as M

import Codec.Picture.Jpg.Types

iclip :: V.Vector Int16
iclip = V.fromListN 1024 [ val i| i <- [(-512) .. 511] ]
    where val i | i < (-256) = -256
                | i > 255    =  255
                | otherwise  =  i

data IDctStage = IDctStage { 
        x0 :: {-# UNPACK #-} !Int,
        x1 :: {-# UNPACK #-} !Int,
        x2 :: {-# UNPACK #-} !Int,
        x3 :: {-# UNPACK #-} !Int,
        x4 :: {-# UNPACK #-} !Int,
        x5 :: {-# UNPACK #-} !Int,
        x6 :: {-# UNPACK #-} !Int,
        x7 :: {-# UNPACK #-} !Int,
        x8 :: {-# UNPACK #-} !Int
    }

w1, w2, w3, w5, w6, w7 :: Int
w1 = 2841 -- 2048*sqrt(2)*cos(1*pi/16)
w2 = 2676 -- 2048*sqrt(2)*cos(2*pi/16)
w3 = 2408 -- 2048*sqrt(2)*cos(3*pi/16)
w5 = 1609 -- 2048*sqrt(2)*cos(5*pi/16)
w6 = 1108 -- 2048*sqrt(2)*cos(6*pi/16)
w7 = 565  -- 2048*sqrt(2)*cos(7*pi/16)

-- row (horizontal) IDCT
--
--           7                       pi         1
-- dst[k] = sum c[l] * src[l] * cos( -- * ( k + - ) * l )
--          l=0                      8          2
--
-- where: c[0]    = 128
--        c[1..7] = 128*sqrt(2)
idctRow :: MutableMacroBlock s Int16 -> Int ->  ST s ()
idctRow blk idx = do
  xx0 <- blk `M.unsafeRead` (0 + idx)
  xx1 <- blk `M.unsafeRead` (4 + idx) 
  xx2 <- blk `M.unsafeRead` (6 + idx)
  xx3 <- blk `M.unsafeRead` (2 + idx)
  xx4 <- blk `M.unsafeRead` (1 + idx)
  xx5 <- blk `M.unsafeRead` (7 + idx)
  xx6 <- blk `M.unsafeRead` (5 + idx)
  xx7 <- blk `M.unsafeRead` (3 + idx)
  let initialState = IDctStage { x0 = (fromIntegral xx0 `unsafeShiftL` 11) + 128
                               , x1 =  fromIntegral xx1 `unsafeShiftL` 11
                               , x2 =  fromIntegral xx2
                               , x3 =  fromIntegral xx3
                               , x4 =  fromIntegral xx4
                               , x5 =  fromIntegral xx5
                               , x6 =  fromIntegral xx6
                               , x7 =  fromIntegral xx7
                               , x8 = 0
                               } 

      firstStage c = c { x4 = x8'  + (w1 - w7) * x4 c
                       , x5 = x8'  - (w1 + w7) * x5 c
                       , x6 = x8'' - (w3 - w5) * x6 c
                       , x7 = x8'' - (w3 + w5) * x7 c
                       , x8 = x8''
                       }
          where x8' = w7 * (x4 c + x5 c)
                x8'' = w3 * (x6 c + x7 c)

      secondStage c = c { x0 = x0 c - x1 c
                        , x8 = x0 c + x1 c 
                        , x1 = x1''
                        , x2 = x1' - (w2 + w6) * x2 c
                        , x3 = x1' + (w2 - w6) * x3 c
                        , x4 = x4 c - x6 c
                        , x6 = x5 c + x7 c
                        , x5 = x5 c - x7 c
                        }
            where x1'  = w6 * (x3 c + x2 c)
                  x1'' = x4 c + x6 c

      thirdStage c = c { x7 = x8 c + x3 c
                       , x8 = x8 c - x3 c
                       , x3 = x0 c + x2 c
                       , x0 = x0 c - x2 c
                       , x2 = (181 * (x4 c + x5 c) + 128) `unsafeShiftR` 8
                       , x4 = (181 * (x4 c - x5 c) + 128) `unsafeShiftR` 8
                       }
      scaled c = c { x0 = (x7 c + x1 c) `unsafeShiftR` 8
                   , x1 = (x3 c + x2 c) `unsafeShiftR` 8
                   , x2 = (x0 c + x4 c) `unsafeShiftR` 8
                   , x3 = (x8 c + x6 c) `unsafeShiftR` 8
                   , x4 = (x8 c - x6 c) `unsafeShiftR` 8
                   , x5 = (x0 c - x4 c) `unsafeShiftR` 8
                   , x6 = (x3 c - x2 c) `unsafeShiftR` 8
                   , x7 = (x7 c - x1 c) `unsafeShiftR` 8
                   }
      transformed = scaled . thirdStage . secondStage $ firstStage initialState

  (blk `M.unsafeWrite` (0 + idx)) . fromIntegral $ x0 transformed
  (blk `M.unsafeWrite` (1 + idx)) . fromIntegral $ x1 transformed
  (blk `M.unsafeWrite` (2 + idx)) . fromIntegral $ x2 transformed
  (blk `M.unsafeWrite` (3 + idx)) . fromIntegral $ x3 transformed
  (blk `M.unsafeWrite` (4 + idx)) . fromIntegral $ x4 transformed
  (blk `M.unsafeWrite` (5 + idx)) . fromIntegral $ x5 transformed
  (blk `M.unsafeWrite` (6 + idx)) . fromIntegral $ x6 transformed
  (blk `M.unsafeWrite` (7 + idx)) . fromIntegral $ x7 transformed

-- column (vertical) IDCT
--
--             7                         pi         1
-- dst[8*k] = sum c[l] * src[8*l] * cos( -- * ( k + - ) * l )
--            l=0                        8          2
--
-- where: c[0]    = 1/1024
--        c[1..7] = (1/1024)*sqrt(2)
--
idctCol :: MutableMacroBlock s Int16 -> Int -> ST s ()
idctCol blk idx = do
  xx0 <- blk `M.unsafeRead` (    0 + idx)
  xx1 <- blk `M.unsafeRead` (8 * 4 + idx)
  xx2 <- blk `M.unsafeRead` (8 * 6 + idx)
  xx3 <- blk `M.unsafeRead` (8 * 2 + idx)
  xx4 <- blk `M.unsafeRead` (8     + idx)
  xx5 <- blk `M.unsafeRead` (8 * 7 + idx)
  xx6 <- blk `M.unsafeRead` (8 * 5 + idx)
  xx7 <- blk `M.unsafeRead` (8 * 3 + idx)
  let initialState = IDctStage { x0 = (fromIntegral xx0 `unsafeShiftL` 8) + 8192
                               , x1 =  fromIntegral xx1 `unsafeShiftL` 8
                               , x2 =  fromIntegral xx2
                               , x3 =  fromIntegral xx3
                               , x4 =  fromIntegral xx4
                               , x5 =  fromIntegral xx5
                               , x6 =  fromIntegral xx6
                               , x7 =  fromIntegral xx7
                               , x8 = 0
                               } 
      firstStage c = c { x4 = (x8'  + (w1 - w7) * x4 c) `unsafeShiftR` 3
                       , x5 = (x8'  - (w1 + w7) * x5 c) `unsafeShiftR` 3
                       , x6 = (x8'' - (w3 - w5) * x6 c) `unsafeShiftR` 3
                       , x7 = (x8'' - (w3 + w5) * x7 c) `unsafeShiftR` 3
                       , x8 = x8''
                       }
          where x8'  = w7 * (x4 c + x5 c) + 4
                x8'' = w3 * (x6 c + x7 c) + 4

      secondStage c = c { x8 = x0 c + x1 c
                        , x0 = x0 c - x1 c
                        , x2 = (x1' - (w2 + w6) * x2 c) `unsafeShiftR` 3
                        , x3 = (x1' + (w2 - w6) * x3 c) `unsafeShiftR` 3
                        , x4 = x4 c - x6 c
                        , x1 = x1''
                        , x6 = x5 c + x7 c
                        , x5 = x5 c - x7 c
                        }
          where x1'  = w6 * (x3 c + x2 c) + 4
                x1'' = x4 c + x6 c
  
      thirdStage c = c { x7 = x8 c + x3 c
                       , x8 = x8 c - x3 c
                       , x3 = x0 c + x2 c
                       , x0 = x0 c - x2 c
                       , x2 = (181 * (x4 c + x5 c) + 128) `unsafeShiftR` 8
                       , x4 = (181 * (x4 c - x5 c) + 128) `unsafeShiftR` 8
                       }

      clip i | i < 511 = if i > -512 then iclip `V.unsafeIndex` (i + 512)
                                    else iclip `V.unsafeIndex` 0
            
             | otherwise = iclip `V.unsafeIndex` 1023

      f = thirdStage . secondStage $ firstStage initialState
  (blk `M.unsafeWrite` (idx + 8*0)) . clip $ (x7 f + x1 f) `unsafeShiftR` 14
  (blk `M.unsafeWrite` (idx + 8  )) . clip $ (x3 f + x2 f) `unsafeShiftR` 14
  (blk `M.unsafeWrite` (idx + 8*2)) . clip $ (x0 f + x4 f) `unsafeShiftR` 14
  (blk `M.unsafeWrite` (idx + 8*3)) . clip $ (x8 f + x6 f) `unsafeShiftR` 14
  (blk `M.unsafeWrite` (idx + 8*4)) . clip $ (x8 f - x6 f) `unsafeShiftR` 14
  (blk `M.unsafeWrite` (idx + 8*5)) . clip $ (x0 f - x4 f) `unsafeShiftR` 14
  (blk `M.unsafeWrite` (idx + 8*6)) . clip $ (x3 f - x2 f) `unsafeShiftR` 14
  (blk `M.unsafeWrite` (idx + 8*7)) . clip $ (x7 f - x1 f) `unsafeShiftR` 14


{-# INLINE fastIdct #-}
-- | Algorithm to call to perform an IDCT, return the same
-- block that the one given as input.
fastIdct :: MutableMacroBlock s Int16
         -> ST s (MutableMacroBlock s Int16)
fastIdct block = rows 0
  where rows 8 = cols 0
        rows i = idctRow block (8 * i) >> rows (i + 1)

        cols 8 = return block
        cols i = idctCol block i >> cols (i + 1)

{-# INLINE mutableLevelShift #-}
-- | Perform a Jpeg level shift in a mutable fashion.
mutableLevelShift :: MutableMacroBlock s Int16
                  -> ST s (MutableMacroBlock s Int16)
mutableLevelShift block = update 0
  where update 64 = return block
        update idx = do
            val <- block `M.unsafeRead` idx
            (block `M.unsafeWrite` idx) $ val + 128
            update $ idx + 1

