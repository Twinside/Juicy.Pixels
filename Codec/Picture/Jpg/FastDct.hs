module Codec.Picture.Jpg.FastDct( fastDct ) where

import Control.Applicative( (<$>) )
import Data.Int( Int16, Int32 )
import Data.Bits( Bits, shiftR )
import Control.Monad.ST( ST )

import qualified Data.Vector.Storable.Mutable as M

import Codec.Picture.Jpg.Types

{-# INLINE (.>>.) #-}
(.>>.) :: (Bits a) => a -> Int -> a
(.>>.) = shiftR

c1, s1, c3, s3, r2c6, r2s6, r2 :: Int32
c1   = 1004 -- cos(pi/16) << 10
s1   = 200 -- sin(pi/16)
c3   = 851 -- cos(3pi/16) << 10
s3   = 569 -- sin(3pi/16) << 10
r2c6 = 554 -- sqrt(2)*cos(6pi/16)
r2s6 = 1337 -- sqrt(2)*sin(6pi/16)
r2   = 181; -- sqrt(2) << 7

fastDct :: MutableMacroBlock s Int32
        -> MutableMacroBlock s Int16
        -> ST s (MutableMacroBlock s Int32)
fastDct workData sample_block = do
 firstPass workData  0
 secondPass workData 0
 mutate (\_ a -> a `div` 8) workData
  where -- Pass 1: process rows.
        -- Note results are scaled up by sqrt(8) compared to a true DCT;
        -- furthermore, we scale the results by 2**PASS1_BITS.
        firstPass         _ 8 = return ()
        firstPass dataBlock i = do
            let baseIdx = i * 8
                readAt idx = fromIntegral <$> sample_block .!!!. (baseIdx + idx)
                writeAt idx n = (dataBlock .<-. (baseIdx + idx)) n

            blk0 <- readAt 0
            blk1 <- readAt 1
            blk2 <- readAt 2
            blk3 <- readAt 3
            blk4 <- readAt 4
            blk5 <- readAt 5
            blk6 <- readAt 6
            blk7 <- readAt 7

            -- Stage 1
            let x8 = blk7 + blk0
                x0 = blk0 - blk7
                x7 = blk1 + blk6
                x1 = blk1 - blk6
                x6 = blk2 + blk5
                x2 = blk2 - blk5
                x5 = blk3 + blk4
                x3 = blk3 - blk4

            -- Stage 2
            let x4' = x8 + x5
                x8' = x8 - x5
                x5' = x7 + x6
                x7' = x7 - x6
                x6' = c1 * (x1 + x2)
                x2' = ((-s1) - c1) * x2 + x6'
                x1' = (s1 - c1) * x1 + x6'
                x6'' = c3 * (x0 + x3)
                x3' = (-s3 - c3) * x3 + x6''
                x0' = (s3 - c3) * x0 + x6''

            -- Stage 3
            let x6''' = x4' + x5'
                x4'' = x4' - x5'
                x5'' = r2c6 * (x7' + x8')
                x7'' = ((-r2s6) - r2c6) * x7' + x5''
                x8'' = (r2s6 - r2c6) * x8' + x5''
                x5''' = x0' + x2'
                x0'' = x0' - x2'
                x2'' = x3' + x1'
                x3'' = x3' - x1'

            -- Stage 4 and output
            writeAt 0 $ x6'''
            writeAt 4 $ x4''
            writeAt 2 $ x8'' .>>. 10
            writeAt 6 $ x7'' .>>. 10
            writeAt 7 $ (x2'' - x5''') .>>. 10
            writeAt 1 $ (x2'' + x5''') .>>. 10
            writeAt 3 $ (x3'' * r2) .>>. 17
            writeAt 5 $ (x0'' * r2) .>>. 17

            firstPass dataBlock $ i + 1

        -- Pass 2: process columns.
        -- We remove the PASS1_BITS scaling, but leave the results scaled up
        -- by an overall factor of 8.
        secondPass :: M.STVector s Int32 -> Int -> ST s ()
        secondPass _     8 = return ()
        secondPass block i = do
            let readAt idx = block .!!!. (i + idx * 8)
                writeAt idx n = (block .<-. (8 * idx + i)) n
            blk0 <- readAt 0
            blk1 <- readAt 1
            blk2 <- readAt 2
            blk3 <- readAt 3
            blk4 <- readAt 4
            blk5 <- readAt 5
            blk6 <- readAt 6
            blk7 <- readAt 7

            -- Stage 1
            let x8 = blk7 + blk0
                x0 = blk0 - blk7
                x7 = blk1 + blk6
                x1 = blk1 - blk6
                x6 = blk2 + blk5
                x2 = blk2 - blk5
                x5 = blk3 + blk4
                x3 = blk3 - blk4

            -- Stage 2
            let x4 = x8 + x5
                x8' = x8 - x5
                x5' = x7 + x6
                x7' = x7 - x6
                x6' = c1 * (x1 + x2)
                x2' = ((-s1) - c1) * x2 + x6'
                x1' = (s1 - c1) * x1 + x6'
                x6'' = c3 * (x0 + x3)
                x3' = ((-s3) - c3) * x3 + x6''
                x0' = (s3 - c3) * x0 + x6''

            -- Stage 3
            let x6''' = x4 + x5'
                x4' = x4 - x5'
                x5'' = r2c6 * (x7' + x8')
                x7'' = ((-r2s6) - r2c6) * x7' + x5''
                x8'' = (r2s6 - r2c6) * x8' + x5''
                x5''' = x0' + x2'
                x0'' = x0' - x2'
                x2'' = x3' + x1'
                x3'' = x3' - x1'

            writeAt 0 $ (x6''' + 16)
            writeAt 4 $ (x4'  +  16)
            writeAt 2 $ (x8'' + 16384) .>>. 10
            writeAt 6 $ (x7'' + 16384) .>>. 10
            writeAt 7 $ (x2'' - x5''' + 16384) .>>. 10
            writeAt 1 $ (x2'' + x5''' + 16384) .>>. 10
            writeAt 3 $ ((x3'' .>>. 8) * r2 + 8192) .>>. 9
            writeAt 5 $ ((x0'' .>>. 8) * r2 + 8192) .>>. 9

            secondPass block (i + 1)

