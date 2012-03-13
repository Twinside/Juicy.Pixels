module Codec.Picture.Jpg.FastDct( fastDct, slowFdct, fastDctLibJpg ) where

import Control.Applicative( (<$>) )
import Data.Int( Int16, Int32 )
import Data.Bits( Bits, shiftR )
import Control.Monad.ST( ST )

import qualified Data.Vector.Storable.Mutable as M

import Codec.Picture.Jpg.Types
import Control.Monad( forM, forM_ )

{-# INLINE (.>>.) #-}
(.>>.) :: (Bits a) => a -> Int -> a
(.>>.) = shiftR

slowFdct :: MutableMacroBlock s Int32
         -> MutableMacroBlock s Int16
         -> ST s (MutableMacroBlock s Int32)
slowFdct workData block = forM_ [(u, v) | u <- [0 :: Int .. 7], v <- [0..7]] (\(u,v) -> do
    val <- at (u,v)
    (workData .<-. (v * 8 + u)) . truncate $ (1 / 4) * c u * c v * val)
    >> return workData
 where -- at :: (Int, Int) -> ST s Float
       at (u,v) = sum <$> (forM [(x,y) | x <- [0..7], y <- [0..7 :: Int]] $ \(x,y) -> do
           sample <- fromIntegral <$> (block .!!!. (y * 8 + x))
           return $ sample * cos ((2 * fromIntegral x + 1) * fromIntegral u * (pi :: Float)/ 16) 
                           * cos ((2 * fromIntegral y + 1) * fromIntegral v * pi / 16))
       c 0 = 1 / sqrt 2
       c _ = 1

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
 return workData
 {-mutate (\_ a -> a `div` 8) workData-}
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
                x3' = ((-s3) - c3) * x3 + x6''
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

            writeAt 0 $ (x6''' + 16) .>>. 3
            writeAt 4 $ (x4'  +  16) .>>. 3
            writeAt 2 $ (x8'' + 16384) .>>. 13
            writeAt 6 $ (x7'' + 16384) .>>. 13
            writeAt 7 $ (x2'' - x5''' + 16384) .>>. 13
            writeAt 1 $ (x2'' + x5''' + 16384) .>>. 13
            writeAt 3 $ ((x3'' .>>. 8) * r2 + 8192) .>>. 12
            writeAt 5 $ ((x0'' .>>. 8) * r2 + 8192) .>>. 12

            secondPass block (i + 1)

cENTERJSAMPLE :: Int32
cENTERJSAMPLE = 128

fIX_0_382683433, fIX_0_541196100, fIX_0_707106781,fIX_1_306562965 :: Int32
fIX_0_382683433  = 98 -- FIX(0.382683433) */
fIX_0_541196100  = 139 -- FIX(0.541196100) */
fIX_0_707106781 = 181		-- FIX(0.707106781) */
fIX_1_306562965 =   334		-- FIX(1.306562965) */

fastDctLibJpg :: MutableMacroBlock s Int32
        -> MutableMacroBlock s Int16
        -> ST s (MutableMacroBlock s Int32)
fastDctLibJpg workData sample_block = do
 {-_ <- mutate (\_ a -> a + 128) sample_block-}
 firstPass workData  0
 secondPass workData (7::Int) (0 :: Int)
 {-return workData-}
 mutate (\_ a -> a `div` 8) workData
  where cONST_BITS = 8
        mult a constant = (a * constant) .>>. cONST_BITS
        -- Pass 1: process rows.
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
            let tmp0 = blk0 + blk7
                tmp7 = blk0 - blk7
                tmp1 = blk1 + blk6
                tmp6 = blk1 - blk6
                tmp2 = blk2 + blk5
                tmp5 = blk2 - blk5
                tmp3 = blk3 + blk4
                tmp4 = blk3 - blk4

            -- Stage 2
            let tmp10 = tmp0 + tmp3
                tmp13 = tmp0 - tmp3
                tmp11 = tmp1 + tmp2
                tmp12 = tmp1 - tmp2

            writeAt 0 $ tmp10 + tmp11 - 8 * cENTERJSAMPLE
            writeAt 4 $ tmp10 - tmp11

            -- Stage 3
            let z1 = mult (tmp12 + tmp13) fIX_0_707106781
            writeAt 2 $ tmp13 + z1
            writeAt 6 $ tmp13 - z1

            let tmp10' = tmp4 + tmp5
                tmp11' = tmp5 + tmp6
                tmp12' = tmp6 + tmp7
                z5 = mult (tmp10' - tmp12') fIX_0_382683433
                z2 = mult tmp10' fIX_0_541196100 + z5
                z4 = mult tmp12' fIX_1_306562965 + z5
                z3 = mult tmp11' fIX_0_707106781
                z11 = tmp7 + z3
                z13 = tmp7 - z3

            writeAt 5 $ z13 + z2
            writeAt 3 $ z13 - z2
            writeAt 1 $ z11 + z4
            writeAt 7 $ z11 - z4

            firstPass dataBlock $ i + 1

        -- Pass 2: process columns.
        -- We remove the PASS1_BITS scaling, but leave the results scaled up
        -- by an overall factor of 8.
        {-secondPass :: M.STVector s Int32 -> Int -> ST s ()-}
        secondPass _     (-1) _ii = return ()
        secondPass block i ii = do
            let readAt idx = block .!!!. (ii + idx * 8)
                writeAt idx n = (block .<-. (8 * idx + ii)) n
            blk0 <- readAt 0
            blk1 <- readAt 1
            blk2 <- readAt 2
            blk3 <- readAt 3
            blk4 <- readAt 4
            blk5 <- readAt 5
            blk6 <- readAt 6
            blk7 <- readAt 7

            -- Stage 1
            let tmp0 = blk0 + blk7
                tmp7 = blk0 - blk7
                tmp1 = blk1 + blk6
                tmp6 = blk1 - blk6
                tmp2 = blk2 + blk5
                tmp5 = blk2 - blk5
                tmp3 = blk3 + blk4
                tmp4 = blk3 - blk4

                tmp10 = tmp0 + tmp3	-- phase 2 */
                tmp13 = tmp0 - tmp3
                tmp11 = tmp1 + tmp2
                tmp12 = tmp1 - tmp2

            writeAt 0 $ tmp10 + tmp11
            writeAt 4 $ tmp10 - tmp11

            let z1 = mult (tmp12 + tmp13) fIX_0_707106781
            writeAt 2 $ tmp13 + z1
            writeAt 6 $ tmp13 - z1


            let tmp10' = tmp4 + tmp5	-- phase 2 */
                tmp11' = tmp5 + tmp6
                tmp12' = tmp6 + tmp7

                z5 = mult (tmp10' - tmp12') fIX_0_382683433
                z2 = mult tmp10' fIX_0_541196100 + z5
                z4 = mult tmp12' fIX_1_306562965 + z5
                z3 = mult tmp11' fIX_0_707106781

                z11 = tmp7 + z3
                z13 = tmp7 - z3

            writeAt 5 $ z13 + z2
            writeAt 3 $ z13 - z2
            writeAt 1 $ z11 + z4
            writeAt 7 $ z11 - z4

            secondPass block (i - 1) (ii + 1)

