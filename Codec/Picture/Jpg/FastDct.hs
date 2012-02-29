module Codec.Picture.Jpg.FastDct( fastDct ) where

{- Copyright (C) 1996, MPEG Software Simulation Group. All Rights Reserved. */
   This routine is a slow-but-accurate integer implementation of the
   forward DCT (Discrete Cosine Transform). Taken from the IJG software
  
   A 2-D DCT can be done by 1-D DCT on each row followed by 1-D DCT
   on each column.  Direct algorithms are also available, but they are
   much more complex and seem not to be any faster when reduced to code.
  
   This implementation is based on an algorithm described in
     C. Loeffler, A. Ligtenberg and G. Moschytz, "Practical Fast 1-D DCT
     Algorithms with 11 Multiplications", Proc. Int'l. Conf. on Acoustics,
     Speech, and Signal Processing 1989 (ICASSP '89), pp. 988-991.
   The primary algorithm described there uses 11 multiplies and 29 adds.
   We use their alternate method with 12 multiplies and 32 adds.
   The advantage of this method is that no data path contains more than one
   multiplication; this allows a very simple and accurate implementation in
   scaled fixed-point arithmetic, with a minimal number of shifts.
  
   The poop on this scaling stuff is as follows:
  
   Each 1-D DCT step produces outputs which are a factor of sqrt(N)
   larger than the true DCT outputs.  The final outputs are therefore
   a factor of N larger than desired; since N=8 this can be cured by
   a simple right shift at the end of the algorithm.  The advantage of
   this arrangement is that we save two multiplications per 1-D DCT,
   because the y0 and y4 outputs need not be divided by sqrt(N).
   In the IJG code, this factor of 8 is removed by the quantization step
   (in jcdctmgr.c), here it is removed.
  
   We have to do addition and subtraction of the integer inputs, which
   is no problem, and multiplication by fractional constants, which is
   a problem to do in integer arithmetic.  We multiply all the constants
   by CONST_SCALE and convert them to integer constants (thus retaining
   CONST_BITS bits of precision in the constants).  After doing a
   multiplication we have to divide the product by CONST_SCALE, with proper
   rounding, to produce the correct output.  This division can be done
   cheaply as a right shift of CONST_BITS bits.  We postpone shifting
   as long as possible so that partial sums can be added together with
   full fractional precision.
  
   The outputs of the first pass are scaled up by PASS1_BITS bits so that
   they are represented to better-than-integral precision.  These outputs
   require 8 + PASS1_BITS + 3 bits; this fits in a 16-bit word
   with the recommended scaling.  (For 12-bit sample data, the intermediate
   array is INT32 anyway.)
  
   To avoid overflow of the 32-bit intermediate results in pass 2, we must
   have 8 + CONST_BITS + PASS1_BITS <= 26.  Error analysis
   shows that the values given below are the most effective.
  
   We can gain a little more speed, with a further compromise in accuracy,
   by omitting the addition in a descaling shift.  This yields an incorrectly
   rounded result half the time...
  -}

import Control.Applicative( (<$>) )
import Data.Int( Int16, Int32 )
import Data.Bits( Bits, shiftL, shiftR )
import Control.Monad.ST( ST )

import qualified Data.Vector.Storable.Mutable as M

import Codec.Picture.Jpg.Types

{-# INLINE (.<<.) #-}
{-# INLINE (.>>.) #-}
(.<<.), (.>>.) :: (Bits a) => a -> Int -> a
(.<<.) = shiftL
(.>>.) = shiftR

{-# INLINE descale #-}
-- | If we don't use accurate rounding, just x instead of subval
descale :: Int32 -> Int -> Int32
descale x n = subVal .>>. n
    where subVal = x + (1 .<<. (n - 1))

cONST_BITS, pASS1_BITS :: Int 
cONST_BITS = 13
pASS1_BITS = 2

fIX_0_298631336, fIX_0_390180644, fIX_0_541196100,
    fIX_0_765366865, fIX_0_899976223, fIX_1_175875602,
    fIX_1_501321110, fIX_1_847759065, fIX_1_961570560,
    fIX_2_053119869, fIX_2_562915447, fIX_3_072711026 :: Int32
fIX_0_298631336 =  2446 -- FIX(0.298631336)
fIX_0_390180644 =  3196 -- FIX(0.390180644)
fIX_0_541196100 =  4433 -- FIX(0.541196100)
fIX_0_765366865 =  6270 -- FIX(0.765366865)
fIX_0_899976223 =  7373 -- FIX(0.899976223)
fIX_1_175875602 =  9633 -- FIX(1.175875602)
fIX_1_501321110 = 12299 -- FIX(1.501321110)
fIX_1_847759065 = 15137 -- FIX(1.847759065)
fIX_1_961570560 = 16069 -- FIX(1.961570560)
fIX_2_053119869 = 16819 -- FIX(2.053119869)
fIX_2_562915447 = 20995 -- FIX(2.562915447)
fIX_3_072711026 = 25172 -- FIX(3.072711026)

-- int data[64];
fastDct :: MutableMacroBlock s Int32
        -> MutableMacroBlock s Int16
        -> ST s (MutableMacroBlock s Int32)
fastDct workData block = do
 firstPass workData  0
 secondPass workData 0
 descaleBlock workData    0
 return workData
  where -- Pass 1: process rows.
        -- Note results are scaled up by sqrt(8) compared to a true DCT;
        -- furthermore, we scale the results by 2**PASS1_BITS.
        firstPass         _ 8 = return ()
        firstPass dataBlock i = do
            let baseIdx = i * 8
            blk0 <- fromIntegral <$> block .!!!. (baseIdx + 0)
            blk1 <- fromIntegral <$> block .!!!. (baseIdx + 1)
            blk2 <- fromIntegral <$> block .!!!. (baseIdx + 2)
            blk3 <- fromIntegral <$> block .!!!. (baseIdx + 3)
            blk4 <- fromIntegral <$> block .!!!. (baseIdx + 4)
            blk5 <- fromIntegral <$> block .!!!. (baseIdx + 5)
            blk6 <- fromIntegral <$> block .!!!. (baseIdx + 6)
            blk7 <- fromIntegral <$> block .!!!. (baseIdx + 7)
            let tmp0 = blk0 + blk7 :: Int32
                tmp7 = blk0 - blk7
                tmp1 = blk1 + blk6
                tmp6 = blk1 - blk6
                tmp2 = blk2 + blk5
                tmp5 = blk2 - blk5
                tmp3 = blk3 + blk4
                tmp4 = blk3 - blk4

                -- Even part per LL&M figure 1 --- note that published figure is faulty;
                -- rotator "sqrt(2)*c1" should be "sqrt(2)*c6".
                tmp10 = tmp0 + tmp3
                tmp13 = tmp0 - tmp3
                tmp11 = tmp1 + tmp2
                tmp12 = tmp1 - tmp2

            (dataBlock .<-.  baseIdx     ) ((tmp10 + tmp11) .<<. pASS1_BITS)
            (dataBlock .<-. (baseIdx + 4)) ((tmp10 - tmp11) .<<. pASS1_BITS)
            let z1 = (tmp12 + tmp13) * fIX_0_541196100
            (dataBlock .<-. (baseIdx + 2)) $
                        descale (z1 + tmp13 * fIX_0_765366865) (cONST_BITS - pASS1_BITS)
            (dataBlock .<-. (baseIdx + 6)) $
                        descale (z1 + tmp12 * (-fIX_1_847759065)) (cONST_BITS - pASS1_BITS)

                -- Odd part per figure 8 --- note paper omits factor of sqrt(2).
                -- cK represents cos(K*pi/16).
                -- i0..i3 in the paper are tmp4..tmp7 here.
            let z1' = tmp4 + tmp7;
                z2 = tmp5 + tmp6;
                z3 = tmp4 + tmp6;
                z4 = tmp5 + tmp7;
                z5 = (z3 + z4) * fIX_1_175875602       -- sqrt(2) * c3

                tmp4' = tmp4 * fIX_0_298631336         -- sqrt(2) * (-c1+c3+c5-c7)
                tmp5' = tmp5 * fIX_2_053119869         -- sqrt(2) * ( c1+c3-c5+c7)
                tmp6' = tmp6 * fIX_3_072711026         -- sqrt(2) * ( c1+c3+c5-c7)
                tmp7' = tmp7 * fIX_1_501321110         -- sqrt(2) * ( c1+c3-c5-c7)
                z1'' = z1' * (-fIX_0_899976223)        -- sqrt(2) * (c7-c3)
                z2' = z2 * (-fIX_2_562915447)          -- sqrt(2) * (-c1-c3)
                z3' = z3 * (-fIX_1_961570560)          -- sqrt(2) * (-c3-c5)
                z4' = z4 * (-fIX_0_390180644)          -- sqrt(2) * (c5-c3)

                z3'' = z3' + z5
                z4'' = z4' + z5

            (dataBlock .<-. (baseIdx + 7)) $ 
                descale (tmp4' + z1'' + z3'') (cONST_BITS - pASS1_BITS)
            (dataBlock .<-. (baseIdx + 5)) $
                descale (tmp5' + z2' + z4'') (cONST_BITS - pASS1_BITS)
            (dataBlock .<-. (baseIdx + 3)) $
                descale (tmp6' + z2' + z3'') (cONST_BITS - pASS1_BITS)
            (dataBlock .<-. (baseIdx + 1)) $
                descale (tmp7' + z1'' + z4'') (cONST_BITS - pASS1_BITS)

            firstPass dataBlock $ i + 1


        -- Pass 2: process columns.
        -- We remove the PASS1_BITS scaling, but leave the results scaled up
        -- by an overall factor of 8.
        secondPass :: M.STVector s Int32 -> Int -> ST s ()
        secondPass _         8 = return ()
        secondPass dataBlock i = do
            data0  <- fromIntegral <$> dataBlock .!!!.  i
            data8  <- fromIntegral <$> dataBlock .!!!. (i + 8)
            data16 <- fromIntegral <$> dataBlock .!!!. (i + 16)
            data24 <- fromIntegral <$> dataBlock .!!!. (i + 24)
            data32 <- fromIntegral <$> dataBlock .!!!. (i + 32)
            data40 <- fromIntegral <$> dataBlock .!!!. (i + 40)
            data48 <- fromIntegral <$> dataBlock .!!!. (i + 48)
            data56 <- fromIntegral <$> dataBlock .!!!. (i + 56)

            let tmp0 = data0 + data56
                tmp7 = data0 - data56
                tmp1 = data8 + data48
                tmp6 = data8 - data48
                tmp2 = data16 + data40
                tmp5 = data16 - data40
                tmp3 = data24 + data32
                tmp4 = data24 - data32

                -- Even part per LL&M figure 1 --- note that published figure is faulty
                -- rotator "sqrt(2)*c1" should be "sqrt(2)*c6".
                tmp10 = tmp0 + tmp3
                tmp13 = tmp0 - tmp3
                tmp11 = tmp1 + tmp2
                tmp12 = tmp1 - tmp2

            (dataBlock .<-.  i      ) $ descale (tmp10 + tmp11) pASS1_BITS
            (dataBlock .<-. (i + 32)) $ descale (tmp10 - tmp11) pASS1_BITS

            let z1 = (tmp12 + tmp13) * fIX_0_541196100
            (dataBlock .<-. (i + 16)) $
                        descale (z1 + tmp13 * fIX_0_765366865) (cONST_BITS + pASS1_BITS)
            (dataBlock .<-. (i + 48)) $
                        descale (z1 + tmp12 * (-fIX_1_847759065)) (cONST_BITS + pASS1_BITS)

                -- Odd part per figure 8 --- note paper omits factor of sqrt(2).
                -- cK represents cos(K*pi/16).
                -- i0..i3 in the paper are tmp4..tmp7 here.
            let z1' = tmp4 + tmp7
                z2 = tmp5 + tmp6
                z3 = tmp4 + tmp6
                z4 = tmp5 + tmp7
                z5 = (z3 + z4) * fIX_1_175875602       -- sqrt(2) * c3

                tmp4' = tmp4 * fIX_0_298631336        -- sqrt(2) * (-c1+c3+c5-c7)
                tmp5' = tmp5 * fIX_2_053119869        -- sqrt(2) * ( c1+c3-c5+c7)
                tmp6' = tmp6 * fIX_3_072711026        -- sqrt(2) * ( c1+c3+c5-c7)
                tmp7' = tmp7 * fIX_1_501321110        -- sqrt(2) * ( c1+c3-c5-c7)
                z1'' = z1' * (-fIX_0_899976223)  -- sqrt(2) * (c7-c3)
                z2'  = z2 * (-fIX_2_562915447)   -- sqrt(2) * (-c1-c3)
                z3'  = z3 * (-fIX_1_961570560)   -- sqrt(2) * (-c3-c5)
                z4'  = z4 * (-fIX_0_390180644)   -- sqrt(2) * (c5-c3)

                z3'' = z3' + z5;
                z4'' = z4' + z5;

            (dataBlock .<-. (i + 56)) $
                descale (tmp4' + z1'' + z3'') (cONST_BITS + pASS1_BITS)
            (dataBlock .<-. (i + 40)) $
                descale (tmp5' + z2' + z4'') (cONST_BITS + pASS1_BITS)
            (dataBlock .<-. (i + 24)) $
                descale (tmp6' + z2' + z3'') (cONST_BITS + pASS1_BITS)
            (dataBlock .<-. (i +  8)) $
                descale (tmp7' + z1'' + z4'') (cONST_BITS + pASS1_BITS)

            secondPass dataBlock (i + 1)

        descaleBlock         _ 64 = return ()
        descaleBlock dataBlock  i = do
            val <- dataBlock .!!!. i
            (block .<-. i) . fromIntegral $ descale val 3
            descaleBlock dataBlock (i + 1)

