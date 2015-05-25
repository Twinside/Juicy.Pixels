{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
module Codec.Picture.Jpg.FastDct( referenceDct, fastDctLibJpeg ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>) )
#endif

import Data.Int( Int16, Int32 )
import Data.Bits( unsafeShiftR, unsafeShiftL )
import Control.Monad.ST( ST )

import qualified Data.Vector.Storable.Mutable as M

import Codec.Picture.Jpg.Types
import Control.Monad( forM, forM_ )

-- | Reference implementation of the DCT, directly implementing the formula
-- of ITU-81. It's slow as hell, perform to many operations, but is accurate
-- and a good reference point.
referenceDct :: MutableMacroBlock s Int32
             -> MutableMacroBlock s Int16
             -> ST s (MutableMacroBlock s Int32)
referenceDct workData block = do
  forM_ [(u, v) | u <- [0 :: Int .. dctBlockSize - 1], v <- [0..dctBlockSize - 1]] $ \(u,v) -> do
    val <- at (u,v)
    (workData `M.unsafeWrite` (v * dctBlockSize + u)) . truncate $ (1 / 4) * c u * c v * val

  return workData
 where -- at :: (Int, Int) -> ST s Float
   at (u,v) = do
     toSum <-
        forM [(x,y) | x <- [0..dctBlockSize - 1], y <- [0..dctBlockSize - 1 :: Int]] $ \(x,y) -> do
            sample <- fromIntegral <$> (block `M.unsafeRead` (y * dctBlockSize + x))
            return $ sample * cos ((2 * fromIntegral x + 1) * fromIntegral u * (pi :: Float)/ 16) 
                            * cos ((2 * fromIntegral y + 1) * fromIntegral v * pi / 16)
     return $ sum toSum

   c 0 = 1 / sqrt 2
   c _ = 1

pASS1_BITS, cONST_BITS :: Int
cONST_BITS = 13
pASS1_BITS =  2


fIX_0_298631336, fIX_0_390180644, fIX_0_541196100,
    fIX_0_765366865, fIX_0_899976223, fIX_1_175875602,
    fIX_1_501321110, fIX_1_847759065, fIX_1_961570560,
    fIX_2_053119869, fIX_2_562915447, fIX_3_072711026 :: Int32
fIX_0_298631336 = 2446    -- FIX(0.298631336) */
fIX_0_390180644 = 3196    -- FIX(0.390180644) */
fIX_0_541196100 = 4433    -- FIX(0.541196100) */
fIX_0_765366865 = 6270    -- FIX(0.765366865) */
fIX_0_899976223 = 7373    -- FIX(0.899976223) */
fIX_1_175875602 = 9633    -- FIX(1.175875602) */
fIX_1_501321110 = 12299    -- FIX(1.501321110) */
fIX_1_847759065 = 15137    -- FIX(1.847759065) */
fIX_1_961570560 = 16069    -- FIX(1.961570560) */
fIX_2_053119869 = 16819    -- FIX(2.053119869) */
fIX_2_562915447 = 20995    -- FIX(2.562915447) */
fIX_3_072711026 = 25172    -- FIX(3.072711026) */

cENTERJSAMPLE :: Int32
cENTERJSAMPLE = 128

-- | Fast DCT extracted from libjpeg
fastDctLibJpeg :: MutableMacroBlock s Int32
        -> MutableMacroBlock s Int16
        -> ST s (MutableMacroBlock s Int32)
fastDctLibJpeg workData sample_block = do
 firstPass workData  0
 secondPass workData 7
 {-_ <- mutate (\_ a -> a `quot` 8) workData-}
 return workData
  where -- Pass 1: process rows.
        -- Note results are scaled up by sqrt(8) compared to a true DCT;
        -- furthermore, we scale the results by 2**PASS1_BITS.
        firstPass         _ i | i == dctBlockSize = return ()
        firstPass dataBlock i = do
            let baseIdx = i * dctBlockSize
                readAt idx = fromIntegral <$> sample_block `M.unsafeRead` (baseIdx + idx)
                mult = (*)
                writeAt idx = dataBlock `M.unsafeWrite` (baseIdx + idx)
                writeAtPos idx n = (dataBlock `M.unsafeWrite` (baseIdx + idx))
                                    (n `unsafeShiftR` (cONST_BITS - pASS1_BITS))

            blk0 <- readAt 0
            blk1 <- readAt 1
            blk2 <- readAt 2
            blk3 <- readAt 3
            blk4 <- readAt 4
            blk5 <- readAt 5
            blk6 <- readAt 6
            blk7 <- readAt 7

            let tmp0 = blk0 + blk7
                tmp1 = blk1 + blk6
                tmp2 = blk2 + blk5
                tmp3 = blk3 + blk4

                tmp10 = tmp0 + tmp3
                tmp12 = tmp0 - tmp3
                tmp11 = tmp1 + tmp2
                tmp13 = tmp1 - tmp2

                tmp0' = blk0 - blk7
                tmp1' = blk1 - blk6
                tmp2' = blk2 - blk5
                tmp3' = blk3 - blk4

            -- Stage 4 and output
            writeAt 0 $ (tmp10 + tmp11 - dctBlockSize * cENTERJSAMPLE) `unsafeShiftL` pASS1_BITS
            writeAt 4 $ (tmp10 - tmp11) `unsafeShiftL` pASS1_BITS

            let z1 = mult (tmp12 + tmp13) fIX_0_541196100
                     + (1 `unsafeShiftL` (cONST_BITS - pASS1_BITS - 1))

            writeAtPos 2 $ z1 + mult tmp12 fIX_0_765366865
            writeAtPos 6 $ z1 - mult tmp13 fIX_1_847759065

            let tmp10' = tmp0' + tmp3'
                tmp11' = tmp1' + tmp2'
                tmp12' = tmp0' + tmp2'
                tmp13' = tmp1' + tmp3'
                z1' = mult (tmp12' + tmp13') fIX_1_175875602 --  c3 */
                        -- Add fudge factor here for final descale. */
                        + (1 `unsafeShiftL` (cONST_BITS - pASS1_BITS-1))
                tmp0'' = mult tmp0' fIX_1_501321110
                tmp1'' = mult tmp1' fIX_3_072711026
                tmp2'' = mult tmp2' fIX_2_053119869
                tmp3'' = mult tmp3' fIX_0_298631336

                tmp10'' = mult tmp10' (- fIX_0_899976223)
                tmp11'' = mult tmp11' (- fIX_2_562915447)
                tmp12'' = mult tmp12' (- fIX_0_390180644) + z1'
                tmp13'' = mult tmp13' (- fIX_1_961570560) + z1'

            writeAtPos 1 $ tmp0'' + tmp10'' + tmp12''
            writeAtPos 3 $ tmp1'' + tmp11'' + tmp13''
            writeAtPos 5 $ tmp2'' + tmp11'' + tmp12''
            writeAtPos 7 $ tmp3'' + tmp10'' + tmp13''

            firstPass dataBlock $ i + 1

        -- Pass 2: process columns.
        -- We remove the PASS1_BITS scaling, but leave the results scaled up
        -- by an overall factor of 8.
        secondPass :: M.STVector s Int32 -> Int -> ST s ()
        secondPass _     (-1) = return ()
        secondPass block i = do
            let readAt idx = block `M.unsafeRead` ((7 - i) + idx * dctBlockSize)
                mult = (*)
                writeAt idx = block `M.unsafeWrite` (dctBlockSize * idx + (7 - i))
                writeAtPos idx n = (block `M.unsafeWrite` (dctBlockSize * idx + (7 - i))) $ n `unsafeShiftR` (cONST_BITS + pASS1_BITS + 3)
            blk0 <- readAt 0
            blk1 <- readAt 1
            blk2 <- readAt 2
            blk3 <- readAt 3
            blk4 <- readAt 4
            blk5 <- readAt 5
            blk6 <- readAt 6
            blk7 <- readAt 7

            let tmp0 = blk0 + blk7
                tmp1 = blk1 + blk6
                tmp2 = blk2 + blk5
                tmp3 = blk3 + blk4

                -- Add fudge factor here for final descale. */
                tmp10 = tmp0 + tmp3 + (1 `unsafeShiftL` (pASS1_BITS-1))
                tmp12 = tmp0 - tmp3
                tmp11 = tmp1 + tmp2
                tmp13 = tmp1 - tmp2

                tmp0' = blk0 - blk7
                tmp1' = blk1 - blk6
                tmp2' = blk2 - blk5
                tmp3' = blk3 - blk4

            writeAt 0 $ (tmp10 + tmp11) `unsafeShiftR` (pASS1_BITS + 3)
            writeAt 4 $ (tmp10 - tmp11) `unsafeShiftR` (pASS1_BITS + 3)

            let z1 = mult (tmp12 + tmp13) fIX_0_541196100
                    + (1 `unsafeShiftL` (cONST_BITS + pASS1_BITS - 1))

            writeAtPos 2 $ z1 + mult tmp12 fIX_0_765366865
            writeAtPos 6 $ z1 - mult tmp13 fIX_1_847759065

            let tmp10' = tmp0' + tmp3'
                tmp11' = tmp1' + tmp2'
                tmp12' = tmp0' + tmp2'
                tmp13' = tmp1' + tmp3'

                z1' = mult (tmp12' + tmp13') fIX_1_175875602
                    -- Add fudge factor here for final descale. */
                   + 1 `unsafeShiftL` (cONST_BITS+pASS1_BITS-1);

                tmp0''  = mult tmp0'    fIX_1_501321110
                tmp1''  = mult tmp1'    fIX_3_072711026
                tmp2''  = mult tmp2'    fIX_2_053119869
                tmp3''  = mult tmp3'    fIX_0_298631336
                tmp10'' = mult tmp10' (- fIX_0_899976223)
                tmp11'' = mult tmp11' (- fIX_2_562915447)
                tmp12'' = mult tmp12' (- fIX_0_390180644)
                            + z1'
                tmp13'' = mult tmp13' (- fIX_1_961570560)
                            + z1'
            writeAtPos 1 $ tmp0'' + tmp10'' + tmp12''
            writeAtPos 3 $ tmp1'' + tmp11'' + tmp13''
            writeAtPos 5 $ tmp2'' + tmp11'' + tmp12''
            writeAtPos 7 $ tmp3'' + tmp10'' + tmp13''

            secondPass block (i - 1)

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}

