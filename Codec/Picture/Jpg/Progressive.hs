module Codec.Picture.Jpg.Progressive
    ( JpgUnpackerParameter( .. )
    , decodeFirstDC
    , decodeRefineDc
    ) where

{-import Control.Applicative( (<$>) )-}
import Control.Monad( when )
{-import Control.Monad.ST( ST )-}
import Control.Monad.Trans( lift )
import Data.Bits( (.|.), unsafeShiftL )
import Data.Int( Int16 )
import qualified Data.Vector.Storable.Mutable as M

import Codec.Picture.BitWriter
import Codec.Picture.Jpg.Common
import Codec.Picture.Jpg.Types
import Codec.Picture.Jpg.DefaultTable

data JpgUnpackerParameter = JpgUnpackerParameter
    { dcHuffmanTree        :: !HuffmanPackedTree
    , acHuffmanTree        :: !HuffmanPackedTree
    , quantization         :: !(MacroBlock Int16)
    , componentIndex       :: !Int
    , restartInterval      :: !Int
    , componentWidth       :: !Int
    , componentHeight      :: !Int
    , coefficientRange     :: !(Int, Int)
    , successiveApprox     :: !(Int, Int)
    , mcuX                 :: !Int
    , mcuY                 :: !Int
    }

decodeFirstDC :: JpgUnpackerParameter
              -> M.STVector s Int16
              -> MutableMacroBlock s Int16
              -> BoolReader s ()
decodeFirstDC params dcCoeffs block = unpack
  where unpack = do
          dcDeltaCoefficient <- dcCoefficientDecode $ dcHuffmanTree params
          previousDc <- lift $ dcCoeffs `M.unsafeRead` componentIndex params
          let neoDcCoefficient = previousDc + dcDeltaCoefficient
              approxLow = fst $ successiveApprox params
              scaledDc = neoDcCoefficient `unsafeShiftL` approxLow
          lift $ (block `M.unsafeWrite` 0) scaledDc 
          lift $ (dcCoeffs `M.unsafeWrite` componentIndex params) neoDcCoefficient

decodeRefineDc :: JpgUnpackerParameter
               -> MutableMacroBlock s Int16
               -> BoolReader s ()
decodeRefineDc params block = unpack
  where approxLow = fst $ successiveApprox params
        plusOne = 1 `unsafeShiftL` approxLow
        unpack = do
            bit <- getNextBitJpg
            when bit . lift $ do
                v <- block `M.unsafeRead` 0
                (block `M.unsafeWrite` 0) $ v .|. plusOne

{-
/*
 * MCU decoding for AC initial scan (either spectral selection,
 * or first pass of successive approximation).
 */

    METHODDEF(boolean)
decode_mcu_AC_first (j_decompress_ptr cinfo, JBLOCKROW *MCU_data)
{   
    phuff_entropy_ptr entropy = (phuff_entropy_ptr) cinfo->entropy;
    int Se = cinfo->Se;
    int Al = cinfo->Al;
    register int s, k, r;
    unsigned int EOBRUN;
    JBLOCKROW block;
    BITREAD_STATE_VARS;
    d_derived_tbl * tbl;

    /* Process restart marker if needed; may have to suspend */
    if (cinfo->restart_interval) {
        if (entropy->restarts_to_go == 0)
            if (! process_restart(cinfo))
                return FALSE;
    }

    /* If we've run out of data, just leave the MCU set to zeroes.
     * This way, we return uniform gray for the remainder of the segment.
     */
    if (! entropy->pub.insufficient_data) {

        /* Load up working state.
         * We can avoid loading/saving bitread state if in an EOB run.
         */
        EOBRUN = entropy->saved.EOBRUN;	/* only part of saved state we need */

        /* There is always only one block per MCU */

        if (EOBRUN > 0)		/* if it's a band of zeroes... */
            EOBRUN--;			/* ...process it now (we do nothing) */
        else {
            BITREAD_LOAD_STATE(cinfo,entropy->bitstate);
            block = MCU_data[0];
            tbl = entropy->ac_derived_tbl;

            for (k = cinfo->Ss; k <= Se; k++) {
                HUFF_DECODE(s, br_state, tbl, return FALSE, label2);
                r = s >> 4;
                s &= 15;
                if (s) {
                    k += r;
                    CHECK_BIT_BUFFER(br_state, s, return FALSE);
                    r = GET_BITS(s);
                    s = HUFF_EXTEND(r, s);
                    /* Scale and output coefficient in natural (dezigzagged) order */
                    (*block)[jpeg_natural_order[k]] = (JCOEF) (s << Al);
                } else {
                    if (r == 15) {	/* ZRL */
                        k += 15;		/* skip 15 zeroes in band */
                    } else {		/* EOBr, run length is 2^r + appended bits */
                        EOBRUN = 1 << r;
                        if (r) {		/* EOBr, r > 0 */
                            CHECK_BIT_BUFFER(br_state, r, return FALSE);
                            r = GET_BITS(r);
                            EOBRUN += r;
                        }
                        EOBRUN--;		/* this band is processed at this moment */
                        break;		/* force end-of-band */
                    }
                }
            }

            BITREAD_SAVE_STATE(cinfo,entropy->bitstate);
        }

        /* Completed MCU, so update state */
        entropy->saved.EOBRUN = EOBRUN;	/* only part of saved state we need */
    }

    /* Account for restart interval (no-op if not using restarts) */
    entropy->restarts_to_go--;

    return TRUE;
}
-}


{- 
/*
 * MCU decoding for AC successive approximation refinement scan.
 */

    METHODDEF(boolean)
decode_mcu_AC_refine (j_decompress_ptr cinfo, JBLOCKROW *MCU_data)
{   
    phuff_entropy_ptr entropy = (phuff_entropy_ptr) cinfo->entropy;
    int Se = cinfo->Se;
    int p1 = 1 << cinfo->Al;	/* 1 in the bit position being coded */
    int m1 = (-1) << cinfo->Al;	/* -1 in the bit position being coded */
    register int s, k, r;
    unsigned int EOBRUN;
    JBLOCKROW block;
    JCOEFPTR thiscoef;
    BITREAD_STATE_VARS;
    d_derived_tbl * tbl;
    int num_newnz;
    int newnz_pos[DCTSIZE2];

    /* Process restart marker if needed; may have to suspend */
    if (cinfo->restart_interval) {
        if (entropy->restarts_to_go == 0)
            if (! process_restart(cinfo))
                return FALSE;
    }

    /* If we've run out of data, don't modify the MCU.
    */
    if (! entropy->pub.insufficient_data) {

        /* Load up working state */
        BITREAD_LOAD_STATE(cinfo,entropy->bitstate);
        EOBRUN = entropy->saved.EOBRUN; /* only part of saved state we need */

        /* There is always only one block per MCU */
        block = MCU_data[0];
        tbl = entropy->ac_derived_tbl;

        /* If we are forced to suspend, we must undo the assignments to any newly
         * nonzero coefficients in the block, because otherwise we'd get confused
         * next time about which coefficients were already nonzero.
         * But we need not undo addition of bits to already-nonzero coefficients;
         * instead, we can test the current bit to see if we already did it.
         */
        num_newnz = 0;

        /* initialize coefficient loop counter to start of band */
        k = cinfo->Ss;

        if (EOBRUN == 0) {
            for (; k <= Se; k++) {
                HUFF_DECODE(s, br_state, tbl, goto undoit, label3);
                r = s >> 4;
                s &= 15;
                if (s) {
                    if (s != 1)		/* size of new coef should always be 1 */
                        WARNMS(cinfo, JWRN_HUFF_BAD_CODE);
                    CHECK_BIT_BUFFER(br_state, 1, goto undoit);
                    if (GET_BITS(1))
                        s = p1;		/* newly nonzero coef is positive */
                    else
                        s = m1;		/* newly nonzero coef is negative */
                } else {
                    if (r != 15) {
                        EOBRUN = 1 << r;	/* EOBr, run length is 2^r + appended bits */
                        if (r) {
                            CHECK_BIT_BUFFER(br_state, r, goto undoit);
                            r = GET_BITS(r);
                            EOBRUN += r;
                        }
                        break;		/* rest of block is handled by EOB logic */
                    }
                    /* note s = 0 for processing ZRL */
                }
                /* Advance over already-nonzero coefs and r still-zero coefs,
                 * appending correction bits to the nonzeroes.  A correction bit is 1
                 * if the absolute value of the coefficient must be increased.
                 */
                do {
                    thiscoef = *block + jpeg_natural_order[k];
                    if (*thiscoef != 0) {
                        CHECK_BIT_BUFFER(br_state, 1, goto undoit);
                        if (GET_BITS(1)) {
                            if ((*thiscoef & p1) == 0) { /* do nothing if already set it */
                                if (*thiscoef >= 0)
                                    *thiscoef += p1;
                                else
                                    *thiscoef += m1;
                            }
                        }
                    } else {
                        if (--r < 0)
                            break;		/* reached target zero coefficient */
                    }
                    k++;
                } while (k <= Se);
                if (s) {
                    int pos = jpeg_natural_order[k];
                    /* Output newly nonzero coefficient */
                    (*block)[pos] = (JCOEF) s;
                    /* Remember its position in case we have to suspend */
                    newnz_pos[num_newnz++] = pos;
                }
            }
        }

        if (EOBRUN > 0) {
            /* Scan any remaining coefficient positions after the end-of-band
             * (the last newly nonzero coefficient, if any).  Append a correction
             * bit to each already-nonzero coefficient.  A correction bit is 1
             * if the absolute value of the coefficient must be increased.
             */
            for (; k <= Se; k++) {
                thiscoef = *block + jpeg_natural_order[k];
                if (*thiscoef != 0) {
                    CHECK_BIT_BUFFER(br_state, 1, goto undoit);
                    if (GET_BITS(1)) {
                        if ((*thiscoef & p1) == 0) { /* do nothing if already changed it */
                            if (*thiscoef >= 0)
                                *thiscoef += p1;
                            else
                                *thiscoef += m1;
                        }
                    }
                }
            }
            /* Count one block completed in EOB run */
            EOBRUN--;
        }

        /* Completed MCU, so update state */
        BITREAD_SAVE_STATE(cinfo,entropy->bitstate);
        entropy->saved.EOBRUN = EOBRUN; /* only part of saved state we need */
    }

    /* Account for restart interval (no-op if not using restarts) */
    entropy->restarts_to_go--;

    return TRUE;

undoit:
    /* Re-zero any output coefficients that we made newly nonzero */
    while (num_newnz > 0)
        (*block)[newnz_pos[--num_newnz]] = 0;

    return FALSE;
}
-}

