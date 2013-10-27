{-# LANGUAGE TupleSections #-}
module Codec.Picture.Jpg.Progressive
    ( JpgUnpackerParameter( .. )
    , progressiveUnpack 
    , prepareUnpacker
    ) where

import Control.Applicative( pure, (<$>) )
import Control.Monad( when, forM_ )
import Control.Monad.ST( ST )
import Control.Monad.Trans( lift )
import Data.Bits( (.&.), (.|.), unsafeShiftL )
import Data.Int( Int16, Int32 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V
import Data.Vector( (!) )
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Storable.Mutable as MS

import Codec.Picture.Types
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
    , readerIndex          :: !Int
    }

decodeFirstDC :: JpgUnpackerParameter
              -> MS.STVector s Int16
              -> MutableMacroBlock s Int16
              -> Int32
              -> BoolReader s Int32
decodeFirstDC params dcCoeffs block eobrun = unpack >> pure eobrun
  where unpack = do
          dcDeltaCoefficient <- dcCoefficientDecode $ dcHuffmanTree params
          previousDc <- lift $ dcCoeffs `MS.unsafeRead` componentIndex params
          let neoDcCoefficient = previousDc + dcDeltaCoefficient
              approxLow = fst $ successiveApprox params
              scaledDc = neoDcCoefficient `unsafeShiftL` approxLow
          lift $ (block `MS.unsafeWrite` 0) scaledDc 
          lift $ (dcCoeffs `MS.unsafeWrite` componentIndex params) neoDcCoefficient

decodeRefineDc :: JpgUnpackerParameter
               -> a
               -> MutableMacroBlock s Int16
               -> Int32
               -> BoolReader s Int32
decodeRefineDc params _ block eobrun = unpack >> pure eobrun
  where approxLow = fst $ successiveApprox params
        plusOne = 1 `unsafeShiftL` approxLow
        unpack = do
            bit <- getNextBitJpg
            when bit . lift $ do
                v <- block `MS.unsafeRead` 0
                (block `MS.unsafeWrite` 0) $ v .|. plusOne

decodeFirstAc :: JpgUnpackerParameter
              -> a
              -> MutableMacroBlock s Int16
              -> Int32
              -> BoolReader s Int32
decodeFirstAc _params _ _block eobrun | eobrun > 0 = pure $ eobrun - 1
decodeFirstAc params _ block _ = unpack startIndex
  where (startIndex, maxIndex) = coefficientRange params
        (low, _) = successiveApprox params
        unpack n | n > maxIndex = pure 0
        unpack n = do
            rrrrssss <- decodeRrrrSsss $ acHuffmanTree params
            case rrrrssss of
                (0xF, 0) -> unpack $ n + 16
                (  r, 0) -> eobrun <$> unpackInt r
                    where eobrun lowBits = (1 `unsafeShiftL` r) - 1 + lowBits
                (  r, s) -> do
                    let n' = n + r
                    val <- (`unsafeShiftL` low) <$> decodeInt s
                    lift . (block `MS.unsafeWrite` n') $ fromIntegral val
                    unpack $ n' + 1

decodeRefineAc :: JpgUnpackerParameter
               -> a
               -> MutableMacroBlock s Int16
               -> Int32
               -> BoolReader s Int32
decodeRefineAc _params _ _block eobrun | eobrun > 0 = pure $ eobrun - 1
decodeRefineAc params _ block _ = unpack startIndex
  where (startIndex, maxIndex) = coefficientRange params
        (low, _) = successiveApprox params
        plusOne = 1 `unsafeShiftL` low
        minusOne = 1 `unsafeShiftL` low

        getBitVal = do
            v <- getNextBitJpg
            pure $ if v then plusOne else minusOne

        unpack idx | idx > maxIndex = pure 0
        unpack idx = do
            rrrrssss <- decodeRrrrSsss $ acHuffmanTree params
            case rrrrssss of
              (0xF, 0) -> error "Dunno"
              (  r, 0) -> eobrun <$> unpackInt r
                 where eobrun lowBits = (1 `unsafeShiftL` r) + lowBits - 1
              (  r, _) -> do
                  idx' <- updateCoeffs r idx
                  val <- getBitVal
                  when (idx <= maxIndex) $
                       (lift $ (block `MS.unsafeWrite` idx') val)
                  unpack $ idx' + 1

        updateCoeffs r idx
            | r   < 0        = pure idx
            | idx > maxIndex = pure idx
        updateCoeffs r idx = do
          coeff <- lift $ block `MS.unsafeRead` idx
          if coeff /= 0 then do
            bit <- getNextBitJpg
            when (bit && coeff .&. plusOne == 0) $
                if coeff >= 0 then
                    lift . (block `MS.unsafeWrite` idx) $ coeff + plusOne
                else
                    lift . (block `MS.unsafeWrite` idx) $ coeff + minusOne
            updateCoeffs r $ idx + 1
          else
            updateCoeffs (r - 1) $ idx + 1

type Unpacker s =
    JpgUnpackerParameter -> MS.STVector s Int16 -> MutableMacroBlock s Int16 -> Int32
        -> BoolReader s Int32


prepareUnpacker :: [([JpgUnpackerParameter], L.ByteString)]
                -> ST s ( V.Vector (V.Vector (JpgUnpackerParameter, Unpacker s))
                        , M.STVector s BoolState)
prepareUnpacker lst = do
    let boolStates = V.fromList $ map snd infos
    vec <- V.unsafeThaw boolStates
    return (V.fromList $ map fst infos, vec)
  where infos = map prepare lst
        prepare ([], _) = error "progressiveUnpack, no component"
        prepare (param : xs, byteString) =
         (V.fromList $ map (,unpacker) (param:xs), boolReader)
           where unpacker = selection (successiveApprox param) (coefficientRange param)

                 boolReader = initBoolState . B.concat $ L.toChunks byteString

                 selection (_, 0) (0, _) = decodeFirstDC
                 selection (_, 0) _      = decodeFirstAc
                 selection _      (0, _) = decodeRefineDc
                 selection _      _      = decodeRefineAc

data ComponentData s = ComponentData
    { componentBlocks :: V.Vector (MutableMacroBlock s Int16)
    }

progressiveUnpack :: (Int, Int)
                  -> JpgFrameHeader
                  -> V.Vector (MacroBlock Int16)
                  -> [([JpgUnpackerParameter], L.ByteString)]
                  -> ST s (MutableImage s PixelYCbCr8)
progressiveUnpack (maxiW, maxiH) frame quants lst = do
    (unpackers, readers) <-  prepareUnpacker lst
    allBlocks <- mapM allocateWorkingBlocks $ jpgComponents frame
    dcCoeffs <- MS.replicate imgComponentCount 0
    eobRuns <- MS.replicate (length lst) 0
    workBlock <- createEmptyMutableMacroBlock
    let imgWidth = fromIntegral $ jpgWidth frame
        imgHeight = fromIntegral $ jpgHeight frame
        elementCount = imgWidth * imgHeight * fromIntegral imgComponentCount
    img <- MutableImage imgWidth imgHeight <$> MS.new elementCount

    rasterMap mcuBlockWidth mcuBlockHeight $ \mmX mmY -> do

        -- Reset all blocks to 0
        forM_ allBlocks $ V.mapM_ (flip MS.set 0) .  componentBlocks

        V.forM_ unpackers $ V.mapM_ $ \(unpackParam, unpacker) -> do
            boolState <- readers `M.read` readerIndex unpackParam
            eobrun <- eobRuns `MS.read` readerIndex unpackParam
            let mcuBlocks = allBlocks !! componentIndex unpackParam
                blockIndex =
                    componentWidth unpackParam * mcuY unpackParam
                            + mcuX unpackParam
                writeBlock = componentBlocks mcuBlocks ! blockIndex
            (eobrun', state) <-
                runBoolReaderWith boolState $
                    unpacker unpackParam dcCoeffs writeBlock eobrun

            (readers `M.write` readerIndex unpackParam) state
            (eobRuns `MS.write` readerIndex unpackParam) eobrun'

        sequence_ [decodeMacroBlock table workBlock block >>=
                      unpackMacroBlock cId imgComponentCount
                                cw8 ch8 rx ry img
                      | (cId, comp) <- zip [0..] $ jpgComponents frame
                      , let compW =
                              maxiW - fromIntegral (horizontalSamplingFactor comp) - 1
                            compH =
                              maxiH - fromIntegral (verticalSamplingFactor comp) - 1
                            cw8 = fromIntegral $ horizontalSamplingFactor comp
                            ch8 = fromIntegral $ verticalSamplingFactor comp
                     
                      , y <- [0 .. compH - 1]
                      , let ry = mmY * compH + y
                      , x <- [0 .. compW - 1]
                      , let rx = mmX * compW + x
                      , let compBlocks = componentBlocks (allBlocks !! cId)
                            block = compBlocks ! (y * compW + x)
                            table = quants ! (min 1 cId)
                      ]
    return img

  where imgComponentCount = length $ jpgComponents frame
        mcuBlockWidth = maxiW * dctBlockSize
        mcuBlockHeight = maxiH * dctBlockSize
        allocateWorkingBlocks comp = ComponentData <$> blocks 
            where hSample = fromIntegral $ horizontalSamplingFactor comp
                  vSample = fromIntegral $ verticalSamplingFactor comp
                  width = maxiW - hSample  + 1
                  height = maxiH - vSample + 1
                  blocks = V.replicateM (width * height)
                                createEmptyMutableMacroBlock
-- -}  

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

    /* If we've run out of data, don't modify the MCU.
    */
    if (! entropy->pub.insufficient_data) {

        /* Load up working state */
        BITREAD_LOAD_STATE(cinfo,entropy->bitstate);
        EOBRUN = entropy->saved.EOBRUN; /* only part of saved state we need */


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

