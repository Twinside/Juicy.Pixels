{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Codec.Picture.Jpg.Progressive
    ( JpgUnpackerParameter( .. )
    , progressiveUnpack
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( pure, (<$>) )
#endif

import Control.Monad( when, unless, forM_ )
import Control.Monad.ST( ST )
import Control.Monad.Trans( lift )
import Data.Bits( (.&.), (.|.), unsafeShiftL )
import Data.Int( Int16, Int32 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Vector( (!) )
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Storable.Mutable as MS

import Codec.Picture.Types
import Codec.Picture.BitWriter
import Codec.Picture.Jpg.Common
import Codec.Picture.Jpg.Types
import Codec.Picture.Jpg.DefaultTable

createMcuLineIndices :: JpgComponent -> Int -> Int -> V.Vector (VS.Vector Int)
createMcuLineIndices param imgWidth mcuWidth =
 V.fromList $ VS.fromList <$> [indexSolo, indexMulti]
  where compW = fromIntegral $ horizontalSamplingFactor param
        compH = fromIntegral $ verticalSamplingFactor param
        imageBlockSize = toBlockSize imgWidth

        indexSolo = take (imageBlockSize * compH) [0 ..]
        indexMulti = 
            [(mcu + y * mcuWidth) * compW + x
                | mcu <- [0 .. mcuWidth - 1]
                , y <- [0 .. compH - 1]
                , x <- [0 .. compW - 1] ]

decodeFirstDC :: JpgUnpackerParameter
              -> MS.STVector s Int16
              -> MutableMacroBlock s Int16
              -> Int32
              -> BoolReader s Int32
decodeFirstDC params dcCoeffs block eobrun = unpack >> pure eobrun
  where unpack = do
          (dcDeltaCoefficient) <- dcCoefficientDecode $ dcHuffmanTree params
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
                (  0, 0) -> return 0
                (  r, 0) -> eobrun <$> unpackInt r
                    where eobrun lowBits = (1 `unsafeShiftL` r) - 1 + lowBits
                (  r, s) -> do
                    let n' = n + r
                    val <- (`unsafeShiftL` low) <$> decodeInt s
                    lift . (block `MS.unsafeWrite` n') $ fromIntegral val
                    unpack $ n' + 1

decodeRefineAc :: forall a s. JpgUnpackerParameter
               -> a
               -> MutableMacroBlock s Int16
               -> Int32
               -> BoolReader s Int32
decodeRefineAc params _ block eobrun
        | eobrun == 0 = unpack startIndex
        | otherwise   = performEobRun startIndex >> return (eobrun - 1)
  where (startIndex, maxIndex) = coefficientRange params
        (low, _) = successiveApprox params
        plusOne = 1 `unsafeShiftL` low
        minusOne = (-1) `unsafeShiftL` low

        getBitVal = do
            v <- getNextBitJpg
            pure $ if v then plusOne else minusOne

        performEobRun idx | idx > maxIndex = pure ()
        performEobRun idx = do
          coeff <- lift $ block `MS.unsafeRead` idx
          if coeff /= 0 then do
            bit <- getNextBitJpg
            case (bit, (coeff .&. plusOne) == 0) of
               (False, _)    -> performEobRun $ idx + 1
               (True, False) -> performEobRun $ idx + 1
               (True, True) -> do
                   let newVal | coeff >= 0 = coeff + plusOne
                              | otherwise = coeff + minusOne
                   lift $ (block `MS.unsafeWrite` idx) newVal
                   performEobRun $ idx + 1
          else
            performEobRun $ idx + 1
                   
        unpack idx | idx > maxIndex = pure 0
        unpack idx = do
            rrrrssss <- decodeRrrrSsss $ acHuffmanTree params
            case rrrrssss of
              (0xF, 0) -> do
                idx' <- updateCoeffs 0xF idx
                unpack $ idx' + 1

              (  r, 0) -> do
                  lowBits <- unpackInt r
                  let newEobRun = (1 `unsafeShiftL` r) + lowBits - 1
                  performEobRun idx
                  pure newEobRun
                         
              (  r, _) -> do
                  val <- getBitVal
                  idx' <- updateCoeffs (fromIntegral r) idx
                  when (idx' <= maxIndex) $
                       lift $ (block `MS.unsafeWrite` idx') val
                  unpack $ idx' + 1

        updateCoeffs :: Int -> Int -> BoolReader s Int
        updateCoeffs r idx
            | r   < 0        = pure $ idx - 1
            | idx > maxIndex = pure idx
        updateCoeffs r idx = do
          coeff <- lift $ block `MS.unsafeRead` idx
          if coeff /= 0 then do
            bit <- getNextBitJpg
            when (bit && coeff .&. plusOne == 0) $ do
              let writeCoeff | coeff >= 0 = coeff + plusOne
                             | otherwise = coeff + minusOne
              lift $ (block `MS.unsafeWrite` idx) writeCoeff
            updateCoeffs r $ idx + 1
          else
            updateCoeffs (r - 1) $ idx + 1

type Unpacker s =
    JpgUnpackerParameter -> MS.STVector s Int16 -> MutableMacroBlock s Int16 -> Int32
        -> BoolReader s Int32


prepareUnpacker :: [([(JpgUnpackerParameter, a)], L.ByteString)]
                -> ST s ( V.Vector (V.Vector (JpgUnpackerParameter, Unpacker s))
                        , M.STVector s BoolState)
prepareUnpacker lst = do
    let boolStates = V.fromList $ map snd infos
    vec <- V.unsafeThaw boolStates
    return (V.fromList $ map fst infos, vec)
  where infos = map prepare lst
        prepare ([], _) = error "progressiveUnpack, no component"
        prepare (whole@((param, _) : _) , byteString) =
         (V.fromList $ map (\(p,_) -> (p, unpacker)) whole, boolReader)
           where unpacker = selection (successiveApprox param) (coefficientRange param)
                 boolReader = initBoolStateJpg . B.concat $ L.toChunks byteString

                 selection (_, 0) (0, _) = decodeFirstDC
                 selection (_, 0) _      = decodeFirstAc
                 selection _      (0, _) = decodeRefineDc
                 selection _      _      = decodeRefineAc

data ComponentData s = ComponentData
  { componentIndices    :: V.Vector (VS.Vector Int)
  , componentBlocks     :: V.Vector (MutableMacroBlock s Int16)
  , componentId         :: !Int
  , componentBlockCount :: !Int
  }

-- | Iteration from 0 to n in monadic context, without data
-- keeping.
lineMap :: (Monad m) => Int -> (Int -> m ()) -> m ()
{-# INLINE lineMap #-}
lineMap count f = go 0
  where go n | n >= count = return ()
        go n = f n >> go (n + 1)

progressiveUnpack :: (Int, Int)
                  -> JpgFrameHeader
                  -> V.Vector (MacroBlock Int16)
                  -> [([(JpgUnpackerParameter, a)], L.ByteString)]
                  -> ST s (MutableImage s PixelYCbCr8)
progressiveUnpack (maxiW, maxiH) frame quants lst = do
    (unpackers, readers) <- prepareUnpacker lst
    allBlocks <- mapM allocateWorkingBlocks . zip [0..] $ jpgComponents frame
                    :: ST s [ComponentData s]
    let scanCount = length lst
        restartIntervalValue = case lst of
                ((p,_):_,_): _ -> restartInterval p
                _ -> -1
    dcCoeffs <- MS.replicate imgComponentCount 0
    eobRuns <- MS.replicate (length lst) 0
    workBlock <- createEmptyMutableMacroBlock
    writeIndices <- MS.replicate imgComponentCount (0 :: Int)
    restartIntervals <- MS.replicate scanCount restartIntervalValue
    let elementCount = imgWidth * imgHeight * fromIntegral imgComponentCount
    img <- MutableImage imgWidth imgHeight <$> MS.replicate elementCount 128

    let processRestartInterval =
          forM_ [0 .. scanCount - 1] $ \ix -> do
            v <- restartIntervals `MS.read` ix
            if v == 0 then do
              -- reset DC prediction
              when (ix == 0) (MS.set dcCoeffs 0)
              reader <- readers `M.read` ix
              (_, updated) <- runBoolReaderWith reader $
                  byteAlignJpg >> decodeRestartInterval
              (readers `M.write` ix) updated 
              (eobRuns `MS.unsafeWrite` ix) 0
              (restartIntervals `MS.unsafeWrite` ix) $ restartIntervalValue - 1
            else
              (restartIntervals `MS.unsafeWrite` ix) $ v - 1


    lineMap imageMcuHeight $ \mmY -> do
      -- Reset all blocks to 0
      forM_ allBlocks $ V.mapM_ (`MS.set` 0) .  componentBlocks
      MS.set writeIndices 0

      lineMap imageMcuWidth $ \_mmx -> do
        processRestartInterval 
        V.forM_ unpackers $ V.mapM_ $ \(unpackParam, unpacker) -> do
            boolState <- readers `M.read` readerIndex unpackParam
            eobrun <- eobRuns `MS.read` readerIndex unpackParam
            let componentNumber = componentIndex unpackParam
            writeIndex <- writeIndices `MS.read` componentNumber
            let componentData = allBlocks !! componentNumber
                -- We get back the correct block indices for the number of component
                -- in the current scope (precalculated)
                indexVector =
                    componentIndices componentData ! indiceVector unpackParam
                maxIndexLength = VS.length indexVector
            unless (writeIndex + blockIndex unpackParam >= maxIndexLength) $ do
               let realIndex = indexVector VS.! (writeIndex + blockIndex unpackParam)
                   writeBlock = componentBlocks componentData ! realIndex
               (eobrun', state) <-
                   runBoolReaderWith boolState $
                       unpacker unpackParam dcCoeffs writeBlock eobrun

               (readers `M.write` readerIndex unpackParam) state
               (eobRuns `MS.write` readerIndex unpackParam) eobrun'

        -- Update the write indices
        forM_ allBlocks $ \comp -> do
          writeIndex <- writeIndices `MS.read` componentId comp
          let newIndex = writeIndex + componentBlockCount comp
          (writeIndices `MS.write` componentId comp) newIndex

      forM_ allBlocks $ \compData -> do
        let compBlocks = componentBlocks compData
            cId = componentId compData
            comp = jpgComponents frame !! cId
            quantId =
                fromIntegral $ quantizationTableDest comp
            table = quants ! min 3 quantId
            compW = fromIntegral $ horizontalSamplingFactor comp
            compH = fromIntegral $ verticalSamplingFactor comp
            cw8 = maxiW - fromIntegral (horizontalSamplingFactor comp) + 1
            ch8 = maxiH - fromIntegral (verticalSamplingFactor comp) + 1

        rasterMap (imageMcuWidth * compW) compH $ \rx y -> do
            let ry = mmY * maxiH + y
                block = compBlocks ! (y * imageMcuWidth * compW + rx)
            transformed <- decodeMacroBlock table workBlock block
            unpackMacroBlock imgComponentCount
                    cw8 ch8 cId (rx * cw8) ry
                    img transformed

    return img

  where imgComponentCount = length $ jpgComponents frame

        imgWidth = fromIntegral $ jpgWidth frame
        imgHeight = fromIntegral $ jpgHeight frame

        imageBlockWidth = toBlockSize imgWidth
        imageBlockHeight = toBlockSize imgHeight

        imageMcuWidth =  (imageBlockWidth + (maxiW - 1)) `div` maxiW
        imageMcuHeight = (imageBlockHeight + (maxiH - 1)) `div` maxiH

        allocateWorkingBlocks (ix, comp) = do
            let blockCount = hSample * vSample * imageMcuWidth * 2
            blocks <- V.replicateM blockCount createEmptyMutableMacroBlock
            return ComponentData 
                { componentBlocks = blocks
                , componentIndices = createMcuLineIndices comp imgWidth imageMcuWidth
                , componentBlockCount = hSample * vSample
                , componentId = ix
                }
            where hSample = fromIntegral $ horizontalSamplingFactor comp
                  vSample = fromIntegral $ verticalSamplingFactor comp

