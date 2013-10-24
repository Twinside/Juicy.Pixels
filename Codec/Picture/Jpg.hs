{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fspec-constr-count=5 #-}
-- | Module used for JPEG file loading and writing.
module Codec.Picture.Jpg( decodeJpeg
                        , encodeJpegAtQuality
                        , encodeJpeg

                        , jpgMachineStep
                        , JpgDecoderState( JpgDecoderState )
                        ) where

import Control.Arrow( (>>>) )
import Control.Applicative( pure, (<$>) )
import Control.Monad( when, forM_, foldM_ )
import Control.Monad.ST( ST, runST )
import Control.Monad.Trans( lift )
import Control.Monad.Trans.RWS( RWS, modify, tell, gets )
import qualified Control.Monad.Trans.State.Strict as S

import Data.Maybe( fromJust )
import Data.List( find )
import Data.Bits( (.|.), unsafeShiftL )
import Data.Int( Int16, Int32 )
import Data.Word(Word8, Word16, Word32)
import Data.Binary( Binary(..), encode )

import Data.Vector( (//) )
import Data.Vector.Unboxed( (!) )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as M
-- import Data.Array.Unboxed( Array, UArray, elems, listArray, (!) )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Codec.Picture.InternalHelper
import Codec.Picture.BitWriter
import Codec.Picture.Types
import Codec.Picture.Jpg.Types
import Codec.Picture.Jpg.Common
import Codec.Picture.Jpg.Progressive
import Codec.Picture.Jpg.DefaultTable
import Codec.Picture.Jpg.FastDct

quantize :: MacroBlock Int16 -> MutableMacroBlock s Int32
         -> ST s (MutableMacroBlock s Int32)
quantize table block = update 0
  where update 64 = return block
        update idx = do
            val <- block `M.unsafeRead` idx
            let q = fromIntegral (table `VS.unsafeIndex` idx)
                finalValue = (val + (q `div` 2)) `quot` q -- rounded integer division
            (block `M.unsafeWrite` idx) finalValue
            update $ idx + 1


-- | This is one of the most important function of the decoding,
-- it form the barebone decoding pipeline for macroblock. It's all
-- there is to know for macro block transformation
decodeMacroBlock :: MacroBlock DctCoefficients
                 -> MutableMacroBlock s Int16
                 -> MutableMacroBlock s Int16
                 -> ST s (MutableMacroBlock s Int16)
decodeMacroBlock quantizationTable zigZagBlock block =
    deQuantize quantizationTable block >>= zigZagReorder zigZagBlock
                                       >>= inverseDirectCosineTransform

powerOf :: Int32 -> Word32
powerOf 0 = 0
powerOf n = limit 1 0
    where val = abs n
          limit range i | val < range = i
          limit range i = limit (2 * range) (i + 1)

encodeInt :: BoolWriteStateRef s -> Word32 -> Int32 -> ST s ()
{-# INLINE encodeInt #-}
encodeInt st ssss n | n > 0 = writeBits' st (fromIntegral n) (fromIntegral ssss)
encodeInt st ssss n         = writeBits' st (fromIntegral $ n - 1) (fromIntegral ssss)

-- | Assume the macro block is initialized with zeroes
acCoefficientsDecode :: HuffmanPackedTree -> MutableMacroBlock s Int16
                     -> BoolReader s (MutableMacroBlock s Int16)
acCoefficientsDecode acTree mutableBlock = parseAcCoefficient 1 >> return mutableBlock
  where parseAcCoefficient n | n >= 64 = return ()
                             | otherwise = do
            rrrrssss <- decodeRrrrSsss acTree
            case rrrrssss of
                (  0, 0) -> return ()
                (0xF, 0) -> parseAcCoefficient (n + 16)
                (rrrr, ssss) -> do
                    decoded <- fromIntegral <$> decodeInt ssss
                    lift $ (mutableBlock `M.unsafeWrite` (n + rrrr)) decoded
                    parseAcCoefficient (n + rrrr + 1)

{-
decodeEOB :: HuffmanTreeInfo -> BoolReader s Int32
decodeEOB tree = do
    rrrrssss <- huffmanPackedDecode tree
    -}

-- | Decompress a macroblock from a bitstream given the current configuration
-- from the frame.
decompressMacroBlock :: HuffmanPackedTree   -- ^ Tree used for DC coefficient
                     -> HuffmanPackedTree   -- ^ Tree used for Ac coefficient
                     -> MacroBlock Int16    -- ^ Current quantization table
                     -> MutableMacroBlock s Int16    -- ^ A zigzag table, to avoid allocation
                     -> DcCoefficient       -- ^ Previous dc value
                     -> BoolReader s (DcCoefficient, MutableMacroBlock s Int16)
decompressMacroBlock dcTree acTree quantizationTable zigzagBlock previousDc = do
    dcDeltaCoefficient <- dcCoefficientDecode dcTree
    block <- lift createEmptyMutableMacroBlock
    let neoDcCoefficient = previousDc + dcDeltaCoefficient
    lift $ (block `M.unsafeWrite` 0) neoDcCoefficient
    fullBlock <- acCoefficientsDecode acTree block
    decodedBlock <- lift $ decodeMacroBlock quantizationTable zigzagBlock fullBlock
    return (neoDcCoefficient, decodedBlock)

gatherQuantTables :: JpgImage -> [JpgQuantTableSpec]
gatherQuantTables img = concat [t | JpgQuantTable t <- jpgFrame img]

gatherHuffmanTables :: JpgImage -> [(JpgHuffmanTableSpec, HuffmanPackedTree   )]
gatherHuffmanTables img =
    concat [lst | JpgHuffmanTable lst <- jpgFrame img] ++ defaultHuffmanTables


gatherScanInfo :: JpgImage -> (JpgFrameKind, JpgFrameHeader)
gatherScanInfo img = fromJust $ unScan <$> find scanDesc (jpgFrame img)
    where scanDesc (JpgScans _ _) = True
          scanDesc _ = False

          unScan (JpgScans a b) = (a,b)
          unScan _ = error "If this can happen, the JPEG image is ill-formed"

pixelClamp :: Int16 -> Word8
pixelClamp n = fromIntegral . min 255 $ max 0 n

unpack444Y :: Int -- ^ x
           -> Int -- ^ y
           -> MutableImage s PixelYCbCr8
           -> MutableMacroBlock s Int16
           -> ST s ()
unpack444Y x y (MutableImage { mutableImageWidth = imgWidth, mutableImageData = img })
                 block = blockVert baseIdx 0 zero
  where zero = 0 :: Int
        baseIdx = x * dctBlockSize + y * dctBlockSize * imgWidth

        blockVert        _       _ j | j >= dctBlockSize = return ()
        blockVert writeIdx readingIdx j = blockHoriz writeIdx readingIdx zero
          where blockHoriz   _ readIdx i | i >= dctBlockSize = blockVert (writeIdx + imgWidth) readIdx $ j + 1
                blockHoriz idx readIdx i = do
                    val <- pixelClamp <$> (block `M.unsafeRead` readIdx)
                    (img `M.unsafeWrite` idx) val
                    blockHoriz (idx + 1) (readIdx + 1) $ i + 1

unpack444Ycbcr :: Int -- ^ Component index
              -> Int -- ^ x
              -> Int -- ^ y
              -> MutableImage s PixelYCbCr8
              -> MutableMacroBlock s Int16
              -> ST s ()
unpack444Ycbcr compIdx x y
                 (MutableImage { mutableImageWidth = imgWidth, mutableImageData = img })
                 block = blockVert baseIdx 0 zero
  where zero = 0 :: Int
        baseIdx = (x * dctBlockSize + y * dctBlockSize * imgWidth) * 3 + compIdx

        blockVert   _       _ j | j >= dctBlockSize = return ()
        blockVert idx readIdx j = do
            val0 <- pixelClamp <$> (block `M.unsafeRead` readIdx)
            val1 <- pixelClamp <$> (block `M.unsafeRead` (readIdx + 1))
            val2 <- pixelClamp <$> (block `M.unsafeRead` (readIdx + 2))
            val3 <- pixelClamp <$> (block `M.unsafeRead` (readIdx + 3))
            val4 <- pixelClamp <$> (block `M.unsafeRead` (readIdx + 4))
            val5 <- pixelClamp <$> (block `M.unsafeRead` (readIdx + 5))
            val6 <- pixelClamp <$> (block `M.unsafeRead` (readIdx + 6))
            val7 <- pixelClamp <$> (block `M.unsafeRead` (readIdx + 7))

            (img `M.unsafeWrite` idx) val0
            (img `M.unsafeWrite` (idx + (3 * 1))) val1
            (img `M.unsafeWrite` (idx + (3 * 2))) val2
            (img `M.unsafeWrite` (idx + (3 * 3))) val3
            (img `M.unsafeWrite` (idx + (3 * 4))) val4
            (img `M.unsafeWrite` (idx + (3 * 5))) val5
            (img `M.unsafeWrite` (idx + (3 * 6))) val6
            (img `M.unsafeWrite` (idx + (3 * 7))) val7

            blockVert (idx + 3 * imgWidth) (readIdx + dctBlockSize) $ j + 1


          {-where blockHoriz   _ readIdx i | i >= 8 = blockVert (writeIdx + imgWidth * 3) readIdx $ j + 1-}
                {-blockHoriz idx readIdx i = do-}
                    {-val <- pixelClamp <$> (block `M.unsafeRead` readIdx) -}
                    {-(img `M.unsafeWrite` idx) val-}
                    {-blockHoriz (idx + 3) (readIdx + 1) $ i + 1-}

unpack422Ycbcr :: Int -- ^ Component index
               -> Int -- ^ x
               -> Int -- ^ y
               -> MutableImage s PixelYCbCr8
               -> MutableMacroBlock s Int16
               -> ST s ()
unpack422Ycbcr compIdx x y
                 (MutableImage { mutableImageWidth = imgWidth,
                                 mutableImageHeight = _, mutableImageData = img })
                 block = blockVert baseIdx 0 zero
  where zero = 0 :: Int
        baseIdx = (x * dctBlockSize * 2 + y * dctBlockSize * imgWidth) * 3 + compIdx
        lineOffset = imgWidth * 3

        blockVert        _       _ j | j >= dctBlockSize = return ()
        blockVert idx readIdx j = do
            v0 <- pixelClamp <$> (block `M.unsafeRead` readIdx)
            v1 <- pixelClamp <$> (block `M.unsafeRead` (readIdx + 1))
            v2 <- pixelClamp <$> (block `M.unsafeRead` (readIdx + 2))
            v3 <- pixelClamp <$> (block `M.unsafeRead` (readIdx + 3))
            v4 <- pixelClamp <$> (block `M.unsafeRead` (readIdx + 4))
            v5 <- pixelClamp <$> (block `M.unsafeRead` (readIdx + 5))
            v6 <- pixelClamp <$> (block `M.unsafeRead` (readIdx + 6))
            v7 <- pixelClamp <$> (block `M.unsafeRead` (readIdx + 7))

            (img `M.unsafeWrite` idx)       v0
            (img `M.unsafeWrite` (idx + 3)) v0

            (img `M.unsafeWrite` (idx + 6 * 1))      v1
            (img `M.unsafeWrite` (idx + 6 * 1 + 3))  v1

            (img `M.unsafeWrite` (idx + 6 * 2))      v2
            (img `M.unsafeWrite` (idx + 6 * 2 + 3))  v2

            (img `M.unsafeWrite` (idx + 6 * 3))      v3
            (img `M.unsafeWrite` (idx + 6 * 3 + 3))  v3

            (img `M.unsafeWrite` (idx + 6 * 4))      v4
            (img `M.unsafeWrite` (idx + 6 * 4 + 3))  v4

            (img `M.unsafeWrite` (idx + 6 * 5))      v5
            (img `M.unsafeWrite` (idx + 6 * 5 + 3))  v5

            (img `M.unsafeWrite` (idx + 6 * 6))      v6
            (img `M.unsafeWrite` (idx + 6 * 6 + 3))  v6

            (img `M.unsafeWrite` (idx + 6 * 7))      v7
            (img `M.unsafeWrite` (idx + 6 * 7 + 3))  v7

            blockVert (idx + lineOffset) (readIdx + dctBlockSize) $ j + 1

-- | Same as for DcCoefficient, to provide nicer type signatures
type DctCoefficients = DcCoefficient

decodeRestartInterval :: BoolReader s Int32
decodeRestartInterval = return (-1) {-  do
  bits <- replicateM 8 getNextBitJpg
  if bits == replicate 8 True
     then do
         marker <- replicateM 8 getNextBitJpg
         return $ packInt marker
     else return (-1)
        -}

type JpgScripter a =
    RWS () [([JpgUnpackerParameter], L.ByteString)] JpgDecoderState a

data JpgDecoderState = JpgDecoderState
    { dcDecoderTables      :: !(V.Vector HuffmanPackedTree)
    , acDecoderTables      :: !(V.Vector HuffmanPackedTree)
    , quantizationMatrices :: !(V.Vector (MacroBlock Int16))
    , currentRestartInterv :: !Word16
    , currentFrame         :: Maybe JpgFrameHeader
    , maximumHorizontalResolution :: !Int
    , minimumVerticalResolution   :: !Int
    , seenBlobs                   :: !Int
    }

-- | This pseudo interpreter interpret the Jpg frame for the huffman,
-- quant table and restart interval parameters.
jpgMachineStep :: JpgFrame -> JpgScripter ()
jpgMachineStep (JpgAppFrame _ _) = pure ()
jpgMachineStep (JpgExtension _ _) = pure ()
jpgMachineStep (JpgScanBlob hdr raw_data) = do
    params <- concat <$> mapM scanSpecifier (scans hdr)
    modify $ \st -> st { seenBlobs = seenBlobs st + 1 }
    tell [(params, raw_data)  ]
  where (selectionLow, selectionHigh) = spectralSelection hdr
        approxHigh = fromIntegral $ successiveApproxHigh hdr
        approxLow = fromIntegral $ successiveApproxLow hdr
        

        scanSpecifier scanSpec = do
            let dcIndex = fromIntegral $ dcEntropyCodingTable scanSpec
                acIndex = fromIntegral $ acEntropyCodingTable scanSpec
                comp = fromIntegral $ componentSelector scanSpec
            dcTree <- gets $ (V.! dcIndex) . dcDecoderTables
            acTree <- gets $ (V.! acIndex) . acDecoderTables
            maxHoriz <- gets maximumHorizontalResolution
            maxVert <- gets minimumVerticalResolution
            restart <- gets currentRestartInterv
            frameInfo <- gets currentFrame
            blobId <- gets seenBlobs                   
            case frameInfo of
              Nothing -> fail "Jpg decoding error - no previous frame"
              Just v -> do
                 let compDesc = jpgComponents v !! comp
                     xSampling = fromIntegral $ horizontalSamplingFactor compDesc
                     xSamplingFactor = maxHoriz - xSampling + 1

                     ySampling = fromIntegral $ verticalSamplingFactor compDesc
                     ySamplingFactor = maxVert - ySampling + 1

                     quantIndex = fromIntegral $ quantizationTableDest compDesc

                 quant <- gets $ (V.! quantIndex) . quantizationMatrices

                 pure [ JpgUnpackerParameter
                          { dcHuffmanTree = dcTree
                          , acHuffmanTree = acTree
                          , quantization = quant
                          , componentIndex = comp
                          , restartInterval = fromIntegral restart
                          , componentWidth = xSamplingFactor
                          , componentHeight = ySamplingFactor
                          , successiveApprox = (approxLow, approxHigh)
                          , readerIndex = blobId
                          , coefficientRange =
                              ( fromIntegral selectionLow
                              , fromIntegral selectionHigh )


                          , mcuX = x
                          , mcuY = y
                          }
                              | y <- [0 .. ySamplingFactor - 1]
                              , x <- [0 .. xSamplingFactor - 1] ]

jpgMachineStep (JpgScans _ hdr) = modify $ \s ->
   s { currentFrame = Just hdr
     , maximumHorizontalResolution =
         fromIntegral $ maximum horizontalResolutions
     , minimumVerticalResolution =
         fromIntegral $ maximum verticalResolutions
     }
    where components = jpgComponents hdr
          horizontalResolutions = map horizontalSamplingFactor components
          verticalResolutions = map verticalSamplingFactor components
jpgMachineStep (JpgIntervalRestart restart) =
    modify $ \s -> s { currentRestartInterv = restart }
jpgMachineStep (JpgHuffmanTable tables) = mapM_ placeHuffmanTrees tables
  where placeHuffmanTrees (spec, tree) = case huffmanTableClass spec of
            DcComponent -> modify $ \s ->
                s { dcDecoderTables = dcDecoderTables s // [(idx, tree)] }
                    where idx = fromIntegral $ huffmanTableDest spec
            AcComponent -> modify $ \s ->
                s { acDecoderTables = acDecoderTables s // [(idx, tree)] }
                    where idx = fromIntegral $ huffmanTableDest spec

jpgMachineStep (JpgQuantTable tables) = mapM_ placeQuantizationTables tables
  where placeQuantizationTables table = do
            let idx = fromIntegral $ quantDestination table
                tableData = quantTable table
            modify $ \s ->
                s { quantizationMatrices =  quantizationMatrices s // [(idx, tableData)] }



decodeImage :: JpgImage
            -> Int -- ^ Component count
            -> MutableImage s PixelYCbCr8 -- ^ Result image to write into
            -> BoolReader s ()
decodeImage img compCount outImage = do
    zigZagArray <- lift $ createEmptyMutableMacroBlock
    dcArray <- lift (M.replicate compCount 0  :: ST s (M.STVector s DcCoefficient))

    let huffmans = gatherHuffmanTables img
        huffmanForComponent dcOrAc dest =
            case [t | (h,t) <- huffmans
                    , huffmanTableClass h == dcOrAc
                    , huffmanTableDest h == dest] of
              (v:_) -> v
              [] -> error "No Huffman table"

        mcuBeforeRestart = case [i | JpgIntervalRestart i <- jpgFrame img] of
            []    -> maxBound :: Int -- HUUUUUUGE value (enough to parse all MCU)
            (x:_) -> fromIntegral x

        quants = gatherQuantTables img
        quantForComponent dest =
            case [quantTable q | q <- quants, quantDestination q == dest] of
                (v:_) -> v
                [] -> error "No quant table"

        hdr = case [h | JpgScanBlob h _ <- jpgFrame img] of
                [] -> error "No scan blob"
                (v:_) -> v

        (_, scanInfo) = gatherScanInfo img
        imgWidth = fromIntegral $ jpgWidth scanInfo
        imgHeight = fromIntegral $ jpgHeight scanInfo

        blockSizeOfDim fullDim maxBlockSize = block + (if rest /= 0 then 1 else 0)
                where (block, rest) = fullDim `divMod` maxBlockSize

        horizontalSamplings = [horiz | (horiz, _, _, _, _, _) <- componentsInfo]

        imgComponentCount = fromIntegral $ jpgImageComponentCount scanInfo :: Int
        isImageLumanOnly = imgComponentCount == 1
        maxHorizFactor | not isImageLumanOnly &&
                            not (allElementsEqual horizontalSamplings) = maximum horizontalSamplings
                       | otherwise = 1

        verticalSamplings = [vert | (_, vert, _, _, _, _) <- componentsInfo]
        maxVertFactor | not isImageLumanOnly &&
                            not (allElementsEqual verticalSamplings) = maximum verticalSamplings
                      | otherwise = 1

        horizontalBlockCount =
           blockSizeOfDim imgWidth $ fromIntegral (maxHorizFactor * dctBlockSize)

        verticalBlockCount =
           blockSizeOfDim imgHeight $ fromIntegral (maxVertFactor * dctBlockSize)

        fetchTablesForComponent component = (horizCount, vertCount, dcTree, acTree, qTable, unpacker)
            where idx = componentIdentifier component
                  descr = case [c | c <- scans hdr, componentSelector c  == idx] of
                      (v:_) -> v
                      [] -> error "No scan"
                  dcTree = huffmanForComponent DcComponent $ dcEntropyCodingTable descr
                  acTree = huffmanForComponent AcComponent $ acEntropyCodingTable descr
                  qTable = quantForComponent $ if idx == 1 then 0 else 1
                  horizCount = if not isImageLumanOnly
                        then fromIntegral $ horizontalSamplingFactor component
                        else 1
                  vertCount = if not isImageLumanOnly
                        then fromIntegral $ verticalSamplingFactor component
                        else 1

                  xScalingFactor = maxHorizFactor - horizCount + 1
                  yScalingFactor = maxVertFactor - vertCount + 1

                  unpacker = unpackerDecision xScalingFactor yScalingFactor

                  unpackerDecision 1 1 | isImageLumanOnly = unpack444Y
                                       | otherwise = unpack444Ycbcr . fromIntegral $ idx - 1
                  unpackerDecision 2 1 = unpack422Ycbcr . fromIntegral $ idx - 1
                  unpackerDecision _ _ =  unpackMacroBlock compCount (fromIntegral $ idx - 1) xScalingFactor yScalingFactor

        componentsInfo = map fetchTablesForComponent $ jpgComponents scanInfo

    let blockIndices = [(x,y) | y <- [0 ..   verticalBlockCount - 1]
                              , x <- [0 .. horizontalBlockCount - 1] ]
        blockBeforeRestart = mcuBeforeRestart

        folder f = foldM_ f blockBeforeRestart blockIndices

    folder (\resetCounter (x,y) -> do
        when (resetCounter == 0)
             (do forM_ [0.. compCount - 1] $
                     \c -> lift $ (dcArray `M.unsafeWrite` c) 0
                 byteAlignJpg
                 _restartCode <- decodeRestartInterval
                 -- if 0xD0 <= restartCode && restartCode <= 0xD7
                 return ())

        let comp        _ [] = return ()
            comp compIdx ((horizCount, vertCount, dcTree, acTree, qTable, unpack):comp_rest) =
              rasterMap horizCount vertCount blockDecode >> comp (compIdx + 1) comp_rest
              where blockDecode xd yd = do
                      dc <- lift $ dcArray `M.unsafeRead` compIdx
                      (dcCoeff, block) <-
                            decompressMacroBlock dcTree acTree qTable zigZagArray $ fromIntegral dc
                      lift $ (dcArray `M.unsafeWrite` compIdx) dcCoeff
                      let verticalLimited = yd == horizCount - 1 || y == verticalBlockCount - 1
                      if (xd == horizCount - 1 && x == horizontalBlockCount - 1) || verticalLimited then
                        lift $ unpackMacroBlock imgComponentCount compIdx (maxHorizFactor - horizCount + 1)
                                                                          (maxVertFactor - vertCount + 1)
                                                (x * horizCount + xd) (y * vertCount + yd) outImage block
                      else
                        lift $ unpack (x * horizCount + xd) (y * vertCount + yd) outImage block

        comp 0 componentsInfo

        if resetCounter /= 0 then return $ resetCounter - 1
                                         -- we use blockBeforeRestart - 1 to count
                                         -- the current MCU
                            else return $ blockBeforeRestart - 1)

allElementsEqual :: (Eq a) => [a] -> Bool
allElementsEqual []     = True
allElementsEqual (x:xs) = all (== x) xs

gatherImageKind :: [JpgFrame] -> Maybe JpgImageKind
gatherImageKind lst = case [k | JpgScans k _ <- lst, isDctSpecifier k] of
    [JpgBaselineDCTHuffman] -> Just BaseLineDCT
    [JpgProgressiveDCTHuffman] -> Just ProgressiveDCT
    _ -> Nothing
  where isDctSpecifier JpgProgressiveDCTHuffman = True
        isDctSpecifier JpgBaselineDCTHuffman = True
        isDctSpecifier _ = False


-- | Try to decompress a jpeg file and decompress. The colorspace is still
-- YCbCr if you want to perform computation on the luma part. You can
-- convert it to RGB using 'convertImage' from the 'ColorSpaceConvertible'
-- typeclass.
--
-- This function can output the following pixel types :
--
--    * PixelY8
--
--    * PixelYCbCr8
--
decodeJpeg :: B.ByteString -> Either String DynamicImage
decodeJpeg file = case runGetStrict get file of
  Left err -> Left err
  Right img -> case (compCount, imgKind) of
                 (_, Nothing) -> Left "Unknown Jpg kind"
                 (_, Just ProgressiveDCT) -> Left "Unsupported Progressive JPEG image"
                 (1, _) -> Right . ImageY8 $ Image imgWidth imgHeight pixelData
                 (3, _) -> Right . ImageYCbCr8 $ Image imgWidth imgHeight pixelData
                 _ -> Left "Wrong component count"

      where (imgData:_) = [d | JpgScanBlob _kind d <- jpgFrame img]
            (_, scanInfo) = gatherScanInfo img
            compCount = length $ jpgComponents scanInfo

            imgKind = gatherImageKind $ jpgFrame img
            imgWidth = fromIntegral $ jpgWidth scanInfo
            imgHeight = fromIntegral $ jpgHeight scanInfo

            imageSize = imgWidth * imgHeight * compCount

            pixelData = runST $ VS.unsafeFreeze =<< S.evalStateT (do
                resultImage <- lift $ M.new imageSize
                let wrapped = MutableImage imgWidth imgHeight resultImage
                setDecodedStringJpg . B.concat $ L.toChunks imgData
                decodeImage img compCount wrapped
                return resultImage) emptyBoolState

extractBlock :: Image PixelYCbCr8       -- ^ Source image
             -> MutableMacroBlock s Int16      -- ^ Mutable block where to put extracted block
             -> Int                     -- ^ Plane
             -> Int                     -- ^ X sampling factor
             -> Int                     -- ^ Y sampling factor
             -> Int                     -- ^ Sample per pixel
             -> Int                     -- ^ Block x
             -> Int                     -- ^ Block y
             -> ST s (MutableMacroBlock s Int16)
extractBlock (Image { imageWidth = w, imageHeight = h, imageData = src })
             block 1 1 sampCount plane bx by | (bx * dctBlockSize) + 7 < w && (by * 8) + 7 < h = do
    let baseReadIdx = (by * dctBlockSize * w) + bx * dctBlockSize
    sequence_ [(block `M.unsafeWrite` (y * dctBlockSize + x)) val
                        | y <- [0 .. dctBlockSize - 1]
                        , let blockReadIdx = baseReadIdx + y * w
                        , x <- [0 .. dctBlockSize - 1]
                        , let val = fromIntegral $ src `VS.unsafeIndex` ((blockReadIdx + x) * sampCount + plane)
                        ]
    return block
extractBlock (Image { imageWidth = w, imageHeight = h, imageData = src })
             block sampWidth sampHeight sampCount plane bx by = do
    let accessPixel x y | x < w && y < h = let idx = (y * w + x) * sampCount + plane in src `VS.unsafeIndex` idx
                        | x >= w = accessPixel (w - 1) y
                        | otherwise = accessPixel x (h - 1)

        pixelPerCoeff = fromIntegral $ sampWidth * sampHeight

        blockVal x y = sum [fromIntegral $ accessPixel (xBase + dx) (yBase + dy)
                                | dy <- [0 .. sampHeight - 1]
                                , dx <- [0 .. sampWidth - 1] ] `div` pixelPerCoeff
            where xBase = blockXBegin + x * sampWidth
                  yBase = blockYBegin + y * sampHeight

        blockXBegin = bx * dctBlockSize * sampWidth
        blockYBegin = by * dctBlockSize * sampHeight

    sequence_ [(block `M.unsafeWrite` (y * dctBlockSize + x)) $ blockVal x y | y <- [0 .. 7], x <- [0 .. 7] ]
    return block

serializeMacroBlock :: BoolWriteStateRef s
                    -> HuffmanWriterCode -> HuffmanWriterCode
                    -> MutableMacroBlock s Int32
                    -> ST s ()
serializeMacroBlock !st !dcCode !acCode !blk =
 (blk `M.unsafeRead` 0) >>= (fromIntegral >>> encodeDc) >> writeAcs (0, 1) >> return ()
  where writeAcs acc@(_, 63) =
            (blk `M.unsafeRead` 63) >>= (fromIntegral >>> encodeAcCoefs acc) >> return ()
        writeAcs acc@(_, i ) =
            (blk `M.unsafeRead`  i) >>= (fromIntegral >>> encodeAcCoefs acc) >>= writeAcs

        encodeDc n = writeBits' st (fromIntegral code) (fromIntegral bitCount)
                        >> when (ssss /= 0) (encodeInt st ssss n)
            where ssss = powerOf $ fromIntegral n
                  (bitCount, code) = dcCode `V.unsafeIndex` fromIntegral ssss

        encodeAc 0         0 = writeBits' st (fromIntegral code) $ fromIntegral bitCount
            where (bitCount, code) = acCode `V.unsafeIndex` 0

        encodeAc zeroCount n | zeroCount >= 16 =
          writeBits' st (fromIntegral code) (fromIntegral bitCount) >>  encodeAc (zeroCount - 16) n
            where (bitCount, code) = acCode `V.unsafeIndex` 0xF0
        encodeAc zeroCount n =
          writeBits' st (fromIntegral code) (fromIntegral bitCount) >> encodeInt st ssss n
            where rrrr = zeroCount `unsafeShiftL` 4
                  ssss = powerOf $ fromIntegral n
                  rrrrssss = rrrr .|. ssss
                  (bitCount, code) = acCode `V.unsafeIndex` fromIntegral rrrrssss

        encodeAcCoefs (            _, 63) 0 = encodeAc 0 0 >> return (0, 64)
        encodeAcCoefs (zeroRunLength,  i) 0 = return (zeroRunLength + 1, i + 1)
        encodeAcCoefs (zeroRunLength,  i) n =
            encodeAc zeroRunLength n >> return (0, i + 1)

encodeMacroBlock :: QuantificationTable
                 -> MutableMacroBlock s Int32
                 -> MutableMacroBlock s Int32
                 -> Int16
                 -> MutableMacroBlock s Int16
                 -> ST s (Int32, MutableMacroBlock s Int32)
encodeMacroBlock quantTableOfComponent workData finalData prev_dc block = do
 -- the inverse level shift is performed internally by the fastDCT routine
 blk <- fastDctLibJpeg workData block
        >>= zigZagReorderForward finalData
        >>= quantize quantTableOfComponent
 dc <- blk `M.unsafeRead` 0
 (blk `M.unsafeWrite` 0) $ dc - fromIntegral prev_dc
 return (dc, blk)

divUpward :: (Integral a) => a -> a -> a
divUpward n dividor = val + (if rest /= 0 then 1 else 0)
    where (val, rest) = n `divMod` dividor

prepareHuffmanTable :: DctComponent -> Word8 -> HuffmanTable
                    -> (JpgHuffmanTableSpec, HuffmanPackedTree)
prepareHuffmanTable classVal dest tableDef =
   (JpgHuffmanTableSpec { huffmanTableClass = classVal
                        , huffmanTableDest  = dest
                        , huffSizes = sizes
                        , huffCodes = V.fromListN 16
                            [VU.fromListN (fromIntegral $ sizes ! i) lst
                                                | (i, lst) <- zip [0..] tableDef ]
                        }, VS.singleton 0)
      where sizes = VU.fromListN 16 $ map (fromIntegral . length) tableDef

-- | Encode an image in jpeg at a reasonnable quality level.
-- If you want better quality or reduced file size, you should
-- use `encodeJpegAtQuality`
encodeJpeg :: Image PixelYCbCr8 -> L.ByteString
encodeJpeg = encodeJpegAtQuality 50

defaultHuffmanTables :: [(JpgHuffmanTableSpec, HuffmanPackedTree)]
defaultHuffmanTables =
    [ prepareHuffmanTable DcComponent 0 defaultDcLumaHuffmanTable
    , prepareHuffmanTable AcComponent 0 defaultAcLumaHuffmanTable
    , prepareHuffmanTable DcComponent 1 defaultDcChromaHuffmanTable
    , prepareHuffmanTable AcComponent 1 defaultAcChromaHuffmanTable
    ]

-- | Function to call to encode an image to jpeg.
-- The quality factor should be between 0 and 100 (100 being
-- the best quality).
encodeJpegAtQuality :: Word8                -- ^ Quality factor
                    -> Image PixelYCbCr8    -- ^ Image to encode
                    -> L.ByteString         -- ^ Encoded JPEG
encodeJpegAtQuality quality img@(Image { imageWidth = w, imageHeight = h }) = encode finalImage
  where finalImage = JpgImage [ JpgQuantTable quantTables
                              , JpgScans JpgBaselineDCTHuffman hdr
                              , JpgHuffmanTable defaultHuffmanTables
                              , JpgScanBlob scanHeader encodedImage
                              ]

        outputComponentCount = 3

        scanHeader = scanHeader'{ scanLength = fromIntegral $ calculateSize scanHeader' }
        scanHeader' = JpgScanHeader
            { scanLength = 0
            , scanComponentCount = outputComponentCount
            , scans = [ JpgScanSpecification { componentSelector = 1
                                             , dcEntropyCodingTable = 0
                                             , acEntropyCodingTable = 0
                                             }
                      , JpgScanSpecification { componentSelector = 2
                                             , dcEntropyCodingTable = 1
                                             , acEntropyCodingTable = 1
                                             }
                      , JpgScanSpecification { componentSelector = 3
                                             , dcEntropyCodingTable = 1
                                             , acEntropyCodingTable = 1
                                             }
                      ]

            , spectralSelection = (0, 63)
            , successiveApproxHigh = 0
            , successiveApproxLow  = 0
            }

        hdr = hdr' { jpgFrameHeaderLength   = fromIntegral $ calculateSize hdr' }
        hdr' = JpgFrameHeader { jpgFrameHeaderLength   = 0
                              , jpgSamplePrecision     = 8
                              , jpgHeight              = fromIntegral h
                              , jpgWidth               = fromIntegral w
                              , jpgImageComponentCount = outputComponentCount
                              , jpgComponents          = [
                                    JpgComponent { componentIdentifier      = 1
                                                 , horizontalSamplingFactor = 2
                                                 , verticalSamplingFactor   = 2
                                                 , quantizationTableDest    = 0
                                                 }
                                  , JpgComponent { componentIdentifier      = 2
                                                 , horizontalSamplingFactor = 1
                                                 , verticalSamplingFactor   = 1
                                                 , quantizationTableDest    = 1
                                                 }
                                  , JpgComponent { componentIdentifier      = 3
                                                 , horizontalSamplingFactor = 1
                                                 , verticalSamplingFactor   = 1
                                                 , quantizationTableDest    = 1
                                                 }
                                  ]
                              }

        lumaQuant = scaleQuantisationMatrix (fromIntegral quality)
                        defaultLumaQuantizationTable
        chromaQuant = scaleQuantisationMatrix (fromIntegral quality)
                            defaultChromaQuantizationTable

        zigzagedLumaQuant = zigZagReorderForwardv lumaQuant
        zigzagedChromaQuant = zigZagReorderForwardv chromaQuant
        quantTables = [ JpgQuantTableSpec { quantPrecision = 0, quantDestination = 0
                                          , quantTable = zigzagedLumaQuant }
                      , JpgQuantTableSpec { quantPrecision = 0, quantDestination = 1
                                          , quantTable = zigzagedChromaQuant }
                      ]

        encodedImage = runST $ do
            let horizontalMetaBlockCount =
                    w `divUpward` (dctBlockSize * maxSampling)
                verticalMetaBlockCount =
                    h `divUpward` (dctBlockSize * maxSampling)
                maxSampling = 2
                lumaSamplingSize = ( maxSampling, maxSampling, zigzagedLumaQuant
                                   , makeInverseTable defaultDcLumaHuffmanTree
                                   , makeInverseTable defaultAcLumaHuffmanTree)
                chromaSamplingSize = ( maxSampling - 1, maxSampling - 1, zigzagedChromaQuant
                                     , makeInverseTable defaultDcChromaHuffmanTree
                                     , makeInverseTable defaultAcChromaHuffmanTree)
                componentDef = [lumaSamplingSize, chromaSamplingSize, chromaSamplingSize]

                imageComponentCount = length componentDef

            dc_table <- M.replicate 3 0
            block <- createEmptyMutableMacroBlock
            workData <- createEmptyMutableMacroBlock
            zigzaged <- createEmptyMutableMacroBlock
            writeState <- newWriteStateRef

            -- It's ugly, I know, be avoid allocation
            let blockDecoder mx my = component $ zip [0..] componentDef
                  where component [] = return ()
                        component ((comp, (sizeX, sizeY, table, dc, ac)) : comp_rest) =
                           rasterMap sizeX sizeY decoder >> component comp_rest
                          where xSamplingFactor = maxSampling - sizeX + 1
                                ySamplingFactor = maxSampling - sizeY + 1
                                extractor = extractBlock img block xSamplingFactor ySamplingFactor imageComponentCount

                                decoder subX subY = do
                                  let blockY = my * sizeY + subY
                                      blockX = mx * sizeX + subX
                                  prev_dc <- dc_table `M.unsafeRead` comp
                                  (dc_coeff, neo_block) <- (extractor comp blockX blockY >>=
                                                          encodeMacroBlock table workData zigzaged prev_dc)
                                  (dc_table `M.unsafeWrite` comp) $ fromIntegral dc_coeff
                                  serializeMacroBlock writeState dc ac neo_block

            rasterMap 
                horizontalMetaBlockCount verticalMetaBlockCount
                blockDecoder

            finalizeBoolWriter writeState
