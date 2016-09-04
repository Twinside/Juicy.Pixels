{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fspec-constr-count=5 #-}
-- | Module used for JPEG file loading and writing.
module Codec.Picture.Jpg( decodeJpeg
                        , decodeJpegWithMetadata
                        , encodeJpegAtQuality
                        , encodeJpegAtQualityWithMetadata
                        , encodeDirectJpegAtQualityWithMetadata
                        , encodeJpeg
                        , JpgEncodable
                        ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable( foldMap )
import Data.Monoid( mempty )
import Control.Applicative( pure, (<$>) )
#endif

import Control.Applicative( (<|>) )

import Control.Arrow( (>>>) )
import Control.Monad( when, forM_ )
import Control.Monad.ST( ST, runST )
import Control.Monad.Trans( lift )
import Control.Monad.Trans.RWS.Strict( RWS, modify, tell, gets, execRWS )

import Data.Bits( (.|.), unsafeShiftL )
import Data.Monoid( (<>) )
import Data.Int( Int16, Int32 )
import Data.Word(Word8, Word32)
import Data.Binary( Binary(..), encode )
import Data.STRef( newSTRef, writeSTRef, readSTRef )

import Data.Vector( (//) )
import Data.Vector.Unboxed( (!) )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Codec.Picture.InternalHelper
import Codec.Picture.BitWriter
import Codec.Picture.Types
import Codec.Picture.Metadata( Metadatas
                             , SourceFormat( SourceJpeg )
                             , basicMetadata )
import Codec.Picture.Tiff.Types
import Codec.Picture.Tiff.Metadata
import Codec.Picture.Jpg.Types
import Codec.Picture.Jpg.Common
import Codec.Picture.Jpg.Progressive
import Codec.Picture.Jpg.DefaultTable
import Codec.Picture.Jpg.FastDct
import Codec.Picture.Jpg.Metadata

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

pixelClamp :: Int16 -> Word8
pixelClamp n = fromIntegral . min 255 $ max 0 n

unpack444Y :: Int -- ^ component index
           -> Int -- ^ x
           -> Int -- ^ y
           -> MutableImage s PixelYCbCr8
           -> MutableMacroBlock s Int16
           -> ST s ()
unpack444Y _ x y (MutableImage { mutableImageWidth = imgWidth, mutableImageData = img })
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
            (img `M.unsafeWrite` (idx +  3     )) val1
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

unpack421Ycbcr :: Int -- ^ Component index
               -> Int -- ^ x
               -> Int -- ^ y
               -> MutableImage s PixelYCbCr8
               -> MutableMacroBlock s Int16
               -> ST s ()
unpack421Ycbcr compIdx x y
                 (MutableImage { mutableImageWidth = imgWidth,
                                 mutableImageHeight = _, mutableImageData = img })
                 block = blockVert baseIdx 0 zero
  where zero = 0 :: Int
        baseIdx = (x * dctBlockSize + y * dctBlockSize * imgWidth) * 3 + compIdx
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

            (img `M.unsafeWrite` (idx + 6    ))      v1
            (img `M.unsafeWrite` (idx + 6     + 3))  v1

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

type Unpacker s = Int -- ^ component index
               -> Int -- ^ x
               -> Int -- ^ y
               -> MutableImage s PixelYCbCr8
               -> MutableMacroBlock s Int16
               -> ST s ()

type JpgScripter s a =
    RWS () [([(JpgUnpackerParameter, Unpacker s)], L.ByteString)] JpgDecoderState a

data JpgDecoderState = JpgDecoderState
    { dcDecoderTables       :: !(V.Vector HuffmanPackedTree)
    , acDecoderTables       :: !(V.Vector HuffmanPackedTree)
    , quantizationMatrices  :: !(V.Vector (MacroBlock Int16))
    , currentRestartInterv  :: !Int
    , currentFrame          :: Maybe JpgFrameHeader
    , app14Marker           :: !(Maybe JpgAdobeApp14)
    , app0JFifMarker        :: !(Maybe JpgJFIFApp0)
    , app1ExifMarker        :: !(Maybe [ImageFileDirectory])
    , componentIndexMapping :: ![(Word8, Int)]
    , isProgressive         :: !Bool
    , maximumHorizontalResolution :: !Int
    , maximumVerticalResolution   :: !Int
    , seenBlobs                   :: !Int
    }

emptyDecoderState :: JpgDecoderState
emptyDecoderState = JpgDecoderState
    { dcDecoderTables =
        let (_, dcLuma) = prepareHuffmanTable DcComponent 0 defaultDcLumaHuffmanTable
            (_, dcChroma) = prepareHuffmanTable DcComponent 1 defaultDcChromaHuffmanTable
        in
        V.fromList [ dcLuma, dcChroma, dcLuma, dcChroma ]

    , acDecoderTables =
        let (_, acLuma) = prepareHuffmanTable AcComponent 0 defaultAcLumaHuffmanTable
            (_, acChroma) = prepareHuffmanTable AcComponent 1 defaultAcChromaHuffmanTable
        in
        V.fromList [acLuma, acChroma, acLuma, acChroma]

    , quantizationMatrices = V.replicate 4 (VS.replicate (8 * 8) 1)
    , currentRestartInterv = -1
    , currentFrame         = Nothing
    , componentIndexMapping = []
    , app14Marker = Nothing
    , app0JFifMarker = Nothing
    , app1ExifMarker = Nothing
    , isProgressive        = False
    , maximumHorizontalResolution = 0
    , maximumVerticalResolution   = 0
    , seenBlobs = 0
    }

-- | This pseudo interpreter interpret the Jpg frame for the huffman,
-- quant table and restart interval parameters.
jpgMachineStep :: JpgFrame -> JpgScripter s ()
jpgMachineStep (JpgAdobeAPP14 app14) = modify $ \s ->
    s { app14Marker = Just app14 }
jpgMachineStep (JpgExif exif) = modify $ \s ->
    s { app1ExifMarker = Just exif }
jpgMachineStep (JpgJFIF app0) = modify $ \s ->
    s { app0JFifMarker = Just app0 }
jpgMachineStep (JpgAppFrame _ _) = pure ()
jpgMachineStep (JpgExtension _ _) = pure ()
jpgMachineStep (JpgScanBlob hdr raw_data) = do
    let scanCount = length $ scans hdr
    params <- concat <$> mapM (scanSpecifier scanCount) (scans hdr)

    modify $ \st -> st { seenBlobs = seenBlobs st + 1 }
    tell [(params, raw_data)  ]
  where (selectionLow, selectionHigh) = spectralSelection hdr
        approxHigh = fromIntegral $ successiveApproxHigh hdr
        approxLow = fromIntegral $ successiveApproxLow hdr

        
        scanSpecifier scanCount scanSpec = do
            compMapping <- gets componentIndexMapping
            comp <- case lookup (componentSelector scanSpec) compMapping of
                Nothing -> fail "Jpg decoding error - bad component selector in blob."
                Just v -> return v
            let maximumHuffmanTable = 4
                dcIndex = min (maximumHuffmanTable - 1) 
                            . fromIntegral $ dcEntropyCodingTable scanSpec
                acIndex = min (maximumHuffmanTable - 1)
                            . fromIntegral $ acEntropyCodingTable scanSpec

            dcTree <- gets $ (V.! dcIndex) . dcDecoderTables
            acTree <- gets $ (V.! acIndex) . acDecoderTables
            isProgressiveImage <- gets isProgressive
            maxiW <- gets maximumHorizontalResolution 
            maxiH <- gets maximumVerticalResolution
            restart <- gets currentRestartInterv
            frameInfo <- gets currentFrame
            blobId <- gets seenBlobs                   
            case frameInfo of
              Nothing -> fail "Jpg decoding error - no previous frame"
              Just v -> do
                 let compDesc = jpgComponents v !! comp
                     compCount = length $ jpgComponents v
                     xSampling = fromIntegral $ horizontalSamplingFactor compDesc
                     ySampling = fromIntegral $ verticalSamplingFactor compDesc
                     componentSubSampling =
                        (maxiW - xSampling + 1, maxiH - ySampling + 1)
                     (xCount, yCount)
                        | scanCount > 1 || isProgressiveImage = (xSampling, ySampling)
                        | otherwise = (1, 1)

                 pure [ (JpgUnpackerParameter
                         { dcHuffmanTree = dcTree
                         , acHuffmanTree = acTree
                         , componentIndex = comp
                         , restartInterval = fromIntegral restart
                         , componentWidth = xSampling
                         , componentHeight = ySampling
                         , subSampling = componentSubSampling
                         , successiveApprox = (approxLow, approxHigh)
                         , readerIndex = blobId
                         , indiceVector =
                             if scanCount == 1 then 0 else 1
                         , coefficientRange =
                             ( fromIntegral selectionLow
                             , fromIntegral selectionHigh )
                         , blockIndex = y * xSampling + x
                         , blockMcuX = x
                         , blockMcuY = y
                         }, unpackerDecision compCount componentSubSampling)
                             | y <- [0 .. yCount - 1]
                             , x <- [0 .. xCount - 1] ]

jpgMachineStep (JpgScans kind hdr) = modify $ \s ->
   s { currentFrame = Just hdr
     , componentIndexMapping =
          [(componentIdentifier comp, ix) | (ix, comp) <- zip [0..] $ jpgComponents hdr]
     , isProgressive = case kind of
            JpgProgressiveDCTHuffman -> True
            _ -> False
     , maximumHorizontalResolution =
         fromIntegral $ maximum horizontalResolutions
     , maximumVerticalResolution =
         fromIntegral $ maximum verticalResolutions
     }
    where components = jpgComponents hdr
          horizontalResolutions = map horizontalSamplingFactor components
          verticalResolutions = map verticalSamplingFactor components
jpgMachineStep (JpgIntervalRestart restart) =
    modify $ \s -> s { currentRestartInterv = fromIntegral restart }
jpgMachineStep (JpgHuffmanTable tables) = mapM_ placeHuffmanTrees tables
  where placeHuffmanTrees (spec, tree) = case huffmanTableClass spec of
            DcComponent -> modify $ \s ->
              if idx >= V.length (dcDecoderTables s) then s
              else
                let neu = dcDecoderTables s // [(idx, tree)] in 
                s { dcDecoderTables = neu }
                    where idx = fromIntegral $ huffmanTableDest spec
                          
            AcComponent -> modify $ \s ->
              if idx >= V.length (acDecoderTables s) then s
              else
                s { acDecoderTables = acDecoderTables s // [(idx, tree)] }
                    where idx = fromIntegral $ huffmanTableDest spec

jpgMachineStep (JpgQuantTable tables) = mapM_ placeQuantizationTables tables
  where placeQuantizationTables table = do
            let idx = fromIntegral $ quantDestination table
                tableData = quantTable table
            modify $ \s ->
                s { quantizationMatrices =  quantizationMatrices s // [(idx, tableData)] }

unpackerDecision :: Int -> (Int, Int) -> Unpacker s
unpackerDecision 1 (1, 1) = unpack444Y
unpackerDecision 3 (1, 1) = unpack444Ycbcr
unpackerDecision _ (2, 1) = unpack421Ycbcr
unpackerDecision compCount (xScalingFactor, yScalingFactor) =
    unpackMacroBlock compCount xScalingFactor yScalingFactor

decodeImage :: JpgFrameHeader
            -> V.Vector (MacroBlock Int16)
            -> [([(JpgUnpackerParameter, Unpacker s)], L.ByteString)]
            -> MutableImage s PixelYCbCr8 -- ^ Result image to write into
            -> ST s (MutableImage s PixelYCbCr8)
decodeImage frame quants lst outImage = do
  let compCount = length $ jpgComponents frame
  zigZagArray <- createEmptyMutableMacroBlock
  dcArray <- M.replicate compCount 0  :: ST s (M.STVector s DcCoefficient)
  resetCounter <- newSTRef restartIntervalValue

  forM_ lst $ \(params, str) -> do
    let componentsInfo = V.fromList params
        compReader = initBoolStateJpg . B.concat $ L.toChunks str
        maxiW = maximum [fst $ subSampling c | (c,_) <- params]
        maxiH = maximum [snd $ subSampling c | (c,_) <- params]

        imageBlockWidth = toBlockSize imgWidth
        imageBlockHeight = toBlockSize imgHeight

        imageMcuWidth = (imageBlockWidth + (maxiW - 1)) `div` maxiW
        imageMcuHeight = (imageBlockHeight + (maxiH - 1)) `div` maxiH

    execBoolReader compReader $ rasterMap imageMcuWidth imageMcuHeight $ \x y -> do
      resetLeft <- lift $ readSTRef resetCounter
      if resetLeft == 0 then do
        lift $ M.set dcArray 0
        byteAlignJpg
        _restartCode <- decodeRestartInterval
        lift $ resetCounter `writeSTRef` (restartIntervalValue - 1)
      else
        lift $ resetCounter `writeSTRef` (resetLeft - 1)

      V.forM_ componentsInfo $ \(comp, unpack) -> do
        let compIdx = componentIndex comp
            dcTree = dcHuffmanTree comp
            acTree = acHuffmanTree comp
            quantId = fromIntegral .  quantizationTableDest
                    $ jpgComponents frame !! compIdx
            qTable = quants V.! min 3 quantId
            xd = blockMcuX comp
            yd = blockMcuY comp
            (subX, subY) = subSampling comp
        dc <- lift $ dcArray `M.unsafeRead` compIdx
        (dcCoeff, block) <-
              decompressMacroBlock dcTree acTree qTable zigZagArray $ fromIntegral dc
        lift $ (dcArray `M.unsafeWrite` compIdx) dcCoeff
        let verticalLimited = y == imageMcuHeight - 1
        if (x == imageMcuWidth - 1) || verticalLimited then
          lift $ unpackMacroBlock imgComponentCount
                                  subX subY compIdx
                                  (x * maxiW + xd) (y * maxiH + yd) outImage block
        else
          lift $ unpack compIdx (x * maxiW + xd) (y * maxiH + yd) outImage block

  return outImage

  where imgComponentCount = length $ jpgComponents frame

        imgWidth = fromIntegral $ jpgWidth frame
        imgHeight = fromIntegral $ jpgHeight frame
        restartIntervalValue = case lst of
                ((p,_):_,_): _ -> restartInterval p
                _ -> -1

gatherImageKind :: [JpgFrame] -> Maybe JpgImageKind
gatherImageKind lst = case [k | JpgScans k _ <- lst, isDctSpecifier k] of
    [JpgBaselineDCTHuffman] -> Just BaseLineDCT
    [JpgProgressiveDCTHuffman] -> Just ProgressiveDCT
    _ -> Nothing
  where isDctSpecifier JpgProgressiveDCTHuffman = True
        isDctSpecifier JpgBaselineDCTHuffman = True
        isDctSpecifier _ = False

gatherScanInfo :: JpgImage -> (JpgFrameKind, JpgFrameHeader)
gatherScanInfo img = head [(a, b) | JpgScans a b <- jpgFrame img]

dynamicOfColorSpace :: (Monad m)
                    => Maybe JpgColorSpace -> Int -> Int -> VS.Vector Word8
                    -> m DynamicImage
dynamicOfColorSpace Nothing _ _ _ = fail "Unknown color space"
dynamicOfColorSpace (Just color) w h imgData = case color of
  JpgColorSpaceCMYK -> return . ImageCMYK8 $ Image w h imgData
  JpgColorSpaceYCCK ->
     let ymg = Image w h $ VS.map (255-) imgData :: Image PixelYCbCrK8 in
     return . ImageCMYK8 $ convertImage ymg
  JpgColorSpaceYCbCr -> return . ImageYCbCr8 $ Image w h imgData
  JpgColorSpaceRGB -> return . ImageRGB8 $ Image w h imgData
  JpgColorSpaceYA -> return . ImageYA8 $ Image w h imgData
  JpgColorSpaceY -> return . ImageY8 $ Image w h imgData
  colorSpace -> fail $ "Wrong color space : " ++ show colorSpace

colorSpaceOfAdobe :: Int -> JpgAdobeApp14 -> Maybe JpgColorSpace
colorSpaceOfAdobe compCount app = case (compCount, _adobeTransform app) of
  (3, AdobeYCbCr) -> pure JpgColorSpaceYCbCr
  (1, AdobeUnknown) -> pure JpgColorSpaceY
  (3, AdobeUnknown) -> pure JpgColorSpaceRGB
  (4, AdobeYCck) -> pure JpgColorSpaceYCCK
  {-(4, AdobeUnknown) -> pure JpgColorSpaceCMYKInverted-}
  _ -> Nothing

colorSpaceOfState :: JpgDecoderState -> Maybe JpgColorSpace
colorSpaceOfState st = do
  hdr <- currentFrame st
  let compStr = [toEnum . fromEnum $ componentIdentifier comp
                        | comp <- jpgComponents hdr]
      app14 = do
        marker <- app14Marker st
        colorSpaceOfAdobe (length compStr) marker
  app14 <|> colorSpaceOfComponentStr compStr


colorSpaceOfComponentStr :: String -> Maybe JpgColorSpace
colorSpaceOfComponentStr s = case s of
  [_] -> pure  JpgColorSpaceY
  [_,_] -> pure  JpgColorSpaceYA
  "\0\1\2" -> pure  JpgColorSpaceYCbCr
  "\1\2\3" -> pure  JpgColorSpaceYCbCr
  "RGB" -> pure  JpgColorSpaceRGB
  "YCc" -> pure  JpgColorSpaceYCC
  [_,_,_] -> pure  JpgColorSpaceYCbCr

  "RGBA" -> pure  JpgColorSpaceRGBA
  "YCcA" -> pure  JpgColorSpaceYCCA
  "CMYK" -> pure  JpgColorSpaceCMYK
  "YCcK" -> pure  JpgColorSpaceYCCK
  [_,_,_,_] -> pure  JpgColorSpaceCMYK
  _ -> Nothing

-- | Try to decompress and decode a jpeg file. The colorspace is still
-- YCbCr if you want to perform computation on the luma part. You can convert it
-- to RGB using 'convertImage' from the 'ColorSpaceConvertible' typeclass.
--
-- This function can output the following images:
--
--  * 'ImageY8'
--
--  * 'ImageYA8'
--
--  * 'ImageRGB8'
--
--  * 'ImageCMYK8'
--
--  * 'ImageYCbCr8'
--
decodeJpeg :: B.ByteString -> Either String DynamicImage
decodeJpeg = fmap fst . decodeJpegWithMetadata

-- | Equivalent to 'decodeJpeg' but also extracts metadatas.
--
-- Extract the following metadatas from the JFIF block:
--
--  * 'Codec.Picture.Metadata.DpiX'
--  * 'Codec.Picture.Metadata.DpiY' 
--
-- Exif metadata are also extracted if present.
--
decodeJpegWithMetadata :: B.ByteString -> Either String (DynamicImage, Metadatas)
decodeJpegWithMetadata file = case runGetStrict get file of
  Left err -> Left err
  Right img -> case imgKind of
     Just BaseLineDCT ->
       let (st, arr) = decodeBaseline
           jfifMeta = foldMap extractMetadatas $ app0JFifMarker st
           exifMeta = foldMap extractTiffMetadata $ app1ExifMarker st
           meta = sizeMeta <> jfifMeta <> exifMeta
       in
       (, meta) <$>
           dynamicOfColorSpace (colorSpaceOfState st) imgWidth imgHeight arr
     Just ProgressiveDCT ->
       let (st, arr) = decodeProgressive
           jfifMeta = foldMap extractMetadatas $ app0JFifMarker st
           exifMeta = foldMap extractTiffMetadata $ app1ExifMarker st
           meta = sizeMeta <> jfifMeta <> exifMeta
       in
       (, meta) <$>
           dynamicOfColorSpace (colorSpaceOfState st) imgWidth imgHeight arr
     _ -> Left "Unknown JPG kind"
    where
      compCount = length $ jpgComponents scanInfo
      (_,scanInfo) = gatherScanInfo img

      imgKind = gatherImageKind $ jpgFrame img
      imgWidth = fromIntegral $ jpgWidth scanInfo
      imgHeight = fromIntegral $ jpgHeight scanInfo

      sizeMeta = basicMetadata SourceJpeg imgWidth imgHeight

      imageSize = imgWidth * imgHeight * compCount


      decodeProgressive = runST $ do
        let (st, wrotten) =
               execRWS (mapM_ jpgMachineStep (jpgFrame img)) () emptyDecoderState
            Just fHdr = currentFrame st
        fimg <-
            progressiveUnpack
                (maximumHorizontalResolution st, maximumVerticalResolution st)
                fHdr
                (quantizationMatrices st)
                wrotten
        frozen <- unsafeFreezeImage fimg
        return (st, imageData frozen)


      decodeBaseline = runST $ do
        let (st, wrotten) =
              execRWS (mapM_ jpgMachineStep (jpgFrame img)) () emptyDecoderState
            Just fHdr = currentFrame st
        resultImage <- M.new imageSize
        let wrapped = MutableImage imgWidth imgHeight resultImage
        fImg <- decodeImage 
            fHdr
            (quantizationMatrices st)
            wrotten
            wrapped
        frozen <- unsafeFreezeImage fImg
        return (st, imageData frozen)

extractBlock :: forall s px. (PixelBaseComponent px ~ Word8)
             => Image px       -- ^ Source image
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

lumaQuantTableAtQuality :: Int -> QuantificationTable 
lumaQuantTableAtQuality qual = scaleQuantisationMatrix qual defaultLumaQuantizationTable

chromaQuantTableAtQuality :: Int -> QuantificationTable
chromaQuantTableAtQuality qual =
  scaleQuantisationMatrix qual defaultChromaQuantizationTable

zigzaggedQuantificationSpec :: Int -> [JpgQuantTableSpec]
zigzaggedQuantificationSpec qual =
  [ JpgQuantTableSpec { quantPrecision = 0, quantDestination = 0, quantTable = luma }
  , JpgQuantTableSpec { quantPrecision = 0, quantDestination = 1, quantTable = chroma }
  ]
  where
    luma = zigZagReorderForwardv $ lumaQuantTableAtQuality qual
    chroma = zigZagReorderForwardv $ chromaQuantTableAtQuality qual

-- | Function to call to encode an image to jpeg.
-- The quality factor should be between 0 and 100 (100 being
-- the best quality).
encodeJpegAtQuality :: Word8                -- ^ Quality factor
                    -> Image PixelYCbCr8    -- ^ Image to encode
                    -> L.ByteString         -- ^ Encoded JPEG
encodeJpegAtQuality quality = encodeJpegAtQualityWithMetadata quality mempty

-- | Record gathering all information to encode a component
-- from the source image. Previously was a huge tuple
-- burried in the code
data EncoderState = EncoderState
  { _encComponentIndex :: !Int
  , _encBlockWidth     :: !Int
  , _encBlockHeight    :: !Int
  , _encQuantTable     :: !QuantificationTable
  , _encDcHuffman      :: !HuffmanWriterCode
  , _encAcHuffman      :: !HuffmanWriterCode
  }


-- | Helper type class describing all JPG-encodable pixel types
class (Pixel px, PixelBaseComponent px ~ Word8) => JpgEncodable px where
  additionalBlocks :: Image px -> [JpgFrame]
  additionalBlocks _ = []

  componentsOfColorSpace :: Image px -> [JpgComponent]

  encodingState :: Int -> Image px -> V.Vector EncoderState

  imageHuffmanTables :: Image px -> [(JpgHuffmanTableSpec, HuffmanPackedTree)]
  imageHuffmanTables _ = defaultHuffmanTables 

  scanSpecificationOfColorSpace :: Image px -> [JpgScanSpecification]

  quantTableSpec :: Image px -> Int -> [JpgQuantTableSpec]
  quantTableSpec _ qual = take 1 $ zigzaggedQuantificationSpec qual

  maximumSubSamplingOf :: Image px -> Int
  maximumSubSamplingOf _ = 1

instance JpgEncodable Pixel8 where
  scanSpecificationOfColorSpace _ =
    [ JpgScanSpecification { componentSelector = 1
                           , dcEntropyCodingTable = 0
                           , acEntropyCodingTable = 0
                           }
    ]

  componentsOfColorSpace _ =
    [ JpgComponent { componentIdentifier      = 1
                   , horizontalSamplingFactor = 1
                   , verticalSamplingFactor   = 1
                   , quantizationTableDest    = 0
                   }
    ]

  imageHuffmanTables _ =
    [ prepareHuffmanTable DcComponent 0 defaultDcLumaHuffmanTable
    , prepareHuffmanTable AcComponent 0 defaultAcLumaHuffmanTable
    ]

  encodingState qual _ = V.singleton EncoderState
     { _encComponentIndex = 0
     , _encBlockWidth     = 1
     , _encBlockHeight    = 1
     , _encQuantTable     = zigZagReorderForwardv $ lumaQuantTableAtQuality qual
     , _encDcHuffman      = makeInverseTable defaultDcLumaHuffmanTree
     , _encAcHuffman      = makeInverseTable defaultAcLumaHuffmanTree
     }


instance JpgEncodable PixelYCbCr8 where
  maximumSubSamplingOf _ = 2
  quantTableSpec _ qual = zigzaggedQuantificationSpec qual
  scanSpecificationOfColorSpace _ =
    [ JpgScanSpecification { componentSelector = 1
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

  componentsOfColorSpace _ =
    [ JpgComponent { componentIdentifier      = 1
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
  
  encodingState qual _ = V.fromListN 3 [lumaState, chromaState, chromaState { _encComponentIndex = 2 }]
    where
      lumaState = EncoderState
        { _encComponentIndex = 0
        , _encBlockWidth     = 2
        , _encBlockHeight    = 2
        , _encQuantTable     = zigZagReorderForwardv $ lumaQuantTableAtQuality qual
        , _encDcHuffman      = makeInverseTable defaultDcLumaHuffmanTree
        , _encAcHuffman      = makeInverseTable defaultAcLumaHuffmanTree
        }
      chromaState = EncoderState
        { _encComponentIndex = 1
        , _encBlockWidth     = 1
        , _encBlockHeight    = 1
        , _encQuantTable     = zigZagReorderForwardv $ chromaQuantTableAtQuality qual
        , _encDcHuffman      = makeInverseTable defaultDcChromaHuffmanTree
        , _encAcHuffman      = makeInverseTable defaultAcChromaHuffmanTree
        }

instance JpgEncodable PixelRGB8 where
  additionalBlocks _ = [] where
    _adobe14 = JpgAdobeApp14
        { _adobeDctVersion = 100
        , _adobeFlag0      = 0
        , _adobeFlag1      = 0
        , _adobeTransform  = AdobeUnknown
        }

  imageHuffmanTables _ =
    [ prepareHuffmanTable DcComponent 0 defaultDcLumaHuffmanTable
    , prepareHuffmanTable AcComponent 0 defaultAcLumaHuffmanTable
    ]

  scanSpecificationOfColorSpace _ = fmap build "RGB" where
    build c = JpgScanSpecification
      { componentSelector = fromIntegral $ fromEnum c
      , dcEntropyCodingTable = 0
      , acEntropyCodingTable = 0
      }

  componentsOfColorSpace _ = fmap build "RGB" where
    build c = JpgComponent
      { componentIdentifier      = fromIntegral $ fromEnum c
      , horizontalSamplingFactor = 1
      , verticalSamplingFactor   = 1
      , quantizationTableDest    = 0
      }

  encodingState qual _ = V.fromListN 3 $ fmap build [0 .. 2] where
    build ix = EncoderState
      { _encComponentIndex = ix
      , _encBlockWidth     = 1
      , _encBlockHeight    = 1
      , _encQuantTable     = zigZagReorderForwardv $ lumaQuantTableAtQuality qual
      , _encDcHuffman      = makeInverseTable defaultDcLumaHuffmanTree
      , _encAcHuffman      = makeInverseTable defaultAcLumaHuffmanTree
      }

instance JpgEncodable PixelCMYK8 where
  additionalBlocks _ = [] where
    _adobe14 = JpgAdobeApp14
        { _adobeDctVersion = 100
        , _adobeFlag0      = 32768
        , _adobeFlag1      = 0
        , _adobeTransform  = AdobeYCck
        }
    
  imageHuffmanTables _ =
    [ prepareHuffmanTable DcComponent 0 defaultDcLumaHuffmanTable
    , prepareHuffmanTable AcComponent 0 defaultAcLumaHuffmanTable
    ]

  scanSpecificationOfColorSpace _ = fmap build "CMYK" where
    build c = JpgScanSpecification
      { componentSelector = fromIntegral $ fromEnum c
      , dcEntropyCodingTable = 0
      , acEntropyCodingTable = 0
      }

  componentsOfColorSpace _ = fmap build "CMYK" where
    build c = JpgComponent
      { componentIdentifier      = fromIntegral $ fromEnum c
      , horizontalSamplingFactor = 1
      , verticalSamplingFactor   = 1
      , quantizationTableDest    = 0
      }

  encodingState qual _ = V.fromListN 4 $ fmap build [0 .. 3] where
    build ix = EncoderState
      { _encComponentIndex = ix
      , _encBlockWidth     = 1
      , _encBlockHeight    = 1
      , _encQuantTable     = zigZagReorderForwardv $ lumaQuantTableAtQuality qual
      , _encDcHuffman      = makeInverseTable defaultDcLumaHuffmanTree
      , _encAcHuffman      = makeInverseTable defaultAcLumaHuffmanTree
      }

-- | Equivalent to 'encodeJpegAtQuality', but will store the following
-- metadatas in the file using a JFIF block:
--
--  * 'Codec.Picture.Metadata.DpiX'
--  * 'Codec.Picture.Metadata.DpiY' 
--
encodeJpegAtQualityWithMetadata :: Word8                -- ^ Quality factor
                                -> Metadatas
                                -> Image PixelYCbCr8    -- ^ Image to encode
                                -> L.ByteString         -- ^ Encoded JPEG
encodeJpegAtQualityWithMetadata = encodeDirectJpegAtQualityWithMetadata

-- | Equivalent to 'encodeJpegAtQuality', but will store the following
-- metadatas in the file using a JFIF block:
--
--  * 'Codec.Picture.Metadata.DpiX'
--  * 'Codec.Picture.Metadata.DpiY' 
--
-- This function also allow to create JPEG files with the following color
-- space:
--
--  * Y ('Pixel8') for greyscale.
--  * RGB ('PixelRGB8') with no color downsampling on any plane
--  * CMYK ('PixelCMYK8') with no color downsampling on any plane
--
encodeDirectJpegAtQualityWithMetadata :: forall px. (JpgEncodable px)
                                      => Word8                -- ^ Quality factor
                                      -> Metadatas
                                      -> Image px             -- ^ Image to encode
                                      -> L.ByteString         -- ^ Encoded JPEG
encodeDirectJpegAtQualityWithMetadata quality metas img = encode finalImage where
  !w = imageWidth img
  !h = imageHeight img
  finalImage = JpgImage $
      encodeMetadatas metas ++
      additionalBlocks img ++
      [ JpgQuantTable $ quantTableSpec img (fromIntegral quality)
      , JpgScans JpgBaselineDCTHuffman hdr
      , JpgHuffmanTable $ imageHuffmanTables img
      , JpgScanBlob scanHeader encodedImage
      ]

  !outputComponentCount = componentCount (undefined :: px)

  scanHeader = scanHeader'{ scanLength = fromIntegral $ calculateSize scanHeader' }
  scanHeader' = JpgScanHeader
      { scanLength = 0
      , scanComponentCount = fromIntegral outputComponentCount
      , scans = scanSpecificationOfColorSpace img
      , spectralSelection = (0, 63)
      , successiveApproxHigh = 0
      , successiveApproxLow  = 0
      }

  hdr = hdr' { jpgFrameHeaderLength   = fromIntegral $ calculateSize hdr' }
  hdr' = JpgFrameHeader
    { jpgFrameHeaderLength   = 0
    , jpgSamplePrecision     = 8
    , jpgHeight              = fromIntegral h
    , jpgWidth               = fromIntegral w
    , jpgImageComponentCount = fromIntegral outputComponentCount
    , jpgComponents          = componentsOfColorSpace img
    }

  !maxSampling = maximumSubSamplingOf img
  !horizontalMetaBlockCount = w `divUpward` (dctBlockSize * maxSampling)
  !verticalMetaBlockCount = h `divUpward` (dctBlockSize * maxSampling)
  !componentDef = encodingState (fromIntegral quality) img

  encodedImage = runST $ do
    dc_table <- M.replicate outputComponentCount 0
    block <- createEmptyMutableMacroBlock
    workData <- createEmptyMutableMacroBlock
    zigzaged <- createEmptyMutableMacroBlock
    writeState <- newWriteStateRef

    rasterMap horizontalMetaBlockCount verticalMetaBlockCount $ \mx my ->
      V.forM_ componentDef $ \(EncoderState comp sizeX sizeY table dc ac) -> 
        let !xSamplingFactor = maxSampling - sizeX + 1
            !ySamplingFactor = maxSampling - sizeY + 1
            !extractor = extractBlock img block xSamplingFactor ySamplingFactor outputComponentCount
        in
        rasterMap sizeX sizeY $ \subX subY -> do
          let !blockY = my * sizeY + subY
              !blockX = mx * sizeX + subX
          prev_dc <- dc_table `M.unsafeRead` comp
          extracted <- extractor comp blockX blockY
          (dc_coeff, neo_block) <- encodeMacroBlock table workData zigzaged prev_dc extracted
          (dc_table `M.unsafeWrite` comp) $ fromIntegral dc_coeff
          serializeMacroBlock writeState dc ac neo_block

    finalizeBoolWriter writeState

