module Codec.Picture.Jpg where

import Control.Monad( when, replicateM )
import Data.Bits
import Data.Word
import Data.Serialize
{-import Data.Array.Unboxed-}

--------------------------------------------------
----            Types
--------------------------------------------------
data JpgFrameKind =
      JpgBaselineDCT_Huffman
    | JpgExtendedSequentialDCT_Huffman
    | JpgProgressiveDCT_Huffman
    | JpgLossless_Huffman
    | JpgDifferentialSequentialDCT_Huffman
    | JpgDifferentialProgressiveDCT_Huffman
    | JpgDifferentialLossless_Huffman
    | JpgExtendedSequential_Arithmetic
    | JpgProgressiveDCT_Arithmetic
    | JpgLossless_Arithmetic
    | JpgDifferentialSequentialDCT_Arithmetic
    | JpgDifferentialProgressiveDCT_Arithmetic
    | JpgDifferentialLossless_Arithmetic


data JpgFrameHeader = JpgFrameHeader 
    { jpgFrameHeaderLength   :: Word16
    , jpgSamplePrecision     :: Word8
    , jpgHeight              :: Word16
    , jpgWidth               :: Word16
    , jpgImageComponentCount :: Word8
    , jpgComponents          :: [JpgComponent]
    }

data JpgComponent = JpgComponent
    { componentIdentifier       :: Word8
      -- | Stored with 4 bits
    , horizontalSamplingFactor  :: Word8
      -- | Stored with 4 bits
    , verticalSamplingFactor    :: Word8
    , quantizationTableDest     :: Word8
    }

--------------------------------------------------
----            Serialization instances
--------------------------------------------------
commonMarkerFirstByte :: Word8
commonMarkerFirstByte = 0xFF

checkMarker :: Word8 -> Word8 -> Get ()
checkMarker b1 b2 = do
    rb1 <- getWord8
    rb2 <- getWord8
    if rb1 /= b1 || rb2 /= b2
       then fail "Invalid marker used"
       else return ()

data JpgImage = JpgImage {  }

instance Serialize JpgImage where
    put = error "Unimplemented"
    get = do
        let startOfImageMarker = 0xD8
            endOfImageMarker = 0xD9
        checkMarker commonMarkerFirstByte startOfImageMarker
        -- frame
        checkMarker commonMarkerFirstByte endOfImageMarker
        return $ JpgImage {  }

secondStartOfFrameByteOfKind :: JpgFrameKind -> Word8
secondStartOfFrameByteOfKind JpgBaselineDCT_Huffman = 0xC0
secondStartOfFrameByteOfKind JpgExtendedSequentialDCT_Huffman = 0xC1
secondStartOfFrameByteOfKind JpgProgressiveDCT_Huffman = 0xC2
secondStartOfFrameByteOfKind JpgLossless_Huffman = 0xC3
secondStartOfFrameByteOfKind JpgDifferentialSequentialDCT_Huffman = 0xC5
secondStartOfFrameByteOfKind JpgDifferentialProgressiveDCT_Huffman = 0xC6
secondStartOfFrameByteOfKind JpgDifferentialLossless_Huffman = 0xC7
secondStartOfFrameByteOfKind JpgExtendedSequential_Arithmetic = 0xC9
secondStartOfFrameByteOfKind JpgProgressiveDCT_Arithmetic = 0xCA
secondStartOfFrameByteOfKind JpgLossless_Arithmetic = 0xCB
secondStartOfFrameByteOfKind JpgDifferentialSequentialDCT_Arithmetic = 0xCD
secondStartOfFrameByteOfKind JpgDifferentialProgressiveDCT_Arithmetic = 0xCE
secondStartOfFrameByteOfKind JpgDifferentialLossless_Arithmetic = 0xCF

instance Serialize JpgFrameKind where
    put v = putWord8 0xFF >> put (secondStartOfFrameByteOfKind v)
    get = do
        word <- getWord8
        word2 <- getWord8
        when (word /= 0xFF) (fail "Invalid Frame marker")
        case word2 of
            0xC0 -> return JpgBaselineDCT_Huffman
            0xC1 -> return JpgExtendedSequentialDCT_Huffman
            0xC2 -> return JpgProgressiveDCT_Huffman
            0xC3 -> return JpgLossless_Huffman
            0xC5 -> return JpgDifferentialSequentialDCT_Huffman
            0xC6 -> return JpgDifferentialProgressiveDCT_Huffman
            0xC7 -> return JpgDifferentialLossless_Huffman
            0xC9 -> return JpgExtendedSequential_Arithmetic
            0xCA -> return JpgProgressiveDCT_Arithmetic
            0xCB -> return JpgLossless_Arithmetic
            0xCD -> return JpgDifferentialSequentialDCT_Arithmetic
            0xCE -> return JpgDifferentialProgressiveDCT_Arithmetic
            0xCF -> return JpgDifferentialLossless_Arithmetic
            _ -> fail "Invalid Jpg frame marker"

instance Serialize JpgComponent where
    get = do
        ident <- getWord8
        samplingFactor <- getWord8
        quantTable <- getWord8
        return $ JpgComponent
            { componentIdentifier = ident
            , horizontalSamplingFactor = (samplingFactor .&. 0xF0) `shiftR` 4
            , verticalSamplingFactor = samplingFactor .&. 0x0F
            , quantizationTableDest = quantTable
            }
    put v = do
        put $ componentIdentifier v
        put $ (horizontalSamplingFactor v `shiftL` 4) 
           .|. verticalSamplingFactor v
        put $ quantizationTableDest v

instance Serialize JpgFrameHeader where
    get = do
        frmHLength <- getWord16be
        samplePrec <- getWord8
        h <- getWord16be
        w <- getWord16be
        componentCount <- getWord8
        components <- replicateM (fromIntegral componentCount) get
        return $ JpgFrameHeader
            { jpgFrameHeaderLength = frmHLength
            , jpgSamplePrecision = samplePrec
            , jpgHeight = h
            , jpgWidth = w
            , jpgImageComponentCount = componentCount
            , jpgComponents = components
            }

    put v = do
        putWord16be $ jpgFrameHeaderLength v
        putWord8    $ jpgSamplePrecision v
        putWord16be $ jpgHeight v
        putWord16be $ jpgWidth v
        putWord8    $ jpgImageComponentCount v
        mapM_ put   $ jpgComponents v

{- 
-- | Extract a 8x8 block in the picture.
extractBlock :: UArray Word32 PixelRGB -> Word32 -> Word32 -> UArray Word32 PixelRGB
extractBlock arr x y 
  | (x + 1) * blockSize < width && (y + 1) * blockSize < height = array (0, blockElemCount)
    [arr ! (left, top) | left <- [blockLeft .. blockLeft + 8], top <- [blockTop .. blockTop + 8]]
  | (x + 1) * blockSize < width =
  | (y + 1) * blockSize < height =
    where blockSize = 8
          blockElemCount = blockSize * blockSize - 1
          (width, height) = bounds arr
          blockLeft = blockSize * x
          blockTop = blockSize * y
-}

