module Codec.Picture.Jpg where

import Control.Applicative( (<$>), (<*>) )
import Control.Monad( when, replicateM )
import Data.Bits
import Data.Word
import Data.Serialize
import Data.Array.Unboxed
import qualified Data.ByteString as B
{-import qualified Data.ByteString.Lazy as Lb-}

import Debug.Trace
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
    | JpgQuantizationTable
    | JpgAppSegment Word8
    | JpgExtensionSegment Word8
    deriving Show


data JpgFrame = 
      JpgAppFrame Word8 B.ByteString
    | JpgExtension Word8 B.ByteString
    | JpgQuantTable B.ByteString
    | JpgScans JpgFrameKind JpgFrameHeader [JpgScanSpecification]
    deriving Show

data JpgFrameHeader = JpgFrameHeader
    { jpgFrameHeaderLength   :: Word16
    , jpgSamplePrecision     :: Word8
    , jpgHeight              :: Word16
    , jpgWidth               :: Word16
    , jpgImageComponentCount :: Word8
    , jpgComponents          :: [JpgComponent]
    }
    deriving Show

data JpgComponent = JpgComponent
    { componentIdentifier       :: Word8
      -- | Stored with 4 bits
    , horizontalSamplingFactor  :: Word8
      -- | Stored with 4 bits
    , verticalSamplingFactor    :: Word8
    , quantizationTableDest     :: Word8
    }
    deriving Show

data JpgImage = JpgImage { jpgFrame :: [JpgFrame]}
    deriving Show

data JpgScanSpecification = JpgScanSpecification 
    { componentSelector :: !Word8
      -- | Encoded as 4 bits
    , dcEntropyCodingTable :: !Word8
      -- | Encoded as 4 bits
    , acEntropyCodingTable :: !Word8
    
    }
    deriving Show

data JpgScanHeader = JpgScanHeader
    { scanLength :: Word16
    , componentCount :: Word8
    , scans :: [JpgScanSpecification]

      -- | (begin, end)
    , spectralSelection    :: (Word8, Word8)

      -- | Encoded as 4 bits
    , successiveApproxHigh :: Word8

      -- | Encoded as 4 bits
    , successiveApproxLow :: Word8
    }

{-data JpgQuantTable = JpgQuantTable-}
    {-{ quantTableSize :: Word16-}
    {-, -}
    {-}-}

data JpgHuffmanTable = JpgHuffmanTable
    { huffmanTableSize :: Word16
      -- | 0 : DC, 1 : AC, stored on 4 bits
    , huffmanTableClass       :: Word8
      -- | Stored on 4 bits
    , huffmanTableDest        :: Word8
    
    , sizes :: UArray Word32 Word8
    , codes :: Array Word32 (UArray Word32 Word8)
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

eatUntilCode :: Get ()
eatUntilCode = do
    code <- lookAhead getWord8
    if code == 0xFF
       then return ()
       else skip 1 >> eatUntilCode

{-instance Serialize JpgFrame where-}
    {-put = error "Unimplemented"-}
    {-get = do-}
        {-hdr <- get-}
        {-return $ JpgFrame { jpgFrameHeader = hdr }-}

instance Serialize JpgImage where
    put = error "Unimplemented"
    get = do
        let startOfImageMarker = 0xD8
            -- endOfImageMarker = 0xD9
        checkMarker commonMarkerFirstByte startOfImageMarker
        eatUntilCode 
        frames <- parseFrames
        {-checkMarker commonMarkerFirstByte endOfImageMarker-}
        return $ JpgImage { jpgFrame = frames }

dropCurrentFrame :: Get ()
dropCurrentFrame = do
    size <- getWord16be
    trace ("Skipping : " ++ show size) $ skip (fromIntegral size - 2)

takeCurrentFrame :: Get B.ByteString
takeCurrentFrame = do
    size <- getWord16be
    trace ("taking : " ++ show size) $ getBytes (fromIntegral size - 2)

parseFrames :: Get [JpgFrame]
parseFrames = do
    kind <- get
    trace (show kind) $ case kind of
        JpgAppSegment c ->
            (\frm lst -> JpgAppFrame c frm : lst) <$> takeCurrentFrame <*> parseFrames
        JpgExtensionSegment c ->
            (\frm lst -> JpgExtension c frm : lst) <$> takeCurrentFrame <*> parseFrames
        JpgQuantizationTable -> 
            (\frm lst -> JpgQuantTable frm : lst) <$> takeCurrentFrame <*> parseFrames
        _ -> (\hdr -> [JpgScans kind hdr []]) <$> get

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
secondStartOfFrameByteOfKind JpgQuantizationTable = 0xDB
secondStartOfFrameByteOfKind (JpgAppSegment a) = a
secondStartOfFrameByteOfKind (JpgExtensionSegment a) = a

instance Serialize JpgFrameKind where
    put v = putWord8 0xFF >> put (secondStartOfFrameByteOfKind v)
    get = do
        word <- getWord8
        word2 <- getWord8
        when (word /= 0xFF) (fail "Invalid Frame marker")
        return $ case word2 of
            0xC0 -> JpgBaselineDCT_Huffman
            0xC1 -> JpgExtendedSequentialDCT_Huffman
            0xC2 -> JpgProgressiveDCT_Huffman
            0xC3 -> JpgLossless_Huffman
            0xC5 -> JpgDifferentialSequentialDCT_Huffman
            0xC6 -> JpgDifferentialProgressiveDCT_Huffman
            0xC7 -> JpgDifferentialLossless_Huffman
            0xC9 -> JpgExtendedSequential_Arithmetic
            0xCA -> JpgProgressiveDCT_Arithmetic
            0xCB -> JpgLossless_Arithmetic
            0xCD -> JpgDifferentialSequentialDCT_Arithmetic
            0xCE -> JpgDifferentialProgressiveDCT_Arithmetic
            0xCF -> JpgDifferentialLossless_Arithmetic
            0xDB -> JpgQuantizationTable
            a -> if a >= 0xF0 then JpgExtensionSegment a
                 else if a >= 0xE0 then JpgAppSegment a
                 else error ("Invalid frame marker (" ++ show a ++ ")")

put4BitsOfEach :: Word8 -> Word8 -> Put
put4BitsOfEach a b = put $ (a `shiftL` 4) .|. b

get4BitOfEach :: Get (Word8, Word8)
get4BitOfEach = do
    val <- get
    return ((val `shiftR` 4) .&. 0xF, val .&. 0xF)

instance Serialize JpgComponent where
    get = do
        ident <- getWord8
        (horiz, vert) <- get4BitOfEach
        quantTable <- getWord8
        return $ JpgComponent
            { componentIdentifier = ident
            , horizontalSamplingFactor = horiz
            , verticalSamplingFactor = vert
            , quantizationTableDest = quantTable
            }
    put v = do
        put $ componentIdentifier v
        put4BitsOfEach (horizontalSamplingFactor v) $ verticalSamplingFactor v
        put $ quantizationTableDest v

instance Serialize JpgFrameHeader where
    get = do
        frmHLength <- getWord16be
        samplePrec <- getWord8
        h <- getWord16be
        w <- getWord16be
        compCount <- getWord8
        components <- replicateM (fromIntegral compCount) get
        return $ JpgFrameHeader
            { jpgFrameHeaderLength = frmHLength
            , jpgSamplePrecision = samplePrec
            , jpgHeight = h
            , jpgWidth = w
            , jpgImageComponentCount = compCount
            , jpgComponents = components
            }

    put v = do
        putWord16be $ jpgFrameHeaderLength v
        putWord8    $ jpgSamplePrecision v
        putWord16be $ jpgHeight v
        putWord16be $ jpgWidth v
        putWord8    $ jpgImageComponentCount v
        mapM_ put   $ jpgComponents v

instance Serialize JpgScanSpecification where
    put v = do
        put $ componentSelector v
        put4BitsOfEach (dcEntropyCodingTable v) $ acEntropyCodingTable v

    get = do
        compSel <- get
        (dc, ac) <- get4BitOfEach
        return $ JpgScanSpecification {
            componentSelector = compSel
          , dcEntropyCodingTable = dc
          , acEntropyCodingTable = ac
          }

instance Serialize JpgScanHeader where
    get = do
        thisScanLength <- getWord16be
        compCount <- getWord8
        comp <- replicateM (fromIntegral compCount) get
        specBeg <- get
        specEnd <- get
        approxHigh <- get
        approxLow <- get

        return $ JpgScanHeader {
            scanLength = thisScanLength,
            componentCount = compCount,
            scans = comp,
            spectralSelection = (specBeg, specEnd),
            successiveApproxHigh = approxHigh,
            successiveApproxLow = approxLow
        }

    put v = do
        put $ scanLength v
        put $ componentCount v
        mapM_ put $ scans v
        put . fst $ spectralSelection v
        put . snd $ spectralSelection v
        put $ successiveApproxHigh v
        put $ successiveApproxLow v

jpegTest :: FilePath -> IO ()
jpegTest path = do
    file <- B.readFile path
    case decode file of
         Left err -> print err
         Right img -> mapM_ (\a -> print a >> putStrLn "\n\n") $ jpgFrame img

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

