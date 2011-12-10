{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Codec.Picture.Jpg where

import Control.Applicative( (<$>), (<*>))
import Control.Monad( when, replicateM, forM )
import Control.Monad.ST( ST, runST )
import Control.Monad.Trans( lift )
import qualified Control.Monad.State as S
{-import Control.Monad.State-}
import Data.List( find, foldl' )
import Data.Bits
import Data.Int
import Data.Word
import Data.Serialize
import Data.Maybe( fromJust )
import Data.Array.Unboxed
import Data.Array.ST
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
    | JpgHuffmanTableMarker
    | JpgStartOfScan
    | JpgAppSegment Word8
    | JpgExtensionSegment Word8

    | JpgRestartInterval
    deriving (Eq, Show)


data JpgFrame = 
      JpgAppFrame     !Word8 B.ByteString
    | JpgExtension    !Word8 B.ByteString
    | JpgQuantTable   !JpgQuantTableSpec
    | JpgHuffmanTable !JpgHuffmanTableSpec !HuffmanTree
    | JpgScanBlob     !JpgScanHeader
    | JpgScans        !JpgFrameKind !JpgFrameHeader
    | JpgIntervalRestart B.ByteString
    deriving Show

data JpgFrameHeader = JpgFrameHeader
    { jpgFrameHeaderLength   :: !Word16
    , jpgSamplePrecision     :: !Word8
    , jpgHeight              :: !Word16
    , jpgWidth               :: !Word16
    , jpgImageComponentCount :: !Word8
    , jpgComponents          :: [JpgComponent]
    }
    deriving Show

data JpgComponent = JpgComponent
    { componentIdentifier       :: !Word8
      -- | Stored with 4 bits
    , horizontalSamplingFactor  :: !Word8
      -- | Stored with 4 bits
    , verticalSamplingFactor    :: !Word8
    , quantizationTableDest     :: !Word8
    }
    deriving Show

data JpgImage = JpgImage { jpgFrame :: [JpgFrame]}
    deriving Show

data JpgScan = JpgScan
    { jpgScanHeader :: !JpgScanHeader
    , jpgScanData   :: !B.ByteString
    }
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
    { scanLength :: !Word16
    , componentCount :: !Word8
    , scans :: [JpgScanSpecification]

      -- | (begin, end)
    , spectralSelection    :: (Word8, Word8)

      -- | Encoded as 4 bits
    , successiveApproxHigh :: !Word8

      -- | Encoded as 4 bits
    , successiveApproxLow :: !Word8
    }
    deriving Show

data JpgQuantTableSpec = JpgQuantTableSpec
    { quantTableSize :: !Word16
      -- | Stored on 4 bits
    , quantPrecision     :: !Word8

      -- | Stored on 4 bits
    , quantDestination   :: !Word8

    , quantTable         :: MacroBlock Int16
    }
    deriving Show

instance Serialize JpgQuantTableSpec where
    put _ = fail "Error unimplemented"
    get = do
        size <- getWord16be
        (precision, dest) <- get4BitOfEach
        coeffs <- replicateM 64 $ if precision == 0 
                then fromIntegral <$> getWord8
                else fromIntegral <$> getWord16be
        let sizeWordSize = 2
            precDestSize = 1
            readed = (if precision == 0 then 1 else 2) * 64 + sizeWordSize + precDestSize
        when (readed < size) (skip $ fromIntegral size - fromIntegral readed)
        return $ JpgQuantTableSpec
            { quantTableSize = size
            , quantPrecision = precision
            , quantDestination = dest
            , quantTable = listArray (0, 63) coeffs
            }

data DctComponent = DcComponent | AcComponent
    deriving (Eq, Show)

data JpgHuffmanTableSpec = JpgHuffmanTableSpec
    { huffmanTableSize        :: !Word16
      -- | 0 : DC, 1 : AC, stored on 4 bits
    , huffmanTableClass       :: !DctComponent
      -- | Stored on 4 bits
    , huffmanTableDest        :: !Word8
    
    , huffSizes :: !(UArray Word32 Word8)
    , huffCodes :: !(Array Word32 (UArray Int Word8))
    }
    deriving Show

data HuffmanTree = Branch HuffmanTree HuffmanTree 
                 | Leaf Word8
                 | Empty
                 deriving (Eq, Show)

type PackedTree = UArray Word16 Word16


-- | Decode a list of huffman values, not optimized for speed, but it 
-- should work.
huffmanDecode :: HuffmanTree -> BoolReader s Word8
huffmanDecode originalTree = S.get >>= huffDecode originalTree
  where huffDecode _     [] = fail "Meh"
        huffDecode Empty _  = fail "Meh"
        huffDecode (Branch l _) (False : rest) = huffDecode l rest
        huffDecode (Branch _ r) (True  : rest) = huffDecode r rest
        huffDecode (Leaf v) boolList = S.put boolList >> return v

-- | Convert a bytestring to a list of word8, removing restart
-- markers.
{-# INLINE markerRemoval #-}
markerRemoval :: B.ByteString -> [Word8]
markerRemoval = markerRemover . B.unpack
  where markerRemover (0xFF:0x00:rest) = 0xFF : markerRemover rest
        markerRemover (0xFF:   _:rest) = markerRemover rest
        markerRemover (x   :rest)      = x : markerRemover rest
        markerRemover []               = []

-- | Bitify a list of things to decode.
{-# INLINE bitifyString #-}
bitifyString :: [Word8] -> [Bool]
bitifyString = concatMap bitify
  where bitify v = [ testBit v 7 , testBit v 6 , testBit v 5 , testBit v 4
                   , testBit v 3 , testBit v 2 , testBit v 1 , testBit v 0 ]

-- | Transform an huffman table to it's graphviz representation.
exportHuffmanTree :: JpgFrame -> Maybe String
exportHuffmanTree (JpgHuffmanTable _ t) = Just $ "digraph a {\n" ++ fst (stringify t (0 :: Int)) "}\n"
  where stringify (Branch left right) i = (fl . fr . thisNode . linka . linkb, i3 + 1)
            where lnode = "n" ++ show (i + 1)
                  rnode = "n" ++ show i2
                  thisNode = str $ "n" ++ show i ++ "[label=\"\", shape=\"box\"];\n"
                  linka = str $ "n" ++ show i ++ " -> " ++ lnode ++ " [label=\"0\"];\n"
                  linkb = str $ "n" ++ show i ++ " -> " ++ rnode ++ " [label=\"1\"];\n"
                  (fl, i2) = stringify left (i + 1)
                  (fr, i3) = stringify right i2
        stringify (Leaf v) i = (str $ "n" ++ show i ++ " [label=\"" ++ show v ++ "\"];\n", i + 1)
        stringify Empty i = (str $ "n" ++ show i ++ " [label=\"Empty\"];\n", i + 1)

        str a = (a ++)
exportHuffmanTree _ = Nothing

buildHuffmanTree :: JpgHuffmanTableSpec -> HuffmanTree
buildHuffmanTree table = foldl' (\a v -> insertHuffmanVal a v) Empty 
                       . concatMap (\(i, t) -> map (i + 1,) $ elems t) 
                       . assocs $ huffCodes table
  where isTreeFullyDefined Empty = False
        isTreeFullyDefined (Leaf _) = True
        isTreeFullyDefined (Branch l r) = isTreeFullyDefined l && isTreeFullyDefined r

        insertHuffmanVal Empty (0, val) = Leaf val
        insertHuffmanVal Empty (d, val) = Branch (insertHuffmanVal Empty (d - 1, val)) Empty
        insertHuffmanVal (Branch l r) (d, val)  
            | isTreeFullyDefined l = Branch l (insertHuffmanVal r (d - 1, val))
            | otherwise            = Branch (insertHuffmanVal l (d - 1, val)) r
        insertHuffmanVal (Leaf _) _ = error "Inserting in value, shouldn't happen"

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

instance Serialize JpgHuffmanTableSpec where
    put = error "Unimplemented"
    get = do
        offsetBegin <- remaining
        size <- getWord16be
        (huffClass, huffDest) <- get4BitOfEach
        sizes <- replicateM 16 getWord8
        codes <- forM sizes $ \s -> do
            let si = fromIntegral s
            listArray (0, si - 1) <$> replicateM (fromIntegral s) getWord8
        offsetEnd <- remaining
        when (offsetBegin - offsetEnd < fromIntegral size)
             (skip $ fromIntegral size - (offsetBegin - offsetEnd))
        return $ JpgHuffmanTableSpec 
            { huffmanTableSize = size
            , huffmanTableClass = 
                (if huffClass == 0 then DcComponent else AcComponent)
            , huffmanTableDest = huffDest
            , huffSizes = listArray (0, 15) sizes
            , huffCodes = listArray (0, 15) codes
            }

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

traShow :: (Show a) => a -> a
traShow a = trace (show a) a

parseFrames :: Get [JpgFrame]
parseFrames = do
    kind <- get
    trace (show kind) $ case kind of
        JpgAppSegment c ->
            (\frm lst -> JpgAppFrame c frm : lst) <$> takeCurrentFrame <*> parseFrames
        JpgExtensionSegment c ->
            (\frm lst -> JpgExtension c frm : lst) <$> takeCurrentFrame <*> parseFrames
        JpgQuantizationTable -> 
            (\quant lst -> JpgQuantTable quant : lst) <$> get <*> parseFrames
        JpgRestartInterval ->
            (\frm lst -> JpgIntervalRestart frm : lst) <$> takeCurrentFrame <*> parseFrames
        JpgHuffmanTableMarker ->
            (\huffTable lst -> JpgHuffmanTable huffTable (buildHuffmanTree huffTable): lst) <$> get <*> parseFrames
        JpgStartOfScan -> 
            (\frm lst -> JpgScanBlob frm : lst) <$> get <*> return [] -- parseFrames
        _ -> (\hdr lst -> JpgScans kind hdr : lst) <$> get <*> parseFrames

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
secondStartOfFrameByteOfKind JpgHuffmanTableMarker = 0xC4
secondStartOfFrameByteOfKind JpgDifferentialSequentialDCT_Arithmetic = 0xCD
secondStartOfFrameByteOfKind JpgDifferentialProgressiveDCT_Arithmetic = 0xCE
secondStartOfFrameByteOfKind JpgDifferentialLossless_Arithmetic = 0xCF
secondStartOfFrameByteOfKind JpgQuantizationTable = 0xDB
secondStartOfFrameByteOfKind JpgStartOfScan = 0xDA
secondStartOfFrameByteOfKind JpgRestartInterval = 0xDD
secondStartOfFrameByteOfKind (JpgAppSegment a) = a
secondStartOfFrameByteOfKind (JpgExtensionSegment a) = a

instance Serialize JpgFrameKind where
    put v = putWord8 0xFF >> put (secondStartOfFrameByteOfKind v)
    get = do
        word <- getWord8
        word2 <- getWord8
        when (word /= 0xFF) (do leftData <- remaining
                                fail $ "Invalid Frame marker (" ++ show word 
                                    ++ ", remaining : " ++ show leftData ++ ")")
        return $ case word2 of
            0xC0 -> JpgBaselineDCT_Huffman
            0xC1 -> JpgExtendedSequentialDCT_Huffman
            0xC2 -> JpgProgressiveDCT_Huffman
            0xC3 -> JpgLossless_Huffman
            0xC4 -> JpgHuffmanTableMarker
            0xC5 -> JpgDifferentialSequentialDCT_Huffman
            0xC6 -> JpgDifferentialProgressiveDCT_Huffman
            0xC7 -> JpgDifferentialLossless_Huffman
            0xC9 -> JpgExtendedSequential_Arithmetic
            0xCA -> JpgProgressiveDCT_Arithmetic
            0xCB -> JpgLossless_Arithmetic
            0xCD -> JpgDifferentialSequentialDCT_Arithmetic
            0xCE -> JpgDifferentialProgressiveDCT_Arithmetic
            0xCF -> JpgDifferentialLossless_Arithmetic
            0xDA -> JpgStartOfScan
            0xDB -> JpgQuantizationTable
            0xDD -> JpgRestartInterval
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
        quantTableIndex <- getWord8
        return $ JpgComponent
            { componentIdentifier = ident
            , horizontalSamplingFactor = horiz
            , verticalSamplingFactor = vert
            , quantizationTableDest = quantTableIndex
            }
    put v = do
        put $ componentIdentifier v
        put4BitsOfEach (horizontalSamplingFactor v) $ verticalSamplingFactor v
        put $ quantizationTableDest v

instance Serialize JpgFrameHeader where
    get = do
        beginOffset <- remaining
        frmHLength <- getWord16be
        samplePrec <- getWord8
        h <- getWord16be
        w <- getWord16be
        compCount <- getWord8
        components <- replicateM (fromIntegral compCount) get
        endOffset <- remaining
        when (beginOffset - endOffset < fromIntegral frmHLength) 
             (skip $ fromIntegral frmHLength - (beginOffset - endOffset))
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

type BoolReader s a = S.StateT [Bool] (ST s) a

dcValueOfMacroBlock :: (IArray UArray a) => MacroBlock a -> a
dcValueOfMacroBlock block = block ! 0

-- | Apply a quantization matrix to a macroblock
{-# INLINE deQuantize #-}
deQuantize :: (IArray UArray a, Num a, Integral a)
           => MacroBlock Int16 -> MacroBlock a -> MacroBlock a
deQuantize table block = makeMacroBlock . map dequant $ indices table
    where dequant i = fromIntegral $ r * l
            where r = fromIntegral (table ! i) :: Int
                  l = fromIntegral (block ! i)

idctCoefficientMatrix :: MacroBlock Float
idctCoefficientMatrix = 
  makeMacroBlock [idctCoefficient x u | x <- [1, 3 .. 15], u <- [0 .. 7 :: Int]]
    where idctCoefficient _ 0 = 0.5 / sqrt 2.0
          idctCoefficient x u = 0.5 * cos(pi / 16.0 * xu)
            where xu = fromIntegral $ x * u

inverseDirectCosineTransform :: (Integral a, IArray UArray a) => MacroBlock a -> MacroBlock a
inverseDirectCosineTransform block =
  makeMacroBlock [coeff i j | i <- [0 .. 7], j <- [0 .. 7] ]
    where dotProduct lst = sum $ (\(a,b) -> a * b) <$> lst
          line i = map (idctCoefficientMatrix !) [ i * 8 .. i * 8 + 7 ]
          column j = map (\i -> fromIntegral $ block ! i) [ j, j + 8 .. 63     ]
          coeff i j = truncate . dotProduct $ zip (line i) (column j)

zigZagReorder :: (IArray UArray a) => MacroBlock a -> MacroBlock a
zigZagReorder block = ixmap (0,63) reorder block
    where reorder i = fromIntegral $ zigZagOrder ! i

          zigZagOrder :: MacroBlock Word8
          zigZagOrder = makeMacroBlock $ concat
              [[ 0, 1, 5, 6,14,15,27,28]
              ,[ 2, 4, 7,13,16,26,29,42]
              ,[ 3, 8,12,17,25,30,41,43]
              ,[ 9,11,18,24,31,40,44,53]
              ,[10,19,23,32,39,45,52,54]
              ,[20,22,33,38,46,51,55,60]
              ,[21,34,37,47,50,56,59,61]
              ,[35,36,48,49,57,58,62,63]
              ]

promoteMacroBlock :: (Integral a, Num b, IArray UArray a, IArray UArray b)
                  => MacroBlock a -> MacroBlock b
promoteMacroBlock = amap fromIntegral

-- | This is one of the most important function of the decoding,
-- it form the barebone decoding pipeline for macroblock. It's all
-- there is to know for macro block transformation
decodeMacroBlock :: MacroBlock Int16 -> MacroBlock Word8 -> MacroBlock Word8
decodeMacroBlock quantizationTable =
    inverseDirectCosineTransform . zigZagReorder 
                                 . deQuantize quantizationTable
                                 . promoteMacroBlock

unpackInt :: Int -> BoolReader s Word8
unpackInt n = do
    bits <- S.get
    let (toUnpack, rest) = n `splitAt` bits  
        bitStep acc True = acc `shiftL` 1 + 1
        bitStep acc False = acc `shiftL` 1
    S.put rest
    return $ foldl' bitStep 0 toUnpack

decodeInt :: Int -> BoolReader s Word8
decodeInt ssss = do
    bits <- S.get
    let dataRange = 1 `shiftL` (ssss - 1)
    case bits of
      []     -> fail "Not engouh bits"
      (True : rest) -> do
          S.put rest
          (dataRange +) <$> unpackInt ssss
      (False : rest) -> do
          S.put rest
          (1 - dataRange * 2 +) <$> unpackInt ssss

dcCoefficientDecode :: HuffmanTree -> BoolReader s Word8
dcCoefficientDecode dcTree = do
    ssss <- huffmanDecode dcTree
    if ssss == 0 
       then return 0
       else decodeInt $ fromIntegral ssss

-- | Use an array of integer?
acCoefficientsDecode :: HuffmanTree -> BoolReader s [Word8]
acCoefficientsDecode acTree = concat <$> parseAcCoefficient 63
  where parseAcCoefficient 0 = return []
        parseAcCoefficient n = do
            rrrrssss <- huffmanDecode acTree
            let rrrr = (rrrrssss `shiftR` 4) .&. 0xF
                ssss =  rrrrssss .&. 0xF
            case (rrrr, ssss) of
              (0,   0) -> return [replicate n 0] 
              (0xF, 0) -> (replicate 16 0 :) <$> parseAcCoefficient (n - 16)
              _        -> do
                  decoded <- decodeInt $ fromIntegral ssss
                  ([decoded]:) <$> parseAcCoefficient (n - 1)

-- | Represent a compact array of 8 * 8 values. The size
-- is not guarenteed by type system, but if makeMacroBlock is
-- used, everything should be fine size-wise
type MacroBlock a = UArray Word32 a

makeMacroBlock :: (IArray UArray a) => [a] -> MacroBlock a
makeMacroBlock = listArray (0, 63)

-- | Decompress a macroblock from a bitstream given the current configuration
-- from the frame.
decompressMacroBlock :: HuffmanTree         -- ^ Tree used for DC coefficient
                     -> HuffmanTree         -- ^ Tree used for Ac coefficient
                     -> MacroBlock Int16    -- ^ Current quantization table
                     -> Word8               -- ^ Previous dc value
                     -> BoolReader s (MacroBlock Word8)
decompressMacroBlock dcTree acTree quantizationTable previousDc = do
    dcDeltaCoefficient <- dcCoefficientDecode dcTree
    acCoefficients <- acCoefficientsDecode acTree
    let block = makeMacroBlock $ 
                    previousDc + dcDeltaCoefficient : acCoefficients
    return $ decodeMacroBlock quantizationTable block

gatherQuantTables :: JpgImage -> [JpgQuantTableSpec]
gatherQuantTables img = [t | JpgQuantTable t <- jpgFrame img]

gatherHuffmanTables :: JpgImage -> [(JpgHuffmanTableSpec, HuffmanTree)]
gatherHuffmanTables img = [(ta, t) | JpgHuffmanTable ta t <- jpgFrame img]

gatherScanInfo :: JpgImage -> (JpgFrameKind, JpgFrameHeader)
gatherScanInfo img = fromJust $ unScan <$> find scanDesc (jpgFrame img)
    where scanDesc (JpgScans _ _) = True
          scanDesc _ = False

          unScan (JpgScans a b) = (a,b)
          unScan _ = error "If this can happen, the JPEG image is ill-formed"

unpackMacroBlock :: (IArray UArray a)
                 => Word32 -- ^ Width coefficient
                 -> Word32 -- ^ Height coefficient
                 -> Word32 -- ^ x
                 -> Word32 -- ^ y
                 -> MacroBlock a
                 -> [((Word32, Word32), a)]
unpackMacroBlock      1      1 x y block =
    [((i + x * 8, j + y * 8), block ! (i + j * 8)) 
                                | i <- [0 .. 7], j <- [0 .. 7] ]

unpackMacroBlock wCoeff hCoeff x y block =
    [(((i + x * 8) * wCoeff + wDup,
       (j + y * 8) * hCoeff + hDup), block ! (i + j * 8)) 
                    | i <- [0 .. 7], j <- [0 .. 7] 
                    -- Repetition to spread macro block
                    , wDup <- [0 .. wCoeff - 1]
                    , hDup <- [0 .. hCoeff - 1]
                    ]

type DcCoefficient = Word8


decodeImage :: Int -> [(Int, DcCoefficient -> BoolReader s [((Word32, Word32), Word8)])]
            -> BoolReader s [((Word32, Word32), (Int, Word8))]
decodeImage compCount lst = concat <$> do
    dcArray <- lift $ (newArray (0, compCount - 1) 0  :: ST s (STUArray s Int Word8))
    forM lst $ \(comp, f) -> do
        dc <- lift $ dcArray `readArray` comp
        block@((_,dcCoeff):_) <- f dc
        lift $ (dcArray `writeArray` comp) dcCoeff
        return [(idx, (comp, val)) | (idx, val) <- block]


-- | An MCU (Minimal coded unit) is an unit of data for all components
-- (Y, Cb & Cr), taking into account downsampling.
buildJpegImageDecoder :: JpgImage -> [(Int, DcCoefficient -> BoolReader s [((Word32, Word32), Word8)] )]
buildJpegImageDecoder img = allBlockToDecode
  where huffmans = gatherHuffmanTables img
        huffmanForComponent dcOrAc comp = head
            [t | (h,t) <- huffmans
               , huffmanTableClass h == dcOrAc
               , let isY = componentIdentifier comp == 1
               , huffmanTableDest h == (if isY then 0 else 1)]

        quants = gatherQuantTables img
        quantForComponent comp = head
            [quantTable q | q <- quants
                          , let isY = componentIdentifier comp == 1
                          , quantDestination q == (if isY then 0 else 1)]

        (_, scanInfo) = gatherScanInfo img
        imgWidth = fromIntegral $ jpgWidth scanInfo
        imgHeight = fromIntegral $ jpgHeight scanInfo


        horizontalBlockCount =
          imgWidth `div` fromIntegral (maximum [horizontalSamplingFactor c | 
                                                    c <- jpgComponents scanInfo] * 8)

        verticalBlockCount =
          imgHeight `div` fromIntegral (maximum [horizontalSamplingFactor c | 
                                                    c <- jpgComponents scanInfo] * 8)

        -- This monstrous list comprehension build a list of function
        -- for all macroblcoks at once, all that remains is to fold
        -- over it to decode
        allBlockToDecode = 
          [(compIdx, \dc -> (return . unpacker) =<< decompressMacroBlock dcTree acTree qTable dc)
                  | x <- [0 .. horizontalBlockCount - 1]
                  , y <- [0 ..  verticalBlockCount - 1]
                  , (compIdx, component) <- zip [0..] $ jpgComponents scanInfo
                  , let acTree = huffmanForComponent AcComponent component
                        dcTree = huffmanForComponent DcComponent component
                        qTable = quantForComponent  component
                        horizCount = horizontalSamplingFactor component
                        vertCount = verticalSamplingFactor component

                  , xd <- [0 .. horizCount - 1]
                  , yd <- [0 .. vertCount - 1]
                  , let unpacker = unpackMacroBlock (fromIntegral horizCount)
                                                    (fromIntegral vertCount)
                                                    (x + fromIntegral xd) (y + fromIntegral yd)
                  ]



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

jpegDecode :: B.ByteString -> Either String (Image PixelYCbCr)
jpegDecode sr = case decode file of
  Left err -> Left err
  Right img ->
      let bitList = [] -- bitifyString $ markerRemoval
          decoder = buildJpegImageDecoder img >>= decodeImage 3
          imgWidth = 0
          imgHeight = 0
          imageSize = ((0, 0), (imgWidth - 1, imgHeight - 1))        
          setter (PixelYCbCr _ cb cr) (0, v) = PixelYCbCr v cb cr
          setter (PixelYCbCr y  _ cr) (1, v) = PixelYCbCr y  v cr
          setter (PixelYCbCr y cb  _) (2, v) = PixelYCbCr y cb  v
          setter _ _ = error "Impossible jpeg decoding can happen"
      in accumArray setter (PixelYCbCr 0 0 0) imageSize . runST $ evalStateT bitList decoder

jpegTest :: FilePath -> IO ()
jpegTest path = do
    file <- B.readFile path
    case decode file of
         Left err -> print err
         Right img -> do
             mapM_ (\a ->
                 case exportHuffmanTree a of
                    Nothing -> return ()
                    Just str -> putStrLn str
                 ) $ jpgFrame img
             mapM_ (\a -> print a >> putStrLn "\n\n") $ jpgFrame img

