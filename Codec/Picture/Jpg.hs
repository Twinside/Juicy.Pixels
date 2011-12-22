{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Codec.Picture.Jpg( loadJpeg
                        , decodeJpeg
                        , jpegTest
                        ) where

import Control.Applicative( (<$>), (<*>))
import Control.Monad( when, replicateM, forM, forM_ )
import Control.Monad.ST( ST, runST )
import Control.Monad.Trans( lift )
import qualified Control.Monad.Trans.State as S

import Data.List( find, foldl', intersperse )
import Data.Bits
import Data.Int
import Data.Word
import Data.Serialize
import Data.Maybe( fromJust )
import Data.Array.Unboxed
import Data.Array.ST
import qualified Data.ByteString as B

import Codec.Picture.Types
import Codec.Picture.Jpg.DefaultTable

import Debug.Trace
import Text.Printf
import Text.Groom

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
      JpgAppFrame        !Word8 B.ByteString
    | JpgExtension       !Word8 B.ByteString
    | JpgQuantTable      ![JpgQuantTableSpec]
    | JpgHuffmanTable    ![(JpgHuffmanTableSpec, HuffmanTree)]
    | JpgScanBlob        !JpgScanHeader !B.ByteString
    | JpgScans           !JpgFrameKind !JpgFrameHeader
    | JpgIntervalRestart !Word16
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
    { -- | Stored on 4 bits
      quantPrecision     :: !Word8

      -- | Stored on 4 bits
    , quantDestination   :: !Word8

    , quantTable         :: MacroBlock Int16
    }
    deriving Show

-- | Type introduced only to avoid some typeclass overlapping
-- problem
newtype TableList a = TableList [a]

class SizeCalculable a where
    calculateSize :: a -> Int

instance (SizeCalculable a, Serialize a) => Serialize (TableList a) where
    put (TableList lst) = do
        putWord16be . fromIntegral $ sum [calculateSize table | table <- lst]
        mapM_ put lst

    get = TableList <$> (getWord16be >>= \s -> innerParse (fromIntegral s - 2))
      where innerParse :: Int -> Get [a]
            innerParse 0    = return []
            innerParse size = do
                onStart <- fromIntegral <$> remaining
                table <- get
                onEnd <- fromIntegral <$> remaining
                (table :) <$> innerParse (size - (onStart - onEnd))

instance SizeCalculable JpgQuantTableSpec where
    calculateSize table =
        1 + (fromIntegral (quantPrecision table) + 1) * 64

instance Serialize JpgQuantTableSpec where
    put table = do
        let precision = quantPrecision table
        put4BitsOfEach precision (quantDestination table)
        forM_ (elems $ quantTable table) $ \coeff ->
            if precision == 0 then putWord8 $ fromIntegral coeff
                             else putWord16be $ fromIntegral coeff

    get = do
        (precision, dest) <- get4BitOfEach
        coeffs <- replicateM 64 $ if precision == 0
                then fromIntegral <$> getWord8
                else fromIntegral <$> getWord16be
        return $ JpgQuantTableSpec
            { quantPrecision = precision
            , quantDestination = dest
            , quantTable = listArray (0, 63) coeffs
            }

data JpgHuffmanTableSpec = JpgHuffmanTableSpec
    { -- | 0 : DC, 1 : AC, stored on 4 bits
      huffmanTableClass       :: !DctComponent
      -- | Stored on 4 bits
    , huffmanTableDest        :: !Word8

    , huffSizes :: !(UArray Word32 Word8)
    , huffCodes :: !(Array Word32 (UArray Int Word8))
    }
    deriving Show

buildPackedHuffmanTree :: Array Word32 (UArray Int Word8) -> HuffmanTree
buildPackedHuffmanTree = buildHuffmanTree . map elems . elems

-- | Decode a list of huffman values, not optimized for speed, but it
-- should work.
huffmanDecode :: HuffmanTree -> BoolReader s Word8
huffmanDecode originalTree = S.get >>= huffDecode originalTree
  where huffDecode _       [] = return 0
                        -- fail "huffmanDecode - No more bits (shouldn't happen)"
        huffDecode Empty rest = S.put rest >> return 0
                        -- fail "huffmanDecode - Empty leaf (shouldn't happen)"
        huffDecode (Branch l _) ((_, False) : rest) = huffDecode l rest
        huffDecode (Branch _ r) ((_, True ) : rest) = huffDecode r rest
        huffDecode (Leaf v) boolList = S.put boolList >> return v

-- |  Drop all bit until the bit of indice 0, usefull to parse restart
-- marker, as they are byte aligned, but Huffman might not.
byteAlign :: BoolReader s ()
byteAlign = S.get >>= S.put . firstBit
    where firstBit bits = snd $ break (\(bitIdx,_) -> bitIdx == 0) bits

-- | Convert a bytestring to a list of word8, removing restart
-- markers.
{-# INLINE markerRemoval #-}
markerRemoval :: B.ByteString -> [Word8]
markerRemoval = markerRemover . B.unpack
  where markerRemover (0xFF:0x00:rest) = 0xFF : markerRemover rest
        markerRemover (0xFF:   _:rest) = markerRemover rest -- restart marker
        markerRemover (x   :rest)      = x : markerRemover rest
        markerRemover []               = []

-- | Position of a bit in the initial word
type BitIndex = Word8

-- | Bitify a list of things to decode.
{-# INLINE bitifyString #-}
bitifyString :: [Word8] -> [(BitIndex, Bool)]
bitifyString = concatMap bitify
  where bitify v = [(7 - i, testBit v $ fromIntegral i) | i <- [7, 6 .. 0]]

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

instance SizeCalculable JpgHuffmanTableSpec where
    calculateSize table = 1 + 16 + sum [fromIntegral e | e <- elems $ huffSizes table]

instance Serialize JpgHuffmanTableSpec where
    put = error "Unimplemented"
    get = do
        (huffClass, huffDest) <- get4BitOfEach
        sizes <- replicateM 16 getWord8
        codes <- forM sizes $ \s -> do
            let si = fromIntegral s
            listArray (0, si - 1) <$> replicateM (fromIntegral s) getWord8
        return $ JpgHuffmanTableSpec
            { huffmanTableClass =
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

takeCurrentFrame :: Get B.ByteString
takeCurrentFrame = do
    size <- getWord16be
    getBytes (fromIntegral size - 2)

parseFrames :: Get [JpgFrame]
parseFrames = do
    kind <- get
    case kind of
        JpgAppSegment c ->
            (\frm lst -> JpgAppFrame c frm : lst) <$> takeCurrentFrame <*> parseFrames
        JpgExtensionSegment c ->
            (\frm lst -> JpgExtension c frm : lst) <$> takeCurrentFrame <*> parseFrames
        JpgQuantizationTable ->
            (\(TableList quants) lst -> JpgQuantTable quants : lst) <$> get <*> parseFrames
        JpgRestartInterval ->
            (\(RestartInterval i) lst -> JpgIntervalRestart i : lst) <$> get <*> parseFrames
        JpgHuffmanTableMarker ->
            (\(TableList huffTables) lst -> 
                    JpgHuffmanTable [(t, buildPackedHuffmanTree $ huffCodes t) | t <- huffTables] : lst) 
                    <$> get <*> parseFrames
        JpgStartOfScan ->
            (\frm imgData -> [JpgScanBlob frm imgData])
                            <$> get <*> (remaining >>= getBytes)

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

newtype RestartInterval = RestartInterval Word16

instance Serialize RestartInterval where
    put (RestartInterval i) = putWord16be 4 >> putWord16be i
    get = do
        size <- getWord16be
        when (size /= 4) (fail "Invalid jpeg restart interval size")
        RestartInterval <$> getWord16be

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
        (approxHigh, approxLow) <- get4BitOfEach
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
        put4BitsOfEach (successiveApproxHigh v) $ successiveApproxLow v

type BoolReader s a = S.StateT [(BitIndex, Bool)] (ST s) a

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

levelScaling :: (Integral a, IArray UArray a, Num a) => MacroBlock a -> MacroBlock Word8
levelScaling = amap (\c -> fromIntegral . max 0 . min 255 $ 128 + c)

macroBlockTranspose :: (IArray UArray a) => MacroBlock a -> MacroBlock a
macroBlockTranspose = ixmap (0, 63) transposer
    where transposer i = let (y,x) =  i `divMod` 8 in x * 8 + y
             

-- | Cast a macroblock from an integer one to a floating point one.
fromIntegralMacroblock :: (IArray UArray a, IArray UArray b, Integral a, Num b) 
                       => MacroBlock a -> MacroBlock b
fromIntegralMacroblock = amap fromIntegral

-- | Drop the fractional part of a macroblock to retrieve an integer
-- macroblock.
truncateMacroblock :: (IArray UArray a, IArray UArray b, RealFrac a, Integral b) 
                   => MacroBlock a -> MacroBlock b
truncateMacroblock = amap truncate

-- | Implement an R^8*8 matrix multiplication between floating points
matrixMultiplication :: MacroBlock Float -> MacroBlock Float -> MacroBlock Float
matrixMultiplication a b = makeMacroBlock [coeff i j | i <- [0 .. 7], j <- [0 .. 7] ]
    where dotProduct lst = sum $ (\(n,m) -> n * m) <$> lst
          line i = map (a !) [ i * 8 .. i * 8 + 7 ]
          column j = map (b !) [j, j + 8 .. 63]
          coeff i j = dotProduct $ zip (line i) (column j)

inverseDirectCosineTransform :: (Integral a, IArray UArray a) => MacroBlock a -> MacroBlock a
inverseDirectCosineTransform = truncateMacroblock 
                             . macroBlockTranspose
                             . matrixMultiplication idctCoefficientMatrix
                             . macroBlockTranspose
                             . matrixMultiplication idctCoefficientMatrix
                             . fromIntegralMacroblock

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

{-macroShow :: (PrintfArg a, IArray UArray a, Show a) => MacroBlock a -> String-}
{-macroShow block = unlines blockLines-}
    {-where blockLines = [line_of j | j <- [0..7]]-}
          {-cell_of :: Word32 -> Word32 -> String-}
          {-cell_of i j = printf "%8d" $ block ! (i + 8 * j)-}
          {-line_of :: Word32 -> String-}
          {-line_of j = concat $ intersperse " " [cell_of i j | i <- [0 .. 7]]-}

-- | This is one of the most important function of the decoding,
-- it form the barebone decoding pipeline for macroblock. It's all
-- there is to know for macro block transformation
decodeMacroBlock :: MacroBlock Int16 
                 -> MacroBlock DctCoefficients -> MacroBlock Word8
decodeMacroBlock quantizationTable =  promoteMacroBlock
                                 {-. (\a -> trace ("Scaled\n" ++ macroShow a ++ "\n") a) -}
                                 . levelScaling
                                 {-. (\a -> trace ("Uncosed \n" ++ macroShow a ++ "\n") a) -}
                                 . inverseDirectCosineTransform 
                                 {-. (\a -> trace ("Reorganized\n" ++ macroShow a ++ "\n") a)-}
                                 . zigZagReorder
                                 {-. (\a -> trace ("Dequantized\n" ++ macroShow a ++ "\n") a)-}
                                 {-. (\a -> trace ("QuantTable\n" ++ macroShow quantizationTable ++ "\n") a)-}
                                 . deQuantize quantizationTable
                                 {-. (\a -> trace ("Promoted\n" ++ macroShow a ++ "\n") a)-}
                                 {-. promoteMacroBlock-}

packInt :: [Bool] -> Int32
packInt = foldl' bitStep 0
    where bitStep acc True = (acc `shiftL` 1) + 1
          bitStep acc False = acc `shiftL` 1

-- | Unpack an int of the given size encoded from MSB to LSB.
unpackInt :: Int32 -> BoolReader s Int32
unpackInt bitCount = do
    bits <- S.get
    let (toUnpack, rest) = fromIntegral bitCount `splitAt` bits
    S.put rest
    return . packInt $ map snd toUnpack

decodeInt :: Int32 -> BoolReader s Int32
decodeInt ssss = do
    bits <- S.get
    let dataRange = 1 `shiftL` fromIntegral (ssss - 1)
        leftBitCount = ssss - 1
    -- First following bits store the sign of the coefficient, and counted in
    -- SSSS, so the bit count for the int, is ssss - 1
    case bits of
      []     -> fail "Not engouh bits"
      ((_, True) : rest) -> do
          S.put rest
          (\w -> dataRange + fromIntegral w) <$> unpackInt leftBitCount
      ((_, False) : rest) -> do
          S.put rest
          (\w -> 1 - dataRange * 2 + fromIntegral w) <$> unpackInt leftBitCount

dcCoefficientDecode :: HuffmanTree -> BoolReader s DcCoefficient
dcCoefficientDecode dcTree = do
    ssss <- huffmanDecode dcTree
    {- trace (printf "dc ssss %d" ssss) $  -}
    if ssss == 0
       then return 0
       else fromIntegral <$> (decodeInt $ fromIntegral ssss)

-- | Use an array of integer?
acCoefficientsDecode :: HuffmanTree -> BoolReader s [DctCoefficients]
acCoefficientsDecode acTree = concat <$> parseAcCoefficient 63
  where parseAcCoefficient 0 = return []
        parseAcCoefficient n = do
            rrrrssss <- huffmanDecode acTree
            let rrrr = fromIntegral $ (rrrrssss `shiftR` 4) .&. 0xF
                ssss =  rrrrssss .&. 0xF
            {- trace (printf "ac rrrrssss %2X" rrrrssss) $-} 
            case (rrrr, ssss) of
                (  0, 0) -> return $ [replicate n 0]
                (0xF, 0) -> (replicate 16 0 :) <$> parseAcCoefficient (n - 16)
                _        -> do
                    let zeroRunLength = replicate rrrr 0
                    decoded <- fromIntegral <$> (decodeInt $ fromIntegral ssss)
                    ((zeroRunLength ++ [decoded]) :) <$> parseAcCoefficient (n - rrrr - 1)

-- | Decompress a macroblock from a bitstream given the current configuration
-- from the frame.
decompressMacroBlock :: HuffmanTree         -- ^ Tree used for DC coefficient
                     -> HuffmanTree         -- ^ Tree used for Ac coefficient
                     -> MacroBlock Int16    -- ^ Current quantization table
                     -> DcCoefficient       -- ^ Previous dc value
                     -> BoolReader s (DcCoefficient, MacroBlock Word8)
decompressMacroBlock dcTree acTree quantizationTable previousDc = do
    dcDeltaCoefficient <- dcCoefficientDecode dcTree
    acCoefficients <- acCoefficientsDecode acTree
    let neoDcCoefficient = previousDc + dcDeltaCoefficient
        block = makeMacroBlock $ neoDcCoefficient : acCoefficients
    return (neoDcCoefficient, decodeMacroBlock quantizationTable block)

gatherQuantTables :: JpgImage -> [JpgQuantTableSpec]
gatherQuantTables img = concat [t | JpgQuantTable t <- jpgFrame img]

gatherHuffmanTables :: JpgImage -> [(JpgHuffmanTableSpec, HuffmanTree)]
gatherHuffmanTables img = concat [lst | JpgHuffmanTable lst <- jpgFrame img]

gatherScanInfo :: JpgImage -> (JpgFrameKind, JpgFrameHeader)
gatherScanInfo img = fromJust $ unScan <$> find scanDesc (jpgFrame img)
    where scanDesc (JpgScans _ _) = True
          scanDesc _ = False

          unScan (JpgScans a b) = (a,b)
          unScan _ = error "If this can happen, the JPEG image is ill-formed"

-- | Given a size coefficient (how much a pixel span horizontally
-- and vertically), the position of the macroblock, return a list
-- of indices and value to be stored in an array (like the final
-- image)
unpackMacroBlock :: (IArray UArray a)
                 => Word32 -- ^ Width coefficient
                 -> Word32 -- ^ Height coefficient
                 -> Word32 -- ^ x
                 -> Word32 -- ^ y
                 -> MacroBlock a
                 -> [((Word32, Word32), a)]
    -- Simple case, a macroblock value => a pixel
unpackMacroBlock      1      1 x y block =
    [((i + x * 8, j + y * 8), block ! (i + j * 8))
                                | i <- [0 .. 7], j <- [0 .. 7] ]

    -- here we have to span
unpackMacroBlock wCoeff hCoeff x y block =
    [(((i + x * 8) * wCoeff + wDup,
       (j + y * 8) * hCoeff + hDup), block ! (i + j * 8))
                    | i <- [0 .. 7], j <- [0 .. 7]
                    -- Repetition to spread macro block
                    , wDup <- [0 .. wCoeff - 1]
                    , hDup <- [0 .. hCoeff - 1]
                    ]

-- | Type only used to make clear what kind of integer we are carrying
-- Might be transformed into newtype in the future
type DcCoefficient = Int32

-- | Same as for DcCoefficient, to provide nicer type signatures
type DctCoefficients = DcCoefficient 

mapAccumLM :: Monad m => (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
mapAccumLM _ acc []     = return (acc, [])
mapAccumLM f acc (x:xs) = do
    (acc', y)      <- f acc x
    (acc'', yList) <- mapAccumLM f acc' xs
    return (acc'', y : yList)

decodeRestartInterval :: BoolReader s Int32
decodeRestartInterval = S.get >>= \bits ->
    case bits of
        ( (_,True) : (_,True) : (_,True) : (_,True)
            : (_,True) : (_,True) : (_,True) : (_,True) : markerString) ->
            let (marker, rest) = splitAt 8 markerString
                intBits = map snd marker
            in S.put rest >> return (packInt intBits)
        _ -> return (-1)


decodeImage :: Int              -- ^ Component count
            -> JpegDecoder s
            -> BoolReader s [((Word32, Word32), (Int, Word8))]
decodeImage compCount decoder = concat <$> do
    dcArray <- lift $ (newArray (0, compCount - 1) 0  :: ST s (STUArray s Int Int32))
    let blockIndices = [(x,y) | y <- [0 ..   verticalMcuCount decoder - 1]
                              , x <- [0 .. horizontalMcuCount decoder - 1] ]
        mcuDecode = mcuDecoder decoder
        blockBeforeRestart = restartInterval decoder

        mapAccumM f = mapAccumLM f blockBeforeRestart mcuDecode >>= \(_, lst) -> return lst

    concat <$> forM blockIndices (\(x,y) ->
        mapAccumM $ \resetCounter (comp, dataUnitDecoder) -> do
            when (resetCounter == 0)
                 (do forM_ [0.. compCount - 1] $ 
                           \c -> lift $ (dcArray `writeArray` c) 0
                     byteAlign
                     restartCode <- decodeRestartInterval
                     if 0xD0 <= restartCode && restartCode <= 0xD7
                     	then return ()
                     	else trace ("Wrong error marker") $ return ())

            dc <- lift $ dcArray `readArray` comp
            (dcCoeff, block) <- dataUnitDecoder x y $ fromIntegral dc
            lift $ (dcArray `writeArray` comp) dcCoeff
            return (resetCounter - 1, [(idx, (comp, val)) | (idx, val) <- block]))

-- | Type used to write into an array
type PixelWriteOrders   = [((Word32, Word32), Word8)]

-- | Type of a data unit (as in the ITU 81) standard
type DataUnitDecoder s  =
    (Int, Word32 -> Word32 -> DcCoefficient -> BoolReader s (DcCoefficient, PixelWriteOrders))

data JpegDecoder s = JpegDecoder
    { restartInterval    :: Int
    , horizontalMcuCount :: Word32
    , verticalMcuCount   :: Word32
    , mcuDecoder         :: [DataUnitDecoder s]
    }

allElementsEqual :: (Eq a) => [a] -> Bool
allElementsEqual []     = True
allElementsEqual (x:xs) = all (== x) xs

-- | An MCU (Minimal coded unit) is an unit of data for all components
-- (Y, Cb & Cr), taking into account downsampling.
buildJpegImageDecoder :: JpgImage -> JpegDecoder s
buildJpegImageDecoder img = JpegDecoder { restartInterval = mcuBeforeRestart
                                        , horizontalMcuCount = horizontalBlockCount
                                        , verticalMcuCount = verticalBlockCount
                                        , mcuDecoder = mcus }
  where huffmans = gatherHuffmanTables img
        huffmanForComponent dcOrAc dest = 
            head [t | (h,t) <- huffmans, huffmanTableClass h == dcOrAc
                                       , huffmanTableDest h == dest]

        mcuBeforeRestart = case [i | JpgIntervalRestart i <- jpgFrame img] of
            []    -> maxBound -- HUUUUUUGE value (enough to parse all MCU)
            (x:_) -> fromIntegral x

        quants = gatherQuantTables img
        quantForComponent dest =
            head [quantTable q | q <- quants, quantDestination q == dest]

        hdr = head [h | JpgScanBlob h _ <- jpgFrame img]   

        (_, scanInfo) = gatherScanInfo img
        imgWidth = fromIntegral $ jpgWidth scanInfo
        imgHeight = fromIntegral $ jpgHeight scanInfo

        blockSizeOfDim fullDim maxBlockSize = block + (if rest /= 0 then 1 else 0)
                where (block, rest) = fullDim `divMod` maxBlockSize

        horizontalSamplings = [horiz | (horiz, _, _, _, _) <- componentsInfo]

        isImageLumanOnly = jpgImageComponentCount scanInfo == 1
        maxHorizFactor | not isImageLumanOnly && 
                            not (allElementsEqual horizontalSamplings) = maximum horizontalSamplings 
                       | otherwise = 1

        verticalSamplings = [vert | (_, vert, _, _, _) <- componentsInfo]
        maxVertFactor | not isImageLumanOnly &&
                            not (allElementsEqual verticalSamplings) = maximum verticalSamplings 
                      | otherwise = 1

        horizontalBlockCount = (\t -> trace (printf "horizontalBlockCount:%d maxHorizFactor:%d " t maxHorizFactor) t) $
           blockSizeOfDim imgWidth $ fromIntegral (maxHorizFactor * 8)

        verticalBlockCount = (\t -> trace (printf "verticalBlockCount:%d maxVert:%d\n" t maxVertFactor) t) $
           blockSizeOfDim imgHeight $ fromIntegral (maxVertFactor * 8)

        fetchTablesForComponent component = (horizCount, vertCount, dcTree, acTree, qTable)
            where idx = componentIdentifier component
                  descr = head [c | c <- scans hdr, componentSelector c  == idx]
                  dcTree = huffmanForComponent DcComponent $ dcEntropyCodingTable descr
                  acTree = huffmanForComponent AcComponent $ acEntropyCodingTable descr
                  qTable = quantForComponent $ if idx == 1 then 0 else 1
                  horizCount = if not isImageLumanOnly
                        then fromIntegral $ horizontalSamplingFactor component
                        else 1
                  vertCount = if not isImageLumanOnly
                        then fromIntegral $ verticalSamplingFactor component
                        else 1

        componentsInfo = map fetchTablesForComponent $ jpgComponents scanInfo

        mcus = [trace (printf "component:%d x:%d y:%d xScale:%d yScale:%d horizCount:%d vertCount:%d" compIdx xd yd xScalingFactor yScalingFactor horizCount vertCount) $
                (compIdx, \x y dc -> do
                           (dcCoeff, block) <- decompressMacroBlock dcTree acTree qTable dc
                           return (dcCoeff, unpacker  (x * horizCount + xd) (y * vertCount + yd) block))
                     | (compIdx, (horizCount, vertCount, dcTree, acTree, qTable)) 
                                   <- zip [0..] componentsInfo
                     , let xScalingFactor = maxHorizFactor - horizCount + 1
                           yScalingFactor = maxVertFactor - vertCount + 1
                     , yd <- [0 .. vertCount - 1]
                     , xd <- [0 .. horizCount - 1]
                     , let unpacker = unpackMacroBlock xScalingFactor yScalingFactor
                     ]

loadJpeg :: FilePath -> IO (Either String (Image PixelYCbCr8))
loadJpeg f = decodeJpeg <$> B.readFile f

decodeJpeg :: B.ByteString -> Either String (Image PixelYCbCr8)
decodeJpeg file = case decode file of
  Left err -> Left err
  Right img -> Right $
      let (imgData:_) = [d | JpgScanBlob _kind d <- jpgFrame img]
          bitList = bitifyString $ markerRemoval imgData
          (_, scanInfo) = gatherScanInfo img
          compCount = length $ jpgComponents scanInfo

          {-decoder :: BoolReader s [((Word32, Word32), (Int, Word8))]-}
          decoder = decodeImage compCount $ buildJpegImageDecoder img

          imgWidth = fromIntegral $ jpgWidth scanInfo
          imgHeight = fromIntegral $ jpgHeight scanInfo

          imageSize = ((0, 0), (imgWidth - 1, imgHeight - 1))
          setter (PixelYCbCr8 _ cb cr) (0, v) = PixelYCbCr8 v cb cr
          setter (PixelYCbCr8 y  _ cr) (1, v) = PixelYCbCr8 y  v cr
          setter (PixelYCbCr8 y cb  _) (2, v) = PixelYCbCr8 y cb  v
          setter _ _ = error "Impossible jpeg decoding can happen"

          pixelList =  runST $ S.evalStateT decoder bitList

          inImageBound ((x, y), _) = x < imgWidth && y < imgHeight

      in accumArray setter (PixelYCbCr8 0 128 128) imageSize $ filter inImageBound pixelList

jpegTest :: FilePath -> IO ()
jpegTest path = do
    file <- B.readFile path
    case decode file of
         Left err -> print err
         Right img -> mapM_ (\a -> putStrLn (groom a) >> putStrLn "\n\n") $ jpgFrame img

