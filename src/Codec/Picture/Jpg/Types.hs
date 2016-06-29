{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
module Codec.Picture.Jpg.Types( MutableMacroBlock
                              , createEmptyMutableMacroBlock
                              , printMacroBlock
                              , printPureMacroBlock
                              , DcCoefficient
                              , JpgImage( .. )
                              , JpgComponent( .. )
                              , JpgFrameHeader( .. )
                              , JpgFrame( .. )
                              , JpgFrameKind( .. )
                              , JpgScanHeader( .. )
                              , JpgQuantTableSpec( .. )
                              , JpgHuffmanTableSpec( .. )
                              , JpgImageKind( .. )
                              , JpgScanSpecification( .. )
                              , JpgColorSpace( .. )
                              , AdobeTransform( .. )
                              , JpgAdobeApp14( .. )
                              , JpgJFIFApp0( .. )
                              , JFifUnit( .. )
                              , calculateSize
                              , dctBlockSize
                              ) where


#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( pure, (<*>), (<$>) )
#endif

import Control.Monad( when, replicateM, forM, forM_, unless )
import Control.Monad.ST( ST )
import Data.Bits( (.|.), (.&.), unsafeShiftL, unsafeShiftR )
import Foreign.Storable ( Storable )
import Data.Vector.Unboxed( (!) )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L

import Data.Int( Int16 )
import Data.Word(Word8, Word16 )
import Data.Binary( Binary(..) )

import Data.Binary.Get( Get
                      , getWord8
                      , getWord16be
                      , getByteString
                      , skip
                      , bytesRead
                      )

import Data.Binary.Put( Put
                      , putWord8
                      , putWord16be
                      , putLazyByteString
                      , putByteString
                      )

import Codec.Picture.InternalHelper
import Codec.Picture.Jpg.DefaultTable
import Codec.Picture.Tiff.Types

{-import Debug.Trace-}
import Text.Printf

-- | Type only used to make clear what kind of integer we are carrying
-- Might be transformed into newtype in the future
type DcCoefficient = Int16

-- | Macroblock that can be transformed.
type MutableMacroBlock s a = M.STVector s a

data JpgFrameKind =
      JpgBaselineDCTHuffman
    | JpgExtendedSequentialDCTHuffman
    | JpgProgressiveDCTHuffman
    | JpgLosslessHuffman
    | JpgDifferentialSequentialDCTHuffman
    | JpgDifferentialProgressiveDCTHuffman
    | JpgDifferentialLosslessHuffman
    | JpgExtendedSequentialArithmetic
    | JpgProgressiveDCTArithmetic
    | JpgLosslessArithmetic
    | JpgDifferentialSequentialDCTArithmetic
    | JpgDifferentialProgressiveDCTArithmetic
    | JpgDifferentialLosslessArithmetic
    | JpgQuantizationTable
    | JpgHuffmanTableMarker
    | JpgStartOfScan
    | JpgEndOfImage
    | JpgAppSegment Word8
    | JpgExtensionSegment Word8

    | JpgRestartInterval
    | JpgRestartIntervalEnd Word8
    deriving (Eq, Show)

data JpgFrame =
      JpgAppFrame        !Word8 B.ByteString
    | JpgAdobeAPP14      !JpgAdobeApp14
    | JpgJFIF            !JpgJFIFApp0
    | JpgExif            ![ImageFileDirectory]
    | JpgExtension       !Word8 B.ByteString
    | JpgQuantTable      ![JpgQuantTableSpec]
    | JpgHuffmanTable    ![(JpgHuffmanTableSpec, HuffmanPackedTree)]
    | JpgScanBlob        !JpgScanHeader !L.ByteString
    | JpgScans           !JpgFrameKind !JpgFrameHeader
    | JpgIntervalRestart !Word16
    deriving Show

data JpgColorSpace
  = JpgColorSpaceYCbCr
  | JpgColorSpaceYCC
  | JpgColorSpaceY
  | JpgColorSpaceYA
  | JpgColorSpaceYCCA
  | JpgColorSpaceYCCK
  | JpgColorSpaceCMYK
  | JpgColorSpaceRGB
  | JpgColorSpaceRGBA
  deriving Show

data AdobeTransform
  = AdobeUnknown    -- ^ Value 0
  | AdobeYCbCr      -- ^ value 1
  | AdobeYCck       -- ^ value 2
  deriving Show

data JpgAdobeApp14 = JpgAdobeApp14
  { _adobeDctVersion :: !Word16
  , _adobeFlag0      :: !Word16
  , _adobeFlag1      :: !Word16
  , _adobeTransform  :: !AdobeTransform
  }
  deriving Show

-- | Size: 1
data JFifUnit
  = JFifUnitUnknown   -- ^ 0
  | JFifPixelsPerInch -- ^ 1
  | JFifPixelsPerCentimeter -- ^ 2
  deriving Show

instance Binary JFifUnit where
  put v = putWord8 $ case v of
    JFifUnitUnknown -> 0
    JFifPixelsPerInch -> 1
    JFifPixelsPerCentimeter -> 2
  get = do
    v <- getWord8
    pure $ case v of
      0 -> JFifUnitUnknown
      1 -> JFifPixelsPerInch
      2 -> JFifPixelsPerCentimeter
      _ -> JFifUnitUnknown

data JpgJFIFApp0 = JpgJFIFApp0
  { _jfifUnit      :: !JFifUnit
  , _jfifDpiX      :: !Word16
  , _jfifDpiY      :: !Word16
  , _jfifThumbnail :: !(Maybe {- (Image PixelRGB8) -} Int)
  }
  deriving Show

instance Binary JpgJFIFApp0 where
  get = do
    sig <- getByteString 5
    when (sig /= BC.pack "JFIF\0") $
        fail "Invalid JFIF signature"
    major <- getWord8
    minor <- getWord8
    when (major /= 1 && minor > 2) $
        fail "Unrecognize JFIF version"
    unit <- get
    dpiX <- getWord16be
    dpiY <- getWord16be
    w <- getWord8
    h <- getWord8
    let pxCount = 3 * w * h
    img <- case pxCount of
      0 -> return Nothing
      _ -> return Nothing
    return $ JpgJFIFApp0
        { _jfifUnit      = unit
        , _jfifDpiX      = dpiX
        , _jfifDpiY      = dpiY
        , _jfifThumbnail = img
        }


  put jfif = do
    putByteString $ BC.pack "JFIF\0" -- 5
    putWord8 1                       -- 1 6
    putWord8 2                       -- 1 7
    put $ _jfifUnit jfif             -- 1 8
    putWord16be $ _jfifDpiX jfif     -- 2 10
    putWord16be $ _jfifDpiY jfif     -- 2 12
    putWord8 0                       -- 1 13
    putWord8 0                       -- 1 14

{-Thumbnail width (tw) 	1 	Horizontal size of embedded JFIF thumbnail in pixels-}
{-Thumbnail height (th) 	1 	Vertical size of embedded JFIF thumbnail in pixels-}
{-Thumbnail data 	3 × tw × th 	Uncompressed 24 bit RGB raster thumbnail-}

instance Binary AdobeTransform where
  put v = case v of
    AdobeUnknown -> putWord8 0
    AdobeYCbCr -> putWord8 1
    AdobeYCck -> putWord8 2

  get = do
    v <- getWord8
    pure $ case v of
      0 -> AdobeUnknown
      1 -> AdobeYCbCr
      2 -> AdobeYCck
      _ -> AdobeUnknown

instance Binary JpgAdobeApp14 where
  get = do
    let sig = BC.pack "Adobe"
    fileSig <- getByteString 5
    when (fileSig /= sig) $
       fail "Invalid Adobe APP14 marker"
    version <- getWord16be
    when (version /= 100) $
       fail $ "Invalid Adobe APP14 version " ++ show version
    JpgAdobeApp14 version
                  <$> getWord16be
                  <*> getWord16be <*> get

  put (JpgAdobeApp14 v f0 f1 t) = do
    putByteString $ BC.pack "Adobe"
    putWord16be v
    putWord16be f0
    putWord16be f1
    put t


data JpgFrameHeader = JpgFrameHeader
    { jpgFrameHeaderLength   :: !Word16
    , jpgSamplePrecision     :: !Word8
    , jpgHeight              :: !Word16
    , jpgWidth               :: !Word16
    , jpgImageComponentCount :: !Word8
    , jpgComponents          :: ![JpgComponent]
    }
    deriving Show


instance SizeCalculable JpgFrameHeader where
    calculateSize hdr = 2 + 1 + 2 + 2 + 1
                      + sum [calculateSize c | c <- jpgComponents hdr]

data JpgComponent = JpgComponent
    { componentIdentifier       :: !Word8
      -- | Stored with 4 bits
    , horizontalSamplingFactor  :: !Word8
      -- | Stored with 4 bits
    , verticalSamplingFactor    :: !Word8
    , quantizationTableDest     :: !Word8
    }
    deriving Show

instance SizeCalculable JpgComponent where
    calculateSize _ = 3

data JpgImage = JpgImage { jpgFrame :: [JpgFrame] }
    deriving Show

data JpgScanSpecification = JpgScanSpecification
    { componentSelector :: !Word8
      -- | Encoded as 4 bits
    , dcEntropyCodingTable :: !Word8
      -- | Encoded as 4 bits
    , acEntropyCodingTable :: !Word8

    }
    deriving Show

instance SizeCalculable JpgScanSpecification where
    calculateSize _ = 2

data JpgScanHeader = JpgScanHeader
    { scanLength :: !Word16
    , scanComponentCount :: !Word8
    , scans :: [JpgScanSpecification]

      -- | (begin, end)
    , spectralSelection    :: (Word8, Word8)

      -- | Encoded as 4 bits
    , successiveApproxHigh :: !Word8

      -- | Encoded as 4 bits
    , successiveApproxLow :: !Word8
    }
    deriving Show

instance SizeCalculable JpgScanHeader where
    calculateSize hdr = 2 + 1
                      + sum [calculateSize c | c <- scans hdr]
                      + 2
                      + 1

data JpgQuantTableSpec = JpgQuantTableSpec
    { -- | Stored on 4 bits
      quantPrecision     :: !Word8

      -- | Stored on 4 bits
    , quantDestination   :: !Word8

    , quantTable         :: MacroBlock Int16
    }
    deriving Show

class SizeCalculable a where
    calculateSize :: a -> Int

-- | Type introduced only to avoid some typeclass overlapping
-- problem
newtype TableList a = TableList [a]

instance (SizeCalculable a, Binary a) => Binary (TableList a) where
    put (TableList lst) = do
        putWord16be . fromIntegral $ sum [calculateSize table | table <- lst] + 2
        mapM_ put lst

    get = TableList <$> (getWord16be >>= \s -> innerParse (fromIntegral s - 2))
      where innerParse :: Int -> Get [a]
            innerParse 0    = return []
            innerParse size = do
                onStart <- fromIntegral <$> bytesRead
                table <- get
                onEnd <- fromIntegral <$> bytesRead
                (table :) <$> innerParse (size - (onEnd - onStart))

instance SizeCalculable JpgQuantTableSpec where
    calculateSize table =
        1 + (fromIntegral (quantPrecision table) + 1) * 64

instance Binary JpgQuantTableSpec where
    put table = do
        let precision = quantPrecision table
        put4BitsOfEach precision (quantDestination table)
        forM_ (VS.toList $ quantTable table) $ \coeff ->
            if precision == 0 then putWord8 $ fromIntegral coeff
                             else putWord16be $ fromIntegral coeff

    get = do
        (precision, dest) <- get4BitOfEach
        coeffs <- replicateM 64 $ if precision == 0
                then fromIntegral <$> getWord8
                else fromIntegral <$> getWord16be
        return JpgQuantTableSpec
            { quantPrecision = precision
            , quantDestination = dest
            , quantTable = VS.fromListN 64 coeffs
            }

data JpgHuffmanTableSpec = JpgHuffmanTableSpec
    { -- | 0 : DC, 1 : AC, stored on 4 bits
      huffmanTableClass       :: !DctComponent
      -- | Stored on 4 bits
    , huffmanTableDest        :: !Word8

    , huffSizes :: !(VU.Vector Word8)
    , huffCodes :: !(V.Vector (VU.Vector Word8))
    }
    deriving Show

instance SizeCalculable JpgHuffmanTableSpec where
    calculateSize table = 1 + 16 + sum [fromIntegral e | e <- VU.toList $ huffSizes table]

instance Binary JpgHuffmanTableSpec where
    put table = do
        let classVal = if huffmanTableClass table == DcComponent
                          then 0 else 1
        put4BitsOfEach classVal $ huffmanTableDest table
        mapM_ put . VU.toList $ huffSizes table
        forM_ [0 .. 15] $ \i ->
            when (huffSizes table ! i /= 0)
                 (let elements = VU.toList $ huffCodes table V.! i
                  in mapM_ put elements)

    get = do
        (huffClass, huffDest) <- get4BitOfEach
        sizes <- replicateM 16 getWord8
        codes <- forM sizes $ \s ->
            VU.replicateM (fromIntegral s) getWord8
        return JpgHuffmanTableSpec
            { huffmanTableClass =
                if huffClass == 0 then DcComponent else AcComponent
            , huffmanTableDest = huffDest
            , huffSizes = VU.fromListN 16 sizes
            , huffCodes = V.fromListN 16 codes
            }

instance Binary JpgImage where
    put (JpgImage { jpgFrame = frames }) =
        putWord8 0xFF >> putWord8 0xD8 >> mapM_ putFrame frames
            >> putWord8 0xFF >> putWord8 0xD9

    get = do
        let startOfImageMarker = 0xD8
            -- endOfImageMarker = 0xD9
        checkMarker commonMarkerFirstByte startOfImageMarker
        eatUntilCode
        frames <- parseFrames
        {-checkMarker commonMarkerFirstByte endOfImageMarker-}
        return JpgImage { jpgFrame = frames }

eatUntilCode :: Get ()
eatUntilCode = do
    code <- getWord8
    unless (code == 0xFF) eatUntilCode

takeCurrentFrame :: Get B.ByteString
takeCurrentFrame = do
    size <- getWord16be
    getByteString (fromIntegral size - 2)

putFrame :: JpgFrame -> Put
putFrame (JpgAdobeAPP14 adobe) = 
    put (JpgAppSegment 14) >> putWord16be 14 >> put adobe
putFrame (JpgJFIF jfif) =
    put (JpgAppSegment 0) >> putWord16be (14+2) >> put jfif
putFrame (JpgExif _exif) =
    return () -- TODO
    {-put (JpgAppSegment 0) >> put exif-}
putFrame (JpgAppFrame appCode str) =
    put (JpgAppSegment appCode) >> putWord16be (fromIntegral $ B.length str) >> put str
putFrame (JpgExtension appCode str) =
    put (JpgExtensionSegment appCode) >> putWord16be (fromIntegral $ B.length str) >> put str
putFrame (JpgQuantTable tables) =
    put JpgQuantizationTable >> put (TableList tables)
putFrame (JpgHuffmanTable tables) =
    put JpgHuffmanTableMarker >> put (TableList $ map fst tables)
putFrame (JpgIntervalRestart size) =
    put JpgRestartInterval >> put (RestartInterval size)
putFrame (JpgScanBlob hdr blob) = do
    put JpgStartOfScan
    put hdr
    putLazyByteString blob
    putWord8 0 -- AKA the libjpeg pleaser, for some unknown reason
               -- libjpeg raise a warning "invalid end of data segment".
               -- so dumbly pad, seems to make the warning go away (and
               -- I don't want to invest that much time looking for that).
               --
               -- Ok this is a crummy fix....
putFrame (JpgScans kind hdr) =
    put kind >> put hdr

--------------------------------------------------
----            Serialization instances
--------------------------------------------------
commonMarkerFirstByte :: Word8
commonMarkerFirstByte = 0xFF

checkMarker :: Word8 -> Word8 -> Get ()
checkMarker b1 b2 = do
    rb1 <- getWord8
    rb2 <- getWord8
    when (rb1 /= b1 || rb2 /= b2)
         (fail "Invalid marker used")

extractScanContent :: L.ByteString -> (L.ByteString, L.ByteString)
extractScanContent str = aux 0
  where maxi = fromIntegral $ L.length str - 1

        aux n | n >= maxi = (str, L.empty)
              | v == 0xFF && vNext /= 0 && not isReset = L.splitAt n str
              | otherwise = aux (n + 1)
            where v = str `L.index` n
                  vNext = str `L.index` (n + 1)
                  isReset = 0xD0 <= vNext && vNext <= 0xD7

parseAdobe14 :: B.ByteString -> [JpgFrame] -> [JpgFrame]
parseAdobe14 str lst = go where
  go = case runGetStrict get str of
    Left _err -> lst
    Right app14 -> JpgAdobeAPP14 app14 : lst

-- | Parse JFIF or JFXX information. Right now only JFIF.
parseJF__  :: B.ByteString -> [JpgFrame] -> [JpgFrame]
parseJF__  str lst = go where
  go = case runGetStrict get str of
    Left _err -> lst
    Right jfif -> JpgJFIF jfif : lst

parseExif :: B.ByteString -> [JpgFrame] -> [JpgFrame]
parseExif str lst 
  | exifHeader `B.isPrefixOf` str = go
  | otherwise = lst
  where
    exifHeader = BC.pack "Exif\0\0"
    tiff = B.drop (B.length exifHeader) str
    go = case runGetStrict (getP tiff) tiff of
      Left _err -> lst
      Right (_hdr :: TiffHeader, ifds) -> JpgExif ifds : lst

parseFrames :: Get [JpgFrame]
parseFrames = do
    kind <- get
    let parseNextFrame = do
            word <- getWord8
            when (word /= 0xFF) $ do
                readedData <- bytesRead
                fail $ "Invalid Frame marker (" ++ show word
                     ++ ", bytes read : " ++ show readedData ++ ")"
            parseFrames

    case kind of
        JpgEndOfImage -> return []
        JpgAppSegment 0 ->
            parseJF__ <$> takeCurrentFrame <*> parseNextFrame
        JpgAppSegment 1 ->
            parseExif <$> takeCurrentFrame <*> parseNextFrame
        JpgAppSegment 14 ->
            parseAdobe14 <$> takeCurrentFrame <*> parseNextFrame
        JpgAppSegment c ->
            (\frm lst -> JpgAppFrame c frm : lst) <$> takeCurrentFrame <*> parseNextFrame
        JpgExtensionSegment c ->
            (\frm lst -> JpgExtension c frm : lst) <$> takeCurrentFrame <*> parseNextFrame
        JpgQuantizationTable ->
            (\(TableList quants) lst -> JpgQuantTable quants : lst) <$> get <*> parseNextFrame
        JpgRestartInterval ->
            (\(RestartInterval i) lst -> JpgIntervalRestart i : lst) <$> get <*> parseNextFrame
        JpgHuffmanTableMarker ->
            (\(TableList huffTables) lst ->
                    JpgHuffmanTable [(t, packHuffmanTree . buildPackedHuffmanTree $ huffCodes t) | t <- huffTables] : lst)
                    <$> get <*> parseNextFrame
        JpgStartOfScan ->
            (\frm imgData ->
                let (d, other) = extractScanContent imgData
                in
                case runGet parseFrames (L.drop 1 other) of
                  Left _ -> [JpgScanBlob frm d]
                  Right lst -> JpgScanBlob frm d : lst
            ) <$> get <*> getRemainingLazyBytes

        _ -> (\hdr lst -> JpgScans kind hdr : lst) <$> get <*> parseNextFrame

buildPackedHuffmanTree :: V.Vector (VU.Vector Word8) -> HuffmanTree
buildPackedHuffmanTree = buildHuffmanTree . map VU.toList . V.toList

secondStartOfFrameByteOfKind :: JpgFrameKind -> Word8
secondStartOfFrameByteOfKind = aux
  where
    aux JpgBaselineDCTHuffman = 0xC0
    aux JpgExtendedSequentialDCTHuffman = 0xC1
    aux JpgProgressiveDCTHuffman = 0xC2
    aux JpgLosslessHuffman = 0xC3
    aux JpgDifferentialSequentialDCTHuffman = 0xC5
    aux JpgDifferentialProgressiveDCTHuffman = 0xC6
    aux JpgDifferentialLosslessHuffman = 0xC7
    aux JpgExtendedSequentialArithmetic = 0xC9
    aux JpgProgressiveDCTArithmetic = 0xCA
    aux JpgLosslessArithmetic = 0xCB
    aux JpgHuffmanTableMarker = 0xC4
    aux JpgDifferentialSequentialDCTArithmetic = 0xCD
    aux JpgDifferentialProgressiveDCTArithmetic = 0xCE
    aux JpgDifferentialLosslessArithmetic = 0xCF
    aux JpgEndOfImage = 0xD9
    aux JpgQuantizationTable = 0xDB
    aux JpgStartOfScan = 0xDA
    aux JpgRestartInterval = 0xDD
    aux (JpgRestartIntervalEnd v) = v
    aux (JpgAppSegment a) = (a + 0xE0)
    aux (JpgExtensionSegment a) = a

data JpgImageKind = BaseLineDCT | ProgressiveDCT

instance Binary JpgFrameKind where
    put v = putWord8 0xFF >> put (secondStartOfFrameByteOfKind v)
    get = do
        -- no lookahead :(
        {-word <- getWord8-}
        word2 <- getWord8
        return $ case word2 of
            0xC0 -> JpgBaselineDCTHuffman
            0xC1 -> JpgExtendedSequentialDCTHuffman
            0xC2 -> JpgProgressiveDCTHuffman
            0xC3 -> JpgLosslessHuffman
            0xC4 -> JpgHuffmanTableMarker
            0xC5 -> JpgDifferentialSequentialDCTHuffman
            0xC6 -> JpgDifferentialProgressiveDCTHuffman
            0xC7 -> JpgDifferentialLosslessHuffman
            0xC9 -> JpgExtendedSequentialArithmetic
            0xCA -> JpgProgressiveDCTArithmetic
            0xCB -> JpgLosslessArithmetic
            0xCD -> JpgDifferentialSequentialDCTArithmetic
            0xCE -> JpgDifferentialProgressiveDCTArithmetic
            0xCF -> JpgDifferentialLosslessArithmetic
            0xD9 -> JpgEndOfImage
            0xDA -> JpgStartOfScan
            0xDB -> JpgQuantizationTable
            0xDD -> JpgRestartInterval
            a | a >= 0xF0 -> JpgExtensionSegment a
              | a >= 0xE0 -> JpgAppSegment (a - 0xE0)
              | a >= 0xD0 && a <= 0xD7 -> JpgRestartIntervalEnd a
              | otherwise -> error ("Invalid frame marker (" ++ show a ++ ")")

put4BitsOfEach :: Word8 -> Word8 -> Put
put4BitsOfEach a b = put $ (a `unsafeShiftL` 4) .|. b

get4BitOfEach :: Get (Word8, Word8)
get4BitOfEach = do
    val <- get
    return ((val `unsafeShiftR` 4) .&. 0xF, val .&. 0xF)

newtype RestartInterval = RestartInterval Word16

instance Binary RestartInterval where
    put (RestartInterval i) = putWord16be 4 >> putWord16be i
    get = do
        size <- getWord16be
        when (size /= 4) (fail "Invalid jpeg restart interval size")
        RestartInterval <$> getWord16be

instance Binary JpgComponent where
    get = do
        ident <- getWord8
        (horiz, vert) <- get4BitOfEach
        quantTableIndex <- getWord8
        return JpgComponent
            { componentIdentifier = ident
            , horizontalSamplingFactor = horiz
            , verticalSamplingFactor = vert
            , quantizationTableDest = quantTableIndex
            }
    put v = do
        put $ componentIdentifier v
        put4BitsOfEach (horizontalSamplingFactor v) $ verticalSamplingFactor v
        put $ quantizationTableDest v

instance Binary JpgFrameHeader where
    get = do
        beginOffset <- fromIntegral <$> bytesRead
        frmHLength <- getWord16be
        samplePrec <- getWord8
        h <- getWord16be
        w <- getWord16be
        compCount <- getWord8
        components <- replicateM (fromIntegral compCount) get
        endOffset <- fromIntegral <$> bytesRead
        when (beginOffset - endOffset < fromIntegral frmHLength)
             (skip $ fromIntegral frmHLength - (endOffset - beginOffset))
        return JpgFrameHeader
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

instance Binary JpgScanSpecification where
    put v = do
        put $ componentSelector v
        put4BitsOfEach (dcEntropyCodingTable v) $ acEntropyCodingTable v

    get = do
        compSel <- get
        (dc, ac) <- get4BitOfEach
        return JpgScanSpecification {
            componentSelector = compSel
          , dcEntropyCodingTable = dc
          , acEntropyCodingTable = ac
          }

instance Binary JpgScanHeader where
    get = do
        thisScanLength <- getWord16be
        compCount <- getWord8
        comp <- replicateM (fromIntegral compCount) get
        specBeg <- get
        specEnd <- get
        (approxHigh, approxLow) <- get4BitOfEach

        return JpgScanHeader {
            scanLength = thisScanLength,
            scanComponentCount = compCount,
            scans = comp,
            spectralSelection = (specBeg, specEnd),
            successiveApproxHigh = approxHigh,
            successiveApproxLow = approxLow
        }

    put v = do
        putWord16be $ scanLength v
        putWord8 $ scanComponentCount v
        mapM_ put $ scans v
        putWord8 . fst $ spectralSelection v
        putWord8 . snd $ spectralSelection v
        put4BitsOfEach (successiveApproxHigh v) $ successiveApproxLow v

{-# INLINE createEmptyMutableMacroBlock #-}
-- | Create a new macroblock with the good array size
createEmptyMutableMacroBlock :: (Storable a, Num a) => ST s (MutableMacroBlock s a)
createEmptyMutableMacroBlock = M.replicate 64 0

printMacroBlock :: (Storable a, PrintfArg a)
                => MutableMacroBlock s a -> ST s String
printMacroBlock block = pLn 0
    where pLn 64 = return "===============================\n"
          pLn i = do
              v <- block `M.unsafeRead` i
              vn <- pLn (i+1)
              return $ printf (if i `mod` 8 == 0 then "\n%5d " else "%5d ") v ++ vn

printPureMacroBlock :: (Storable a, PrintfArg a) => MacroBlock a -> String
printPureMacroBlock block = pLn 0
    where pLn 64 = "===============================\n"
          pLn i = str ++ pLn (i + 1)
            where str | i `mod` 8 == 0 = printf "\n%5d " v
                      | otherwise = printf "%5d" v
                  v = block VS.! i


{-# INLINE dctBlockSize #-}
dctBlockSize :: Num a => a
dctBlockSize = 8
