{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Module implementing TIFF decoding.
--
-- Supported compression schemes :
--
--   * Uncompressed
--
--   * PackBits
--
--   * LZW
--
-- Supported bit depth :
--
--   * 2 bits
--
--   * 4 bits
--
--   * 8 bits
--
--   * 16 bits
--
module Codec.Picture.Tiff( decodeTiff, TiffSaveable, encodeTiff, writeTiff ) where

import Control.Applicative( (<$>), (<*>), pure )
import Control.Monad( when, replicateM, foldM_ )
import Control.Monad.ST( ST, runST )
import Control.Monad.Writer.Strict( execWriter, tell, Writer )
import Data.Int( Int8 )
import Data.Word( Word8, Word16 )
import Data.Bits( (.&.), (.|.), unsafeShiftL, unsafeShiftR )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( Get
                      , getWord16le, getWord16be
                      , getWord32le, getWord32be
                      , bytesRead
                      , skip
                      , getByteString
                      )
import Data.Binary.Put( Put, runPut
                      , putWord16le, putWord16be
                      , putWord32le, putWord32be
                      , putByteString
                      )

import Data.List( sortBy, mapAccumL )
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as M
import Data.Word( Word32 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lb
import qualified Data.ByteString.Unsafe as BU

import Foreign.Storable( sizeOf )

import Codec.Picture.InternalHelper
import Codec.Picture.BitWriter
import Codec.Picture.Types
import Codec.Picture.Gif.LZW
import Codec.Picture.VectorByteConversion( toByteString )

data Endianness = EndianLittle
                | EndianBig
                deriving (Eq, Show)

instance Binary Endianness where
    put EndianLittle = putWord16le 0x4949
    put EndianBig = putWord16le 0x4D4D

    get = do
        tag <- getWord16le
        case tag of
            0x4949 -> return EndianLittle
            0x4D4D -> return EndianBig
            _ -> fail "Invalid endian tag value"

-- | Because having a polymorphic get with endianness is to nice
-- to pass on, introducing this helper type class, which is just
-- a superset of Binary, but formalising a parameter passing
-- into it.
class BinaryParam a b where
    getP :: a -> Get b
    putP :: a -> b -> Put

data TiffHeader = TiffHeader
    { hdrEndianness :: !Endianness
    , hdrOffset     :: {-# UNPACK #-} !Word32
    }
    deriving (Eq, Show)

instance BinaryParam Endianness Word16 where
    putP EndianLittle = putWord16le
    putP EndianBig = putWord16be

    getP EndianLittle = getWord16le
    getP EndianBig = getWord16be

instance BinaryParam Endianness Word32 where
    putP EndianLittle = putWord32le
    putP EndianBig = putWord32be

    getP EndianLittle = getWord32le
    getP EndianBig = getWord32be

instance Binary TiffHeader where
    put hdr = do
        let endian = hdrEndianness hdr
        put endian
        putP endian (42 :: Word16)
        putP endian $ hdrOffset hdr

    get = do
        endian <- get
        magic <- getP endian
        let magicValue = 42 :: Word16
        when (magic /= magicValue)
             (fail "Invalid TIFF magic number")
        TiffHeader endian <$> getP endian

data TiffPlanarConfiguration =
      PlanarConfigContig    -- = 1
    | PlanarConfigSeparate  -- = 2

planarConfgOfConstant :: Word32 -> Get TiffPlanarConfiguration
planarConfgOfConstant 0 = pure PlanarConfigContig
planarConfgOfConstant 1 = pure PlanarConfigContig
planarConfgOfConstant 2 = pure PlanarConfigSeparate
planarConfgOfConstant v = fail $ "Unknown planar constant (" ++ show v ++ ")"

constantToPlaneConfiguration :: TiffPlanarConfiguration -> Word16
constantToPlaneConfiguration PlanarConfigContig = 1
constantToPlaneConfiguration PlanarConfigSeparate = 2

data TiffCompression =
      CompressionNone           -- 1
    | CompressionModifiedRLE    -- 2
    | CompressionLZW            -- 5
    | CompressionJPEG           -- 6
    | CompressionPackBit        -- 32273

data IfdType = TypeByte
             | TypeAscii
             | TypeShort
             | TypeLong
             | TypeRational
             | TypeSByte
             | TypeUndefined
             | TypeSignedShort
             | TypeSignedLong
             | TypeSignedRational
             | TypeFloat
             | TypeDouble

instance BinaryParam Endianness IfdType where
    getP endianness = getP endianness >>= conv
      where
        conv :: Word16 -> Get IfdType
        conv 1  = return TypeByte
        conv 2  = return TypeAscii
        conv 3  = return TypeShort
        conv 4  = return TypeLong
        conv 5  = return TypeRational
        conv 6  = return TypeSByte
        conv 7  = return TypeUndefined
        conv 8  = return TypeSignedShort
        conv 9  = return TypeSignedLong
        conv 10 = return TypeSignedRational
        conv 11 = return TypeFloat
        conv 12 = return TypeDouble
        conv _  = fail "Invalid TIF directory type"

    putP endianness = putP endianness . conv
      where
        conv :: IfdType -> Word16
        conv TypeByte = 1
        conv TypeAscii = 2
        conv TypeShort = 3
        conv TypeLong = 4
        conv TypeRational = 5
        conv TypeSByte = 6
        conv TypeUndefined = 7
        conv TypeSignedShort = 8
        conv TypeSignedLong = 9
        conv TypeSignedRational = 10
        conv TypeFloat = 11
        conv TypeDouble = 12

data TiffTag = TagPhotometricInterpretation
             | TagCompression -- ^ Short type
             | TagImageWidth  -- ^ Short or long type
             | TagImageLength -- ^ Short or long type
             | TagXResolution -- ^ Rational type
             | TagYResolution -- ^ Rational type
             | TagResolutionUnit --  ^ Short type
             | TagRowPerStrip -- ^ Short or long type
             | TagStripByteCounts -- ^ Short or long
             | TagStripOffsets -- ^ Short or long
             | TagBitsPerSample --  ^ Short
             | TagColorMap -- ^ Short
             | TagTileWidth
             | TagTileLength
             | TagTileOffset
             | TagTileByteCount
             | TagSamplesPerPixel -- ^ Short
             | TagArtist
             | TagDocumentName
             | TagSoftware
             | TagPlanarConfiguration -- ^ Short
             | TagOrientation
             | TagSampleFormat -- ^ Short
             | TagInkSet
             | TagSubfileType
             | TagFillOrder
             | TagYCbCrCoeff
             | TagYCbCrSubsampling
             | TagYCbCrPositioning
             | TagReferenceBlackWhite
             | TagXPosition
             | TagYPosition
             | TagExtraSample
             | TagImageDescription

             | TagJpegProc
             | TagJPEGInterchangeFormat
             | TagJPEGInterchangeFormatLength
             | TagJPEGRestartInterval
             | TagJPEGLosslessPredictors
             | TagJPEGPointTransforms
             | TagJPEGQTables
             | TagJPEGDCTables
             | TagJPEGACTables

             | TagUnknown Word16
             deriving (Eq, Show)

tagOfWord16 :: Word16 -> TiffTag
tagOfWord16 = aux
  where aux 255 = TagSubfileType
        aux 256 = TagImageWidth
        aux 257 = TagImageLength
        aux 258 = TagBitsPerSample
        aux 259 = TagCompression
        aux 262 = TagPhotometricInterpretation
        aux 266 = TagFillOrder
        aux 269 = TagDocumentName
        aux 270 = TagImageDescription
        aux 273 = TagStripOffsets
        aux 274 = TagOrientation
        aux 277 = TagSamplesPerPixel
        aux 278 = TagRowPerStrip
        aux 279 = TagStripByteCounts
        aux 282 = TagXResolution
        aux 283 = TagYResolution
        aux 284 = TagPlanarConfiguration
        aux 286 = TagXPosition
        aux 287 = TagYPosition
        aux 296 = TagResolutionUnit
        aux 305 = TagSoftware
        aux 315 = TagArtist
        aux 320 = TagColorMap
        aux 322 = TagTileWidth
        aux 323 = TagTileLength
        aux 324 = TagTileOffset
        aux 325 = TagTileByteCount
        aux 332 = TagInkSet
        aux 338 = TagExtraSample
        aux 339 = TagSampleFormat
        aux 529 = TagYCbCrCoeff
        aux 512 = TagJpegProc
        aux 513 = TagJPEGInterchangeFormat
        aux 514 = TagJPEGInterchangeFormatLength
        aux 515 = TagJPEGRestartInterval
        aux 517 = TagJPEGLosslessPredictors
        aux 518 = TagJPEGPointTransforms
        aux 519 = TagJPEGQTables
        aux 520 = TagJPEGDCTables
        aux 521 = TagJPEGACTables
        aux 530 = TagYCbCrSubsampling
        aux 531 = TagYCbCrPositioning
        aux 532 = TagReferenceBlackWhite
        aux v = TagUnknown v

word16OfTag :: TiffTag -> Word16
word16OfTag = aux
  where aux TagSubfileType = 255
        aux TagImageWidth = 256
        aux TagImageLength = 257
        aux TagBitsPerSample = 258
        aux TagCompression = 259
        aux TagPhotometricInterpretation = 262
        aux TagFillOrder = 266
        aux TagDocumentName = 269
        aux TagImageDescription = 270
        aux TagStripOffsets = 273
        aux TagOrientation = 274
        aux TagSamplesPerPixel = 277
        aux TagRowPerStrip = 278
        aux TagStripByteCounts = 279
        aux TagXResolution = 282
        aux TagYResolution = 283
        aux TagPlanarConfiguration = 284
        aux TagXPosition = 286
        aux TagYPosition = 287
        aux TagResolutionUnit = 296
        aux TagSoftware = 305
        aux TagArtist = 315
        aux TagColorMap = 320
        aux TagTileWidth = 322
        aux TagTileLength = 323
        aux TagTileOffset = 324
        aux TagTileByteCount = 325
        aux TagInkSet = 332
        aux TagExtraSample = 338
        aux TagSampleFormat = 339
        aux TagYCbCrCoeff = 529
        aux TagJpegProc = 512
        aux TagJPEGInterchangeFormat = 513
        aux TagJPEGInterchangeFormatLength = 514
        aux TagJPEGRestartInterval = 515
        aux TagJPEGLosslessPredictors = 517
        aux TagJPEGPointTransforms = 518
        aux TagJPEGQTables = 519
        aux TagJPEGDCTables = 520
        aux TagJPEGACTables = 521
        aux TagYCbCrSubsampling = 530
        aux TagYCbCrPositioning = 531
        aux TagReferenceBlackWhite = 532
        aux (TagUnknown v) = v

instance BinaryParam Endianness TiffTag where
  getP endianness = tagOfWord16 <$> getP endianness
  putP endianness = putP endianness . word16OfTag

data ExtendedDirectoryData =
      ExtendedDataNone
    | ExtendedDataAscii !B.ByteString
    | ExtendedDataShort !(V.Vector Word16)
    | ExtendedDataLong  !(V.Vector Word32)
    deriving (Eq, Show)

instance BinaryParam (Endianness, ImageFileDirectory) ExtendedDirectoryData where
  putP (endianness, _) = dump
    where
      dump ExtendedDataNone = pure ()
      dump (ExtendedDataAscii bstr) = putByteString bstr
      dump (ExtendedDataShort shorts) = V.mapM_ (putP endianness) shorts
      dump (ExtendedDataLong longs) = V.mapM_ (putP endianness) longs

  getP (endianness, ifd) = fetcher ifd
    where
      align ImageFileDirectory { ifdOffset = offset } = do
        readed <- bytesRead
        skip . fromIntegral $ fromIntegral offset - readed

      getE :: (BinaryParam Endianness a) => Get a
      getE = getP endianness

      getVec count = V.replicateM (fromIntegral count)

      fetcher ImageFileDirectory { ifdType = TypeAscii, ifdCount = count } | count > 1 =
          align ifd >> (ExtendedDataAscii <$> getByteString (fromIntegral count))
      fetcher ImageFileDirectory { ifdType = TypeShort, ifdCount = 2, ifdOffset = ofs } =
          pure . ExtendedDataShort $ V.fromListN 2 valList
            where high = fromIntegral $ ofs `unsafeShiftR` 16
                  low = fromIntegral $ ofs .&. 0xFFFF
                  valList = case endianness of
                    EndianLittle -> [low, high]
                    EndianBig -> [high, low]
      fetcher ImageFileDirectory { ifdType = TypeShort, ifdCount = count } | count > 2 =
          align ifd >> (ExtendedDataShort <$> getVec count getE)
      fetcher ImageFileDirectory { ifdType = TypeLong, ifdCount = count } | count > 1 =
          align ifd >> (ExtendedDataLong <$> getVec count getE)
      fetcher _ = pure ExtendedDataNone

data TiffSampleFormat =
      TiffSampleUint
    | TiffSampleInt
    | TiffSampleDouble
    | TiffSampleUnknown
    deriving Eq

unpackSampleFormat :: Word32 -> Get TiffSampleFormat
unpackSampleFormat = aux
  where
    aux 1 = pure TiffSampleUint
    aux 2 = pure TiffSampleInt
    aux 3 = pure TiffSampleDouble
    aux 4 = pure TiffSampleUnknown
    aux v = fail $ "Undefined data format (" ++ show v ++ ")"

data ImageFileDirectory = ImageFileDirectory
    { ifdIdentifier :: !TiffTag
    , ifdType       :: !IfdType
    , ifdCount      :: !Word32
    , ifdOffset     :: !Word32
    , ifdExtended   :: !ExtendedDirectoryData
    }

unLong :: String -> ExtendedDirectoryData -> Get (V.Vector Word32)
unLong _ (ExtendedDataShort v) = pure $ V.map fromIntegral v
unLong _ (ExtendedDataLong v) = pure v
unLong errMessage _ = fail errMessage

cleanImageFileDirectory :: Endianness -> ImageFileDirectory -> ImageFileDirectory
cleanImageFileDirectory EndianBig ifd@(ImageFileDirectory { ifdCount = 1 }) = aux $ ifdType ifd
    where aux TypeShort = ifd { ifdOffset = ifdOffset ifd `unsafeShiftR` 16 }
          aux _ = ifd
cleanImageFileDirectory _ ifd = ifd

instance BinaryParam Endianness ImageFileDirectory where
  getP endianness =
    ImageFileDirectory <$> getE <*> getE <*> getE <*> getE
                       <*> pure ExtendedDataNone
        where getE :: (BinaryParam Endianness a) => Get a
              getE = getP endianness

  putP endianness ifd =do
    let putE :: (BinaryParam Endianness a) => a -> Put
        putE = putP endianness
    putE $ ifdIdentifier ifd
    putE $ ifdType ifd
    putE $ ifdCount ifd
    putE $ ifdOffset ifd

instance BinaryParam Endianness [ImageFileDirectory] where
  getP endianness = do
    count <- getP endianness :: Get Word16
    rez <- replicateM (fromIntegral count) $ getP endianness
    _ <- getP endianness :: Get Word32
    pure rez

  putP endianness lst = do
    let count = fromIntegral $ length lst :: Word16
    putP endianness count
    mapM_ (putP endianness) lst
    putP endianness (0 :: Word32)

fetchExtended :: Endianness -> [ImageFileDirectory] -> Get [ImageFileDirectory]
fetchExtended endian = mapM $ \ifd -> do
        v <- getP (endian, ifd)
        pure $ ifd { ifdExtended = v }

findIFD :: String -> TiffTag -> [ImageFileDirectory]
        -> Get ImageFileDirectory
findIFD errorMessage tag lst =
    case [v | v <- lst, ifdIdentifier v == tag] of
        [] -> fail errorMessage
        (x:_) -> pure x

findPalette :: [ImageFileDirectory] -> Get (Maybe (Image PixelRGB16))
findPalette ifds =
    case [v | v <- ifds, ifdIdentifier v == TagColorMap] of
        (ImageFileDirectory { ifdExtended = ExtendedDataShort vec }:_) ->
            pure . Just . Image pixelCount 1 $ VS.generate (V.length vec) axx
                where pixelCount = V.length vec `div` 3
                      axx v = vec `V.unsafeIndex` (idx + color * pixelCount)
                          where (idx, color) = v `divMod` 3

        _ -> pure Nothing

findIFDData :: String -> TiffTag -> [ImageFileDirectory] -> Get Word32
findIFDData msg tag lst = ifdOffset <$> findIFD msg tag lst

findIFDDefaultData :: Word32 -> TiffTag -> [ImageFileDirectory] -> Get Word32
findIFDDefaultData d tag lst =
    case [v | v <- lst, ifdIdentifier v == tag] of
        [] -> pure d
        (x:_) -> pure $ ifdOffset x

findIFDExt :: String -> TiffTag -> [ImageFileDirectory] -> Get ExtendedDirectoryData
findIFDExt msg tag lst = do
    val <- findIFD msg tag lst
    case val of
      ImageFileDirectory
        { ifdCount = 1, ifdOffset = ofs, ifdType = TypeShort } ->
               pure . ExtendedDataShort . V.singleton $ fromIntegral ofs
      ImageFileDirectory
        { ifdCount = 1, ifdOffset = ofs, ifdType = TypeLong } ->
               pure . ExtendedDataLong . V.singleton $ fromIntegral ofs
      ImageFileDirectory { ifdExtended = v } -> pure v


findIFDExtDefaultData :: [Word32] -> TiffTag -> [ImageFileDirectory]
                      -> Get [Word32]
findIFDExtDefaultData d tag lst =
    case [v | v <- lst, ifdIdentifier v == tag] of
        [] -> pure d
        (ImageFileDirectory { ifdExtended = ExtendedDataNone }:_) -> return d
        (x:_) -> V.toList <$> unLong errorMessage (ifdExtended x)
            where errorMessage =
                    "Can't parse tag " ++ show tag ++ " " ++ show (ifdExtended x)

-- It's temporary, remove once tiff decoding is better
-- handled.
{-  
instance Show (Image PixelRGB16) where
    show _ = "Image PixelRGB16"
-}

data TiffInfo = TiffInfo
    { tiffHeader             :: TiffHeader
    , tiffWidth              :: Word32
    , tiffHeight             :: Word32
    , tiffColorspace         :: TiffColorspace
    , tiffSampleCount        :: Word32
    , tiffRowPerStrip        :: Word32
    , tiffPlaneConfiguration :: TiffPlanarConfiguration
    , tiffSampleFormat       :: [TiffSampleFormat]
    , tiffBitsPerSample      :: V.Vector Word32
    , tiffCompression        :: TiffCompression
    , tiffStripSize          :: V.Vector Word32
    , tiffOffsets            :: V.Vector Word32
    , tiffPalette            :: Maybe (Image PixelRGB16)
    , tiffYCbCrSubsampling   :: V.Vector Word32
    }

data TiffColorspace =
      TiffMonochromeWhite0 -- ^ 0
    | TiffMonochrome       -- ^ 1
    | TiffRGB              -- ^ 2
    | TiffPaleted          -- ^ 3
    | TiffTransparencyMask -- ^ 4
    | TiffCMYK             -- ^ 5
    | TiffYCbCr            -- ^ 6
    | TiffCIELab           -- ^ 8

packPhotometricInterpretation :: TiffColorspace -> Word16
packPhotometricInterpretation = aux
  where
    aux TiffMonochromeWhite0 = 0
    aux TiffMonochrome       = 1
    aux TiffRGB              = 2
    aux TiffPaleted          = 3
    aux TiffTransparencyMask = 4
    aux TiffCMYK             = 5
    aux TiffYCbCr            = 6
    aux TiffCIELab           = 8

unpackPhotometricInterpretation :: Word32 -> Get TiffColorspace
unpackPhotometricInterpretation = aux
  where aux 0 = pure TiffMonochromeWhite0
        aux 1 = pure TiffMonochrome
        aux 2 = pure TiffRGB
        aux 3 = pure TiffPaleted
        aux 4 = pure TiffTransparencyMask
        aux 5 = pure TiffCMYK
        aux 6 = pure TiffYCbCr
        aux 8 = pure TiffCIELab
        aux v = fail $ "Unrecognized color space " ++ show v

unPackCompression :: Word32 -> Get TiffCompression
unPackCompression = aux
  where
    aux 0 = pure CompressionNone
    aux 1 = pure CompressionNone
    aux 2 = pure CompressionModifiedRLE
    aux 5 = pure CompressionLZW
    aux 6 = pure CompressionJPEG
    aux 32773 = pure CompressionPackBit
    aux v = fail $ "Unknown compression scheme " ++ show v

packCompression :: TiffCompression -> Word16
packCompression = aux
  where
    aux CompressionNone        = 1
    aux CompressionModifiedRLE = 2
    aux CompressionLZW         = 5
    aux CompressionJPEG        = 6
    aux CompressionPackBit     = 32773

copyByteString :: B.ByteString -> M.STVector s Word8 -> Int -> Int -> (Word32, Word32)
               -> ST s Int
copyByteString str vec stride startWrite (from, count) = inner startWrite fromi
  where fromi = fromIntegral from
        maxi = fromi + fromIntegral count

        inner writeIdx i | i >= maxi = pure writeIdx
        inner writeIdx i = do
            let v = str `BU.unsafeIndex` i
            (vec `M.unsafeWrite` writeIdx) v
            inner (writeIdx + stride) $ i + 1

unpackPackBit :: B.ByteString -> M.STVector s Word8 -> Int -> Int
              -> (Word32, Word32)
              -> ST s Int
unpackPackBit str outVec stride writeIndex (offset, size) = loop fromi writeIndex
  where fromi = fromIntegral offset
        maxi = fromi + fromIntegral size

        replicateByte writeIdx _     0 = pure writeIdx
        replicateByte writeIdx v count = do
            (outVec `M.unsafeWrite` writeIdx) v
            replicateByte (writeIdx + stride) v $ count - 1

        loop i writeIdx | i >= maxi = pure writeIdx
        loop i writeIdx = choice
          {-where v = fromIntegral (str `BU.unsafeIndex` i) :: Int8-}
          where v = fromIntegral (str `B.index` i) :: Int8

                choice
                    -- data
                    | 0    <= v = do
                        copyByteString str outVec stride writeIdx
                                        (fromIntegral $ i + 1, fromIntegral v + 1)
                            >>= loop (i + 2 + fromIntegral v)
                    -- run
                    | -127 <= v = do
                        {-let nextByte = str `BU.unsafeIndex` (i + 1)-}
                        let nextByte = str `B.index` (i + 1)
                            count = negate (fromIntegral v) + 1 :: Int
                        replicateByte writeIdx nextByte count
                            >>= loop (i + 2)

                    -- noop
                    | otherwise = loop writeIdx $ i + 1

uncompressAt :: TiffCompression
             -> B.ByteString -> M.STVector s Word8 -> Int -> Int -> (Word32, Word32)
             -> ST s Int
uncompressAt CompressionNone = copyByteString
uncompressAt CompressionPackBit = unpackPackBit
uncompressAt CompressionLZW =  \str outVec _stride writeIndex (offset, size) -> do
    let toDecode = B.take (fromIntegral size) $ B.drop (fromIntegral offset) str
    runBoolReader $ decodeLzwTiff toDecode outVec writeIndex
    return 0
uncompressAt _ = error "Unhandled compression"

class Unpackable a where
    type StorageType a :: *

    outAlloc :: a -> Int -> ST s (M.STVector s (StorageType a))

    -- | Final image and size, return offset and vector
    allocTempBuffer :: a  -> M.STVector s (StorageType a) -> Int
                    -> ST s (M.STVector s Word8)

    offsetStride :: a -> Int -> Int -> (Int, Int)

    mergeBackTempBuffer :: a    -- ^ Type witness, just for the type checker.
                        -> Endianness
                        -> M.STVector s Word8 -- ^ Temporary buffer handling decompression.
                        -> Int -- ^ Line size in pixels
                        -> Int  -- ^ Write index, in bytes
                        -> Word32  -- ^ size, in bytes
                        -> Int  -- ^ Stride
                        -> M.STVector s (StorageType a) -- ^ Final buffer
                        -> ST s ()

-- | The Word8 instance is just a passthrough, to avoid
-- copying memory twice
instance Unpackable Word8 where
  type StorageType Word8 = Word8

  offsetStride _ i stride = (i, stride)
  allocTempBuffer _ buff _ = pure buff
  mergeBackTempBuffer _ _ _ _ _ _ _ _ = pure ()
  outAlloc _ count = M.replicate count 0 -- M.new

instance Unpackable Word16 where
  type StorageType Word16 = Word16

  offsetStride _ _ _ = (0, 1)
  outAlloc _ = M.new
  allocTempBuffer _ _ s = M.new $ s * 2
  mergeBackTempBuffer _ EndianLittle tempVec _ index size stride outVec =
        looperLe index 0
    where looperLe _ readIndex | readIndex >= fromIntegral size = pure ()
          looperLe writeIndex readIndex = do
              v1 <- tempVec `M.read` readIndex
              v2 <- tempVec `M.read` (readIndex + 1)
              let finalValue =
                    (fromIntegral v2 `unsafeShiftL` 8) .|. fromIntegral v1
              (outVec `M.write` writeIndex) finalValue

              looperLe (writeIndex + stride) (readIndex + 2)
  mergeBackTempBuffer _ EndianBig tempVec _ index size stride outVec =
         looperBe index 0
    where looperBe _ readIndex | readIndex >= fromIntegral size = pure ()
          looperBe writeIndex readIndex = do
              v1 <- tempVec `M.read` readIndex
              v2 <- tempVec `M.read` (readIndex + 1)
              let finalValue =
                    (fromIntegral v1 `unsafeShiftL` 8) .|. fromIntegral v2
              (outVec `M.write` writeIndex) finalValue

              looperBe (writeIndex + stride) (readIndex + 2)

instance Unpackable Word32 where
  type StorageType Word32 = Word32

  offsetStride _ _ _ = (0, 1)
  outAlloc _ = M.new
  allocTempBuffer _ _ s = M.new $ s * 4
  mergeBackTempBuffer _ EndianLittle tempVec _ index size stride outVec =
        looperLe index 0
    where looperLe _ readIndex | readIndex >= fromIntegral size = pure ()
          looperLe writeIndex readIndex = do
              v1 <- tempVec `M.read` readIndex
              v2 <- tempVec `M.read` (readIndex + 1)
              v3 <- tempVec `M.read` (readIndex + 2)
              v4 <- tempVec `M.read` (readIndex + 3)
              let finalValue =
                    (fromIntegral v4 `unsafeShiftL` 24) .|.
                    (fromIntegral v3 `unsafeShiftL` 16) .|.
                    (fromIntegral v2 `unsafeShiftL` 8) .|.
                    fromIntegral v1
              (outVec `M.write` writeIndex) finalValue

              looperLe (writeIndex + stride) (readIndex + 4)
  mergeBackTempBuffer _ EndianBig tempVec _ index size stride outVec =
         looperBe index 0
    where looperBe _ readIndex | readIndex >= fromIntegral size = pure ()
          looperBe writeIndex readIndex = do
              v1 <- tempVec `M.read` readIndex
              v2 <- tempVec `M.read` (readIndex + 1)
              v3 <- tempVec `M.read` (readIndex + 2)
              v4 <- tempVec `M.read` (readIndex + 3)
              let finalValue =
                    (fromIntegral v1 `unsafeShiftL` 24) .|.
                    (fromIntegral v2 `unsafeShiftL` 16) .|.
                    (fromIntegral v3 `unsafeShiftL` 8) .|.
                    fromIntegral v4
              (outVec `M.write` writeIndex) finalValue

              looperBe (writeIndex + stride) (readIndex + 4)

data Pack4 = Pack4

instance Unpackable Pack4 where
  type StorageType Pack4 = Word8
  allocTempBuffer _ _ = M.new
  offsetStride _ _ _ = (0, 1)
  outAlloc _ = M.new
  mergeBackTempBuffer _ _ tempVec lineSize index size stride outVec =
        inner 0 index pxCount
    where pxCount = lineSize `div` stride

          maxWrite = M.length outVec
          inner readIdx writeIdx _
                | readIdx >= fromIntegral size || writeIdx >= maxWrite = pure ()
          inner readIdx writeIdx line
                | line <= 0 = inner readIdx (writeIdx + line * stride) pxCount
          inner readIdx writeIdx line = do
            v <- tempVec `M.read` readIdx
            let high = (v `unsafeShiftR` 4) .&. 0xF
                low = v .&. 0xF
            (outVec `M.write` writeIdx) high
            when (writeIdx + stride < maxWrite) $
                 (outVec `M.write` (writeIdx + stride)) low

            inner (readIdx + 1) (writeIdx + 2 * stride) (line - 2)

data Pack2 = Pack2

instance Unpackable Pack2 where
  type StorageType Pack2 = Word8
  allocTempBuffer _ _ = M.new
  offsetStride _ _ _ = (0, 1)
  outAlloc _ = M.new
  mergeBackTempBuffer _ _ tempVec lineSize index size stride outVec =
        inner 0 index pxCount
    where pxCount = lineSize `div` stride

          maxWrite = M.length outVec
          inner readIdx writeIdx _
                | readIdx >= fromIntegral size || writeIdx >= maxWrite = pure ()
          inner readIdx writeIdx line
                | line <= 0 = inner readIdx (writeIdx + line * stride) pxCount
          inner readIdx writeIdx line = do
            v <- tempVec `M.read` readIdx
            let v0 = (v `unsafeShiftR` 6) .&. 0x3
                v1 = (v `unsafeShiftR` 4) .&. 0x3
                v2 = (v `unsafeShiftR` 2) .&. 0x3
                v3 = v .&. 0x3

            (outVec `M.write` writeIdx) v0
            when (writeIdx + 1 * stride < maxWrite) $
                 (outVec `M.write` (writeIdx + stride)) v1

            when (writeIdx + 2 * stride < maxWrite) $
                 (outVec `M.write` (writeIdx + stride * 2)) v2

            when (writeIdx + 3 * stride < maxWrite) $
                 (outVec `M.write` (writeIdx + stride * 3)) v3

            inner (readIdx + 1) (writeIdx + 4 * stride) (line - 4)

data Pack12 = Pack12

instance Unpackable Pack12 where
  type StorageType Pack12 = Word16
  allocTempBuffer _ _ = M.new
  offsetStride _ _ _ = (0, 1)
  outAlloc _ = M.new
  mergeBackTempBuffer _ _ tempVec lineSize index size stride outVec =
        inner 0 index pxCount
    where pxCount = lineSize `div` stride

          maxWrite = M.length outVec
          inner readIdx writeIdx _
                | readIdx >= fromIntegral size || writeIdx >= maxWrite = pure ()
          inner readIdx writeIdx line
                | line <= 0 = inner readIdx (writeIdx + line * stride) pxCount
          inner readIdx writeIdx line = do
            v0 <- tempVec `M.read` readIdx
            v1 <- if readIdx + 1 < fromIntegral size
                then tempVec `M.read` (readIdx + 1)
                else pure 0
            v2 <- if readIdx + 2 < fromIntegral size
                then tempVec `M.read` (readIdx + 2)
                else pure 0

            let high0 = fromIntegral v0 `unsafeShiftL` 4
                low0 = (fromIntegral v1 `unsafeShiftR` 4) .&. 0xF

                p0 = high0 .|. low0

                high1 = (fromIntegral v1 .&. 0xF) `unsafeShiftL` 8
                low1 = fromIntegral v2
                p1 = high1 .|. low1

            (outVec `M.write` writeIdx) p0
            when (writeIdx + 1 * stride < maxWrite) $
                 (outVec `M.write` (writeIdx + stride)) p1

            inner (readIdx + 3) (writeIdx + 2 * stride) (line - 2)

data YCbCrSubsampling = YCbCrSubsampling
    { ycbcrWidth        :: !Int
    , ycbcrHeight       :: !Int
    , ycbcrImageWidth   :: !Int
    , ycbcrStripHeight  :: !Int
    }

instance Unpackable YCbCrSubsampling where
  type StorageType YCbCrSubsampling = Word8

  offsetStride _ _ _ = (0, 1)
  outAlloc _ = M.new
  allocTempBuffer _ _ = M.new
  mergeBackTempBuffer subSampling _ tempVec _ index size _ outVec =
      foldM_ unpacker 0 [(bx, by) | by <- [0, h .. lineCount - 1]
                                  , bx <- [0, w .. imgWidth - 1]]
    where w = ycbcrWidth subSampling
          h = ycbcrHeight subSampling
          imgWidth = ycbcrImageWidth subSampling
          lineCount = ycbcrStripHeight subSampling

          lumaCount = w * h
          blockSize = lumaCount + 2

          maxOut = M.length outVec

          unpacker readIdx _ | readIdx >= fromIntegral size * 3 = pure readIdx
          unpacker readIdx (bx, by) = do
              cb <- tempVec `M.read` (readIdx + lumaCount)
              cr <- tempVec `M.read` (readIdx + lumaCount + 1)

              let pixelIndices =
                        [index + ((by + y) * imgWidth + bx + x) * 3 | y <- [0 .. h - 1], x <- [0 .. w - 1]]

                  writer readIndex writeIdx | writeIdx + 3 > maxOut = pure readIndex
                  writer readIndex writeIdx = do
                    y <- tempVec `M.read` readIndex
                    (outVec `M.write` writeIdx) y
                    (outVec `M.write` (writeIdx + 1)) cb
                    (outVec `M.write` (writeIdx + 2)) cr
                    return $ readIndex + 1

              foldM_ writer readIdx pixelIndices

              return $ readIdx + blockSize

gatherStrips :: ( Unpackable comp
                , Pixel pixel
                , StorageType comp ~ PixelBaseComponent pixel
                )
             => comp -> B.ByteString -> TiffInfo -> Image pixel
gatherStrips comp str nfo = runST $ do
  let width = fromIntegral $ tiffWidth nfo
      height = fromIntegral $ tiffHeight nfo
      sampleCount = if tiffSampleCount nfo /= 0
        then fromIntegral $ tiffSampleCount nfo
        else V.length $ tiffBitsPerSample nfo

      rowPerStrip = fromIntegral $ tiffRowPerStrip nfo
      endianness = hdrEndianness $ tiffHeader nfo

      stripCount = V.length $ tiffOffsets nfo
      compression = tiffCompression nfo

  outVec <- outAlloc comp $ width * height * sampleCount
  tempVec <- allocTempBuffer comp outVec
                        (rowPerStrip * width * sampleCount)

  let mutableImage = MutableImage
                   { mutableImageWidth = fromIntegral width
                   , mutableImageHeight = fromIntegral height
                   , mutableImageData = outVec
                   }

  case tiffPlaneConfiguration nfo of
    PlanarConfigContig -> V.mapM_ unpacker sizes
        where unpacker (idx, offset, size) = do
                  let (writeIdx, tempStride)  = offsetStride comp idx 1
                  _ <- uncompressAt compression str tempVec tempStride
                                    writeIdx (offset, size)
                  mergeBackTempBuffer comp endianness tempVec (width * sampleCount)
                                      idx size 1 outVec

              startWriteOffset =
                  V.generate stripCount(width * rowPerStrip * sampleCount *)

              sizes = V.zip3 startWriteOffset (tiffOffsets nfo) (tiffStripSize nfo)

    PlanarConfigSeparate -> V.mapM_ unpacker sizes
        where unpacker (idx, offset, size) = do
                  let (writeIdx, tempStride) = offsetStride comp idx stride
                  _ <- uncompressAt compression str tempVec tempStride
                                    writeIdx (offset, size)
                  mergeBackTempBuffer comp endianness tempVec (width * sampleCount)
                                      idx size stride outVec

              stride = V.length $ tiffOffsets nfo
              idxVector = V.enumFromN 0 stride
              sizes = V.zip3 idxVector (tiffOffsets nfo) (tiffStripSize nfo)

  unsafeFreezeImage mutableImage

ifdSingleLong :: TiffTag -> Word32 -> Writer [ImageFileDirectory] ()
ifdSingleLong tag = ifdMultiLong tag . V.singleton

ifdSingleShort :: Endianness -> TiffTag -> Word16
               -> Writer [ImageFileDirectory] ()
ifdSingleShort endian tag = ifdMultiShort endian tag . V.singleton . fromIntegral

ifdMultiLong :: TiffTag -> V.Vector Word32 -> Writer [ImageFileDirectory] ()
ifdMultiLong tag v = tell . pure $ ImageFileDirectory
        { ifdIdentifier = tag
        , ifdType       = TypeLong
        , ifdCount      = fromIntegral $ V.length v
        , ifdOffset     = offset
        , ifdExtended   = extended
        }
  where (offset, extended)
                | V.length v > 1 = (0, ExtendedDataLong v)
                | otherwise = (V.head v, ExtendedDataNone)

ifdMultiShort :: Endianness -> TiffTag -> V.Vector Word32
              -> Writer [ImageFileDirectory] ()
ifdMultiShort endian tag v = tell . pure $ ImageFileDirectory
        { ifdIdentifier = tag
        , ifdType       = TypeShort
        , ifdCount      = size
        , ifdOffset     = offset
        , ifdExtended   = extended
        }
    where size = fromIntegral $ V.length v
          (offset, extended)
                | size > 2 = (0, ExtendedDataShort $ V.map fromIntegral v)
                | size == 2 =
                    let v1 = fromIntegral $ V.head v
                        v2 = fromIntegral $ v `V.unsafeIndex` 1
                    in
                    case endian of
                      EndianLittle -> (v2 `unsafeShiftL` 16 .|. v1, ExtendedDataNone)
                      EndianBig -> (v1 `unsafeShiftL` 16 .|. v2, ExtendedDataNone)

                | otherwise = case endian of
                    EndianLittle -> (V.head v, ExtendedDataNone)
                    EndianBig -> (V.head v `unsafeShiftL` 16, ExtendedDataNone)

-- | All the IFD must be written in order according to the tag
-- value of the IFD. To avoid getting to much restriction in the
-- serialization code, just sort it.
orderIfdByTag :: [ImageFileDirectory] -> [ImageFileDirectory]
orderIfdByTag = sortBy comparer
  where comparer a b = compare t1 t2
           where t1 = word16OfTag $ ifdIdentifier a
                 t2 = word16OfTag $ ifdIdentifier b

-- | Given an official offset and a list of IFD, update the offset information
-- of the IFD with extended data.
setupIfdOffsets :: Word32 -> [ImageFileDirectory] -> [ImageFileDirectory]
setupIfdOffsets initialOffset lst = snd $ mapAccumL updater startExtended lst
  where ifdElementCount = fromIntegral $ length lst
        ifdSize = 12
        ifdCountSize = 2
        nextOffsetSize = 4
        startExtended = initialOffset
                     + ifdElementCount * ifdSize
                     + ifdCountSize + nextOffsetSize

        updater ix ifd@(ImageFileDirectory { ifdExtended = ExtendedDataAscii b }) =
            (ix + fromIntegral (B.length b), ifd { ifdOffset = ix } )
        updater ix ifd@(ImageFileDirectory { ifdExtended = ExtendedDataLong v })
            | V.length v > 1 = ( ix + fromIntegral (V.length v * 4)
                               , ifd { ifdOffset = ix } )
        updater ix ifd@(ImageFileDirectory { ifdExtended = ExtendedDataShort v })
            | V.length v > 2 = ( ix + fromIntegral (V.length v * 2)
                             , ifd { ifdOffset = ix })
        updater ix ifd = (ix, ifd)

instance BinaryParam B.ByteString TiffInfo where
  putP rawData nfo = do
    put $ tiffHeader nfo

    let ifdStartOffset = hdrOffset $ tiffHeader nfo
        endianness = hdrEndianness $ tiffHeader nfo

        ifdShort = ifdSingleShort endianness
        ifdShorts = ifdMultiShort endianness
        list = setupIfdOffsets ifdStartOffset . orderIfdByTag . execWriter $ do
            ifdSingleLong TagImageWidth $ tiffWidth nfo
            ifdSingleLong TagImageLength $ tiffHeight nfo
            ifdShorts TagBitsPerSample $ tiffBitsPerSample nfo
            ifdSingleLong TagSamplesPerPixel $ tiffSampleCount nfo
            ifdSingleLong TagRowPerStrip $ tiffRowPerStrip nfo
            ifdShort TagPhotometricInterpretation
                                        . packPhotometricInterpretation
                                        $ tiffColorspace nfo
            ifdShort TagPlanarConfiguration
                    . constantToPlaneConfiguration $ tiffPlaneConfiguration nfo
            ifdShort TagCompression . packCompression
                                          $ tiffCompression nfo
            ifdMultiLong TagStripOffsets $ tiffOffsets nfo

            ifdMultiLong TagStripByteCounts $ tiffStripSize nfo

            let subSampling = tiffYCbCrSubsampling nfo
            when (not $ V.null subSampling) $
                 ifdShorts TagYCbCrSubsampling subSampling

    putByteString rawData
    putP endianness list
    mapM_ (\ifd -> putP (endianness, ifd) $ ifdExtended ifd) list

  getP _ = do
    hdr <- get
    readed <- bytesRead
    skip . fromIntegral $ fromIntegral (hdrOffset hdr) - readed
    let endian = hdrEndianness hdr

    ifd <- fmap (cleanImageFileDirectory endian) <$> getP endian
    cleaned <- fetchExtended endian ifd

    let dataFind str tag = findIFDData str tag cleaned
        dataDefault def tag = findIFDDefaultData def tag cleaned
        extFind str tag = findIFDExt str tag cleaned
        extDefault def tag = findIFDExtDefaultData def tag cleaned

    TiffInfo hdr
        <$> dataFind "Can't find width" TagImageWidth
        <*> dataFind "Can't find height" TagImageLength
        <*> (dataFind "Can't find color space" TagPhotometricInterpretation
                     >>= unpackPhotometricInterpretation)
        <*> dataFind "Can't find sample per pixel" TagSamplesPerPixel
        <*> dataFind "Can't find row per strip" TagRowPerStrip
        <*> (dataDefault 1 TagPlanarConfiguration
                     >>= planarConfgOfConstant)
        <*> (extDefault [1] TagSampleFormat
                     >>= mapM unpackSampleFormat)
        <*> (extFind "Can't find bit per sample" TagBitsPerSample
                     >>= unLong "Can't find bit depth")
        <*> (dataFind "Can't find Compression" TagCompression
                     >>= unPackCompression)
        <*> (extFind "Can't find byte counts" TagStripByteCounts
                     >>= unLong "Can't find bit per sample")
        <*> (extFind "Strip offsets missing" TagStripOffsets
                     >>= unLong "Can't find strip offsets")
        <*> findPalette cleaned
        <*> (V.fromList <$> extDefault [2, 2] TagYCbCrSubsampling)

unpack :: B.ByteString -> TiffInfo -> Either String DynamicImage
-- | while mandatory some images don't put correct
-- rowperstrip. So replacing 0 with actual image height.
unpack file nfo@TiffInfo { tiffRowPerStrip = 0 } =
    unpack file $ nfo { tiffRowPerStrip = tiffHeight nfo }
unpack file nfo@TiffInfo { tiffColorspace = TiffPaleted
                         , tiffBitsPerSample = lst
                         , tiffSampleFormat = format
                         , tiffPalette = Just p
                         }
  | lst == V.singleton 8 && format == [TiffSampleUint] =
      let applyPalette = pixelMap (\v -> pixelAt p (fromIntegral v) 0)
          gathered :: Image Pixel8
          gathered = gatherStrips (0 :: Word8) file nfo
      in
      pure . ImageRGB16 $ applyPalette gathered

  | lst == V.singleton 4 && format == [TiffSampleUint] =
      let applyPalette = pixelMap (\v -> pixelAt p (fromIntegral v) 0)
          gathered :: Image Pixel8
          gathered = gatherStrips Pack4 file nfo
      in
      pure . ImageRGB16 $ applyPalette gathered

  | lst == V.singleton 2 && format == [TiffSampleUint] =
      let applyPalette = pixelMap (\v -> pixelAt p (fromIntegral v) 0)
          gathered :: Image Pixel8
          gathered = gatherStrips Pack2 file nfo
      in
      pure . ImageRGB16 $ applyPalette gathered

unpack file nfo@TiffInfo { tiffColorspace = TiffCMYK
                         , tiffBitsPerSample = lst
                         , tiffSampleFormat = format }
  | lst == V.fromList [8, 8, 8, 8] && all (TiffSampleUint ==) format =
        pure . ImageCMYK8 $ gatherStrips (0 :: Word8) file nfo

  | lst == V.fromList [16, 16, 16, 16] && all (TiffSampleUint ==) format =
        pure . ImageCMYK16 $ gatherStrips (0 :: Word16) file nfo

unpack file nfo@TiffInfo { tiffColorspace = TiffMonochromeWhite0 }
  | otherwise = do
    img <- unpack file (nfo { tiffColorspace = TiffMonochrome })
    case img of
      ImageY8 i -> pure . ImageY8 $ pixelMap (maxBound -) i
      ImageY16 i -> pure . ImageY16 $ pixelMap (maxBound -) i
      _ -> pure img

unpack file nfo@TiffInfo { tiffColorspace = TiffMonochrome
                         , tiffBitsPerSample = lst
                         , tiffSampleFormat = format }
  | lst == V.singleton 2 && all (TiffSampleUint ==) format =
        pure . ImageY8 . pixelMap (colorMap (64 *)) $ gatherStrips Pack2 file nfo
  | lst == V.singleton 4 && all (TiffSampleUint ==) format =
        pure . ImageY8 . pixelMap (colorMap (16 *)) $ gatherStrips Pack4 file nfo
  | lst == V.singleton 8 && all (TiffSampleUint ==) format =
        pure . ImageY8 $ gatherStrips (0 :: Word8) file nfo
  | lst == V.singleton 12 && all (TiffSampleUint ==) format =
        pure . ImageY16 . pixelMap (16 *) $ gatherStrips Pack12 file nfo
  | lst == V.singleton 16 && all (TiffSampleUint ==) format =
        pure . ImageY16 $ gatherStrips (0 :: Word16) file nfo
  | lst == V.singleton 32 && all (TiffSampleUint ==) format =
        pure . ImageY16 $ pixelMap (toWord16) img
           where toWord16 v = fromIntegral $ v `unsafeShiftR` 16
                 img = gatherStrips (0 :: Word32) file nfo :: Image Pixel32

unpack file nfo@TiffInfo { tiffColorspace = TiffYCbCr
                         , tiffBitsPerSample = lst
                         , tiffPlaneConfiguration = PlanarConfigContig
                         , tiffSampleFormat = format }
  | lst == V.fromList [8, 8, 8] && all (TiffSampleUint ==) format =
    pure . ImageYCbCr8 $ gatherStrips cbcrConf  file nfo
      where defaulting 0 = 2
            defaulting n = n

            w = defaulting $ tiffYCbCrSubsampling nfo V.! 0
            h = defaulting $ tiffYCbCrSubsampling nfo V.! 1
            cbcrConf = YCbCrSubsampling
                { ycbcrWidth        = fromIntegral w
                , ycbcrHeight       = fromIntegral h
                , ycbcrImageWidth   = fromIntegral $ tiffWidth nfo
                , ycbcrStripHeight  = fromIntegral $ tiffRowPerStrip nfo
                }

unpack file nfo@TiffInfo { tiffColorspace = TiffRGB
                         , tiffBitsPerSample = lst
                         , tiffSampleFormat = format }
  | lst == V.fromList [2, 2, 2] && all (TiffSampleUint ==) format =
        pure . ImageRGB8 . pixelMap (colorMap (64 *)) $ gatherStrips Pack2 file nfo
  | lst == V.fromList [4, 4, 4] && all (TiffSampleUint ==) format =
        pure . ImageRGB8 . pixelMap (colorMap (16 *)) $ gatherStrips Pack4 file nfo
  | lst == V.fromList [8, 8, 8] && all (TiffSampleUint ==) format =
        pure . ImageRGB8 $ gatherStrips (0 :: Word8) file nfo
  | lst == V.fromList [8, 8, 8, 8] && all (TiffSampleUint ==) format =
        pure . ImageRGBA8 $ gatherStrips (0 :: Word8) file nfo
  | lst == V.fromList [16, 16, 16] && all (TiffSampleUint ==) format =
        pure . ImageRGB16 $ gatherStrips (0 :: Word16) file nfo
  | lst == V.fromList [16, 16, 16, 16] && all (TiffSampleUint ==) format =
        pure . ImageRGBA16 $ gatherStrips (0 :: Word16) file nfo
unpack file nfo@TiffInfo { tiffColorspace = TiffMonochrome
                         , tiffBitsPerSample = lst
                         , tiffSampleFormat = format }
  -- some files are a little bit borked...
  | lst == V.fromList [8, 8, 8] && all (TiffSampleUint ==) format =
        pure . ImageRGB8 $ gatherStrips (0 :: Word8) file nfo

unpack _ _ = fail "Failure to unpack TIFF file"

-- | Decode a tiff encoded image while preserving the underlying
-- pixel type.
--
-- This function can output the following pixel types :
--
-- * PixelY8
--
-- * PixelY16
--
-- * PixelRGB8
--
-- * PixelRGB16
--
-- * PixelCMYK8
--
-- * PixelCMYK16
--
decodeTiff :: B.ByteString -> Either String DynamicImage
decodeTiff file = runGetStrict (getP file) file >>= unpack file

-- | Class defining which pixel types can be serialized in a
-- Tiff file.
class (Pixel px) => TiffSaveable px where
  colorSpaceOfPixel :: px -> TiffColorspace

  subSamplingInfo   :: px -> V.Vector Word32
  subSamplingInfo _ = V.empty

instance TiffSaveable Pixel8 where
  colorSpaceOfPixel _ = TiffMonochrome

instance TiffSaveable Pixel16 where
  colorSpaceOfPixel _ = TiffMonochrome

instance TiffSaveable PixelCMYK8 where
  colorSpaceOfPixel _ = TiffCMYK

instance TiffSaveable PixelCMYK16 where
  colorSpaceOfPixel _ = TiffCMYK

instance TiffSaveable PixelRGB8 where
  colorSpaceOfPixel  _ = TiffRGB

instance TiffSaveable PixelRGB16 where
  colorSpaceOfPixel  _ = TiffRGB

instance TiffSaveable PixelRGBA8 where
  colorSpaceOfPixel _ = TiffRGB

instance TiffSaveable PixelRGBA16 where
  colorSpaceOfPixel _ = TiffRGB

instance TiffSaveable PixelYCbCr8 where
  colorSpaceOfPixel _ = TiffYCbCr
  subSamplingInfo _ = V.fromListN 2 [1, 1]

-- | Transform an image into a Tiff encoded bytestring, reade to be
-- written as a file.
encodeTiff :: forall px. (TiffSaveable px) => Image px -> Lb.ByteString
encodeTiff img = runPut $ putP rawPixelData hdr
  where intSampleCount = componentCount (undefined :: px)
        sampleCount = fromIntegral intSampleCount

        sampleType = (undefined :: PixelBaseComponent px)
        pixelData = imageData img

        rawPixelData = toByteString pixelData
        width = fromIntegral $ imageWidth img
        height = fromIntegral $ imageHeight img
        intSampleSize = sizeOf sampleType
        sampleSize = fromIntegral intSampleSize
        bitPerSample = sampleSize * 8
        imageSize = width * height * sampleCount * sampleSize
        headerSize = 8

        hdr = TiffInfo
            { tiffHeader             = TiffHeader
                                            { hdrEndianness = EndianLittle
                                            , hdrOffset = headerSize + imageSize
                                            }
            , tiffWidth              = width
            , tiffHeight             = height
            , tiffColorspace         = colorSpaceOfPixel (undefined :: px)
            , tiffSampleCount        = fromIntegral $ sampleCount
            , tiffRowPerStrip        = fromIntegral $ imageHeight img
            , tiffPlaneConfiguration = PlanarConfigContig
            , tiffSampleFormat       = [TiffSampleUint]
            , tiffBitsPerSample      = V.replicate intSampleCount bitPerSample
            , tiffCompression        = CompressionNone
            , tiffStripSize          = V.singleton imageSize
            , tiffOffsets            = V.singleton headerSize
            , tiffPalette            = Nothing
            , tiffYCbCrSubsampling   = subSamplingInfo (undefined :: px)
            }

-- | Helper function to directly write an image as a tiff on disk.
writeTiff :: (TiffSaveable pixel) => FilePath -> Image pixel -> IO ()
writeTiff path img = Lb.writeFile path $ encodeTiff img

