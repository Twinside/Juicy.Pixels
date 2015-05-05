{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Codec.Picture.Tiff.Types
    ( BinaryParam( .. )
    , Endianness( .. )
    , TiffHeader( .. )
    , TiffPlanarConfiguration( .. )
    , TiffCompression( .. )
    , IfdType( .. )
    , TiffTag( .. )
    , TiffColorspace( .. )
    , ExtendedDirectoryData( .. )
    , TiffSampleFormat( .. )
    , ImageFileDirectory( .. )
    , ExtraSample( .. )

    , planarConfgOfConstant
    , constantToPlaneConfiguration
    , unpackSampleFormat
    , word16OfTag
    , unpackPhotometricInterpretation
    , packPhotometricInterpretation
    , codeOfExtraSample
    , unPackCompression
    , packCompression 
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>), (<*>), pure )
#endif

import Control.Monad( when, replicateM, )
import Data.Bits( (.&.), unsafeShiftR )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( Get
                      , getWord16le, getWord16be
                      , getWord32le, getWord32be
                      , bytesRead
                      , skip
                      , getByteString
                      )
import Data.Binary.Put( Put
                      , putWord16le, putWord16be
                      , putWord32le, putWord32be
                      , putByteString
                      )

import qualified Data.Vector as V
import qualified Data.ByteString as B
import Data.Word( Word16, Word32 )

data Endianness
  = EndianLittle
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

data TiffPlanarConfiguration
  = PlanarConfigContig    -- = 1
  | PlanarConfigSeparate  -- = 2

planarConfgOfConstant :: Word32 -> Get TiffPlanarConfiguration
planarConfgOfConstant 0 = pure PlanarConfigContig
planarConfgOfConstant 1 = pure PlanarConfigContig
planarConfgOfConstant 2 = pure PlanarConfigSeparate
planarConfgOfConstant v = fail $ "Unknown planar constant (" ++ show v ++ ")"

constantToPlaneConfiguration :: TiffPlanarConfiguration -> Word16
constantToPlaneConfiguration PlanarConfigContig = 1
constantToPlaneConfiguration PlanarConfigSeparate = 2

data TiffCompression
  = CompressionNone           -- 1
  | CompressionModifiedRLE    -- 2
  | CompressionLZW            -- 5
  | CompressionJPEG           -- 6
  | CompressionPackBit        -- 32273

data IfdType
  = TypeByte
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
  deriving Show

instance BinaryParam Endianness IfdType where
    getP endianness = getP endianness >>= conv where
      conv :: Word16 -> Get IfdType
      conv v = case v of
        1  -> return TypeByte
        2  -> return TypeAscii
        3  -> return TypeShort
        4  -> return TypeLong
        5  -> return TypeRational
        6  -> return TypeSByte
        7  -> return TypeUndefined
        8  -> return TypeSignedShort
        9  -> return TypeSignedLong
        10 -> return TypeSignedRational
        11 -> return TypeFloat
        12 -> return TypeDouble
        _  -> fail "Invalid TIF directory type"

    putP endianness = putP endianness . conv where
      conv :: IfdType -> Word16
      conv v = case v of
        TypeByte -> 1
        TypeAscii -> 2
        TypeShort -> 3
        TypeLong -> 4
        TypeRational -> 5
        TypeSByte -> 6
        TypeUndefined -> 7
        TypeSignedShort -> 8
        TypeSignedLong -> 9
        TypeSignedRational -> 10
        TypeFloat -> 11
        TypeDouble -> 12

data TiffTag
  = TagPhotometricInterpretation
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
  | TagCopyright

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
tagOfWord16 v = case v of
  255 -> TagSubfileType
  256 -> TagImageWidth
  257 -> TagImageLength
  258 -> TagBitsPerSample
  259 -> TagCompression
  262 -> TagPhotometricInterpretation
  266 -> TagFillOrder
  269 -> TagDocumentName
  270 -> TagImageDescription
  273 -> TagStripOffsets
  274 -> TagOrientation
  277 -> TagSamplesPerPixel
  278 -> TagRowPerStrip
  279 -> TagStripByteCounts
  282 -> TagXResolution
  283 -> TagYResolution
  284 -> TagPlanarConfiguration
  286 -> TagXPosition
  287 -> TagYPosition
  296 -> TagResolutionUnit
  305 -> TagSoftware
  315 -> TagArtist
  320 -> TagColorMap
  322 -> TagTileWidth
  323 -> TagTileLength
  324 -> TagTileOffset
  325 -> TagTileByteCount
  332 -> TagInkSet
  338 -> TagExtraSample
  339 -> TagSampleFormat
  529 -> TagYCbCrCoeff
  512 -> TagJpegProc
  513 -> TagJPEGInterchangeFormat
  514 -> TagJPEGInterchangeFormatLength
  515 -> TagJPEGRestartInterval
  517 -> TagJPEGLosslessPredictors
  518 -> TagJPEGPointTransforms
  519 -> TagJPEGQTables
  520 -> TagJPEGDCTables
  521 -> TagJPEGACTables
  530 -> TagYCbCrSubsampling
  531 -> TagYCbCrPositioning
  532 -> TagReferenceBlackWhite
  33432 -> TagCopyright
  vv -> TagUnknown vv

word16OfTag :: TiffTag -> Word16
word16OfTag t = case t of
  TagSubfileType -> 255
  TagImageWidth -> 256
  TagImageLength -> 257
  TagBitsPerSample -> 258
  TagCompression -> 259
  TagPhotometricInterpretation -> 262
  TagFillOrder -> 266
  TagDocumentName -> 269
  TagImageDescription -> 270
  TagStripOffsets -> 273
  TagOrientation -> 274
  TagSamplesPerPixel -> 277
  TagRowPerStrip -> 278
  TagStripByteCounts -> 279
  TagXResolution -> 282
  TagYResolution -> 283
  TagPlanarConfiguration -> 284
  TagXPosition -> 286
  TagYPosition -> 287
  TagResolutionUnit -> 296
  TagSoftware -> 305
  TagArtist -> 315
  TagColorMap -> 320
  TagTileWidth -> 322
  TagTileLength -> 323
  TagTileOffset -> 324
  TagTileByteCount -> 325
  TagInkSet -> 332
  TagExtraSample -> 338
  TagSampleFormat -> 339
  TagYCbCrCoeff -> 529
  TagJpegProc -> 512
  TagJPEGInterchangeFormat -> 513
  TagJPEGInterchangeFormatLength -> 514
  TagJPEGRestartInterval -> 515
  TagJPEGLosslessPredictors -> 517
  TagJPEGPointTransforms -> 518
  TagJPEGQTables -> 519
  TagJPEGDCTables -> 520
  TagJPEGACTables -> 521
  TagYCbCrSubsampling -> 530
  TagYCbCrPositioning -> 531
  TagReferenceBlackWhite -> 532
  TagCopyright -> 33432
  (TagUnknown v) -> v

instance BinaryParam Endianness TiffTag where
  getP endianness = tagOfWord16 <$> getP endianness
  putP endianness = putP endianness . word16OfTag

data ExtendedDirectoryData
  = ExtendedDataNone
  | ExtendedDataAscii    !B.ByteString
  | ExtendedDataShort    !(V.Vector Word16)
  | ExtendedDataLong     !(V.Vector Word32)
  | ExtendedDataRational !Word32 !Word32
  deriving (Eq, Show)

instance BinaryParam (Endianness, ImageFileDirectory) ExtendedDirectoryData where
  putP (endianness, _) = dump
    where
      dump ExtendedDataNone = pure ()
      dump (ExtendedDataAscii bstr) = putByteString bstr
      dump (ExtendedDataShort shorts) = V.mapM_ (putP endianness) shorts
      dump (ExtendedDataLong longs) = V.mapM_ (putP endianness) longs
      dump (ExtendedDataRational a b) = putP endianness a >> putP endianness b

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
      fetcher ImageFileDirectory { ifdType = TypeRational, ifdCount = 1 } = do
          align ifd
          ExtendedDataRational <$> getP EndianLittle <*> getP EndianLittle
      fetcher ImageFileDirectory { ifdType = TypeShort, ifdCount = count } | count > 2 =
          align ifd >> (ExtendedDataShort <$> getVec count getE)
      fetcher ImageFileDirectory { ifdType = TypeLong, ifdCount = count } | count > 1 =
          align ifd >> (ExtendedDataLong <$> getVec count getE)
      fetcher _ = pure ExtendedDataNone

data TiffSampleFormat
  = TiffSampleUint
  | TiffSampleInt
  | TiffSampleDouble
  | TiffSampleUnknown
  deriving Eq

unpackSampleFormat :: Word32 -> Get TiffSampleFormat
unpackSampleFormat v = case v of
  1 -> pure TiffSampleUint
  2 -> pure TiffSampleInt
  3 -> pure TiffSampleDouble
  4 -> pure TiffSampleUnknown
  vv -> fail $ "Undefined data format (" ++ show vv ++ ")"

data ImageFileDirectory = ImageFileDirectory
  { ifdIdentifier :: !TiffTag
  , ifdType       :: !IfdType
  , ifdCount      :: !Word32
  , ifdOffset     :: !Word32
  , ifdExtended   :: !ExtendedDirectoryData
  }
  deriving Show

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

data TiffColorspace
  = TiffMonochromeWhite0 -- ^ 0
  | TiffMonochrome       -- ^ 1
  | TiffRGB              -- ^ 2
  | TiffPaleted          -- ^ 3
  | TiffTransparencyMask -- ^ 4
  | TiffCMYK             -- ^ 5
  | TiffYCbCr            -- ^ 6
  | TiffCIELab           -- ^ 8


packPhotometricInterpretation :: TiffColorspace -> Word16
packPhotometricInterpretation v = case v of
  TiffMonochromeWhite0 -> 0
  TiffMonochrome       -> 1
  TiffRGB              -> 2
  TiffPaleted          -> 3
  TiffTransparencyMask -> 4
  TiffCMYK             -> 5
  TiffYCbCr            -> 6
  TiffCIELab           -> 8

unpackPhotometricInterpretation :: Word32 -> Get TiffColorspace
unpackPhotometricInterpretation v = case v of
  0 -> pure TiffMonochromeWhite0
  1 -> pure TiffMonochrome
  2 -> pure TiffRGB
  3 -> pure TiffPaleted
  4 -> pure TiffTransparencyMask
  5 -> pure TiffCMYK
  6 -> pure TiffYCbCr
  8 -> pure TiffCIELab
  vv -> fail $ "Unrecognized color space " ++ show vv

data ExtraSample
  = ExtraSampleUnspecified       -- ^ 0
  | ExtraSampleAssociatedAlpha   -- ^ 1
  | ExtraSampleUnassociatedAlpha -- ^ 2

codeOfExtraSample :: ExtraSample -> Word16
codeOfExtraSample v = case v of
  ExtraSampleUnspecified -> 0
  ExtraSampleAssociatedAlpha -> 1
  ExtraSampleUnassociatedAlpha -> 2

unPackCompression :: Word32 -> Get TiffCompression
unPackCompression v = case v of
  0 -> pure CompressionNone
  1 -> pure CompressionNone
  2 -> pure CompressionModifiedRLE
  5 -> pure CompressionLZW
  6 -> pure CompressionJPEG
  32773 -> pure CompressionPackBit
  vv -> fail $ "Unknown compression scheme " ++ show vv

packCompression :: TiffCompression -> Word16
packCompression v = case v of
  CompressionNone        -> 1
  CompressionModifiedRLE -> 2
  CompressionLZW         -> 5
  CompressionJPEG        -> 6
  CompressionPackBit     -> 32773

