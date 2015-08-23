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
    , TiffColorspace( .. )
    , TiffSampleFormat( .. )
    , ImageFileDirectory( .. )
    , ExtraSample( .. )
    , Predictor( .. )

    , planarConfgOfConstant
    , constantToPlaneConfiguration
    , unpackSampleFormat
    , word16OfTag
    , unpackPhotometricInterpretation
    , packPhotometricInterpretation
    , codeOfExtraSample
    , unPackCompression
    , packCompression 
    , predictorOfConstant
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
import Data.Function( on )
import Data.List( sortBy, mapAccumL )
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Data.Int( Int32 )
import Data.Word( Word16, Word32 )

import Codec.Picture.Metadata.Exif
{-import Debug.Trace-}

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

instance BinaryParam Endianness Int32 where
  putP en v = putP en $ (fromIntegral v :: Word32)
  getP en = fromIntegral <$> (getP en :: Get Word32) 

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

instance BinaryParam Endianness ExifTag where
  getP endianness = tagOfWord16 <$> getP endianness
  putP endianness = putP endianness . word16OfTag

data Predictor
  = PredictorNone                   -- 1
  | PredictorHorizontalDifferencing -- 2
  deriving Eq

predictorOfConstant :: Word32 -> Get Predictor
predictorOfConstant 1 = pure PredictorNone
predictorOfConstant 2 = pure PredictorHorizontalDifferencing
predictorOfConstant v = fail $ "Unknown predictor (" ++ show v ++ ")"

instance BinaryParam (Endianness, Int, ImageFileDirectory) ExifData where
  putP (endianness, _, _) = dump
    where
      dump ExifNone = pure ()
      dump (ExifLong _) = pure ()
      dump (ExifShort _) = pure ()
      dump (ExifIFD _) = pure ()
      dump (ExifString bstr) = putByteString bstr
      dump (ExifUndefined bstr) = putByteString bstr
      -- wrong if length == 2
      dump (ExifShorts shorts) = V.mapM_ (putP endianness) shorts
      dump (ExifLongs longs) = V.mapM_ (putP endianness) longs
      dump (ExifRational a b) = putP endianness a >> putP endianness b
      dump (ExifSignedRational a b) = putP endianness a >> putP endianness b

  getP (endianness, maxi, ifd) = fetcher ifd
    where
      align ImageFileDirectory { ifdOffset = offset } act = do
        readed <- bytesRead
        let delta = fromIntegral offset - readed
        if offset >= fromIntegral maxi || fromIntegral readed > offset then
          pure ExifNone
        else do
          skip $ fromIntegral delta
          act

      getE :: (BinaryParam Endianness a) => Get a
      getE = getP endianness

      getVec count = V.replicateM (fromIntegral count)

      fetcher ImageFileDirectory { ifdIdentifier = TagExifOffset
                                 , ifdType = TypeLong
                                 , ifdCount = 1 } = do
         align ifd $ do
            let byOffset = sortBy (compare `on` ifdOffset)
                cleansIfds = fmap (cleanImageFileDirectory endianness)
            subIfds <- cleansIfds . byOffset <$> getP endianness
            cleaned <- fetchExtended endianness maxi $ sortBy (compare `on` ifdOffset) subIfds
            pure $ ExifIFD [(ifdIdentifier fd, ifdExtended fd) | fd <- cleaned]
         {-  
      fetcher ImageFileDirectory { ifdIdentifier = TagGPSInfo
                                 , ifdType = TypeLong
                                 , ifdCount = 1 } = do
         align ifd 
         subIfds <- fmap (cleanImageFileDirectory endianness) <$> getP endianness
         cleaned <- fetchExtended endianness subIfds
         pure $ ExifIFD [(ifdIdentifier fd, ifdExtended fd) | fd <- cleaned]
        -}
      fetcher ImageFileDirectory { ifdType = TypeUndefined, ifdCount = count } | count > 4 =
         align ifd $ ExifUndefined <$> getByteString (fromIntegral count)
      fetcher ImageFileDirectory { ifdType = TypeUndefined, ifdOffset = ofs } =
          pure . ExifUndefined . B.pack $ take (fromIntegral $ ifdCount ifd)
              [fromIntegral $ ofs .&. 0xFF000000 `unsafeShiftR` (3 * 8)
              ,fromIntegral $ ofs .&. 0x00FF0000 `unsafeShiftR` (2 * 8)
              ,fromIntegral $ ofs .&. 0x0000FF00 `unsafeShiftR` (1 * 8)
              ,fromIntegral $ ofs .&. 0x000000FF
              ]
      fetcher ImageFileDirectory { ifdType = TypeAscii, ifdCount = count } | count > 1 =
          align ifd $ ExifString <$> getByteString (fromIntegral count)
      fetcher ImageFileDirectory { ifdType = TypeShort, ifdCount = 2, ifdOffset = ofs } =
          pure . ExifShorts $ V.fromListN 2 valList
            where high = fromIntegral $ ofs `unsafeShiftR` 16
                  low = fromIntegral $ ofs .&. 0xFFFF
                  valList = case endianness of
                    EndianLittle -> [low, high]
                    EndianBig -> [high, low]
      fetcher ImageFileDirectory { ifdType = TypeRational, ifdCount = 1 } = do
          align ifd $ ExifRational <$> getP EndianLittle <*> getP EndianLittle
      fetcher ImageFileDirectory { ifdType = TypeSignedRational, ifdCount = 1 } = do
          align ifd $ ExifSignedRational <$> getP EndianLittle <*> getP EndianLittle
      fetcher ImageFileDirectory { ifdType = TypeShort, ifdCount = 1 } =
          pure . ExifShort . fromIntegral $ ifdOffset ifd
      fetcher ImageFileDirectory { ifdType = TypeShort, ifdCount = count } | count > 2 =
          align ifd $ ExifShorts <$> getVec count getE
      fetcher ImageFileDirectory { ifdType = TypeLong, ifdCount = 1 } =
          pure . ExifLong . fromIntegral $ ifdOffset ifd
      fetcher ImageFileDirectory { ifdType = TypeLong, ifdCount = count } | count > 1 =
          align ifd $ ExifLongs <$> getVec count getE
      fetcher _ = pure ExifNone

cleanImageFileDirectory :: Endianness -> ImageFileDirectory -> ImageFileDirectory
cleanImageFileDirectory EndianBig ifd@(ImageFileDirectory { ifdCount = 1 }) = aux $ ifdType ifd
  where
    aux TypeShort = ifd { ifdOffset = ifdOffset ifd `unsafeShiftR` 16 }
    aux _ = ifd
cleanImageFileDirectory _ ifd = ifd

fetchExtended :: Endianness -> Int -> [ImageFileDirectory] -> Get [ImageFileDirectory]
fetchExtended endian maxi = mapM $ \ifd -> do
  v <- getP (endian, maxi, ifd)
  pure $ ifd { ifdExtended = v }

-- | All the IFD must be written in order according to the tag
-- value of the IFD. To avoid getting to much restriction in the
-- serialization code, just sort it.
orderIfdByTag :: [ImageFileDirectory] -> [ImageFileDirectory]
orderIfdByTag = sortBy comparer where
  comparer a b = compare t1 t2 where
    t1 = word16OfTag $ ifdIdentifier a
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

        updater ix ifd@(ImageFileDirectory { ifdExtended = ExifString b }) =
            (ix + fromIntegral (B.length b), ifd { ifdOffset = ix } )
        updater ix ifd@(ImageFileDirectory { ifdExtended = ExifLongs v })
            | V.length v > 1 = ( ix + fromIntegral (V.length v * 4)
                               , ifd { ifdOffset = ix } )
        updater ix ifd@(ImageFileDirectory { ifdExtended = ExifShorts v })
            | V.length v > 2 = ( ix + fromIntegral (V.length v * 2)
                             , ifd { ifdOffset = ix })
        updater ix ifd = (ix, ifd)

instance BinaryParam B.ByteString (TiffHeader, [ImageFileDirectory]) where
  putP rawData (hdr, ifds) = do
    put hdr
    putByteString rawData
    let endianness = hdrEndianness hdr
        list = setupIfdOffsets (hdrOffset hdr) $ orderIfdByTag ifds
    putP endianness list
    mapM_ (\ifd -> putP (endianness, (0::Int), ifd) $ ifdExtended ifd) list

  getP raw = do
    hdr <- get
    readed <- bytesRead
    skip . fromIntegral $ fromIntegral (hdrOffset hdr) - readed
    let endian = hdrEndianness hdr
        byOffset = sortBy (compare `on` ifdOffset)
        cleanIfds = fmap (cleanImageFileDirectory endian)

    ifd <- cleanIfds . byOffset <$> getP endian
    cleaned <- fetchExtended endian (B.length raw) ifd
    return (hdr, cleaned)

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
  { ifdIdentifier :: !ExifTag
  , ifdType       :: !IfdType
  , ifdCount      :: !Word32
  , ifdOffset     :: !Word32
  , ifdExtended   :: !ExifData
  }
  deriving Show

instance BinaryParam Endianness ImageFileDirectory where
  getP endianness =
    ImageFileDirectory <$> getE <*> getE <*> getE <*> getE
                       <*> pure ExifNone
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

