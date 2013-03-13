{-# LANGUAGE TupleSections #-}
module Codec.Picture.Tiff where

import Control.Applicative( (<$>), (<*>), pure )
import Control.Monad( when, replicateM )
import Data.Word( Word16 )
import Data.Bits( unsafeShiftR )
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
                      , putWord32le, putWord32be )

import qualified Data.Vector as V
import Data.Word( Word32 )
import qualified Data.ByteString as B

import Text.Groom
import Codec.Picture.InternalHelper
import Codec.Picture.Types

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

data TiffHeader = TiffHeader
    { hdrEndianness :: !Endianness
    , hdrOffset     :: {-# UNPACK #-} !Word32
    }
    deriving (Eq, Show)

putWord16Endian :: Endianness -> Word16 -> Put
putWord16Endian EndianLittle = putWord16le
putWord16Endian EndianBig = putWord16be

getWord16Endian :: Endianness -> Get Word16
getWord16Endian EndianLittle = getWord16le
getWord16Endian EndianBig = getWord16be

putWord32Endian :: Endianness -> Word32 -> Put
putWord32Endian EndianLittle = putWord32le
putWord32Endian EndianBig = putWord32be

getWord32Endian :: Endianness -> Get Word32
getWord32Endian EndianLittle = getWord32le
getWord32Endian EndianBig = getWord32be

instance Binary TiffHeader where
    put hdr = do
        let endian = hdrEndianness hdr
        put endian
        putWord16Endian endian 42
        putWord32Endian endian $ hdrOffset hdr

    get = do
        endian <- get
        magic <- getWord16Endian endian
        when (magic /= 42)
             (fail "Invalid TIFF magic number")
        TiffHeader endian <$> getWord32Endian endian

data TiffCompression =
      CompressionNone           -- 1
    | CompressionModifiedRLE    -- 2
    | CompressionPackBit        -- 32273
    deriving (Eq, Show)

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
             deriving (Eq, Show)

wordOfType :: IfdType -> Word16
wordOfType TypeByte           = 1
wordOfType TypeAscii          = 2
wordOfType TypeShort          = 3
wordOfType TypeLong           = 4
wordOfType TypeRational       = 5
wordOfType TypeSByte          = 6
wordOfType TypeUndefined      = 7
wordOfType TypeSignedShort    = 8
wordOfType TypeSignedLong     = 9
wordOfType TypeSignedRational = 10
wordOfType TypeFloat          = 11
wordOfType TypeDouble         = 12

typeOfWord :: Word16 -> Get IfdType
typeOfWord 1  = return TypeByte
typeOfWord 2  = return TypeAscii
typeOfWord 3  = return TypeShort
typeOfWord 4  = return TypeLong
typeOfWord 5  = return TypeRational
typeOfWord 6  = return TypeSByte
typeOfWord 7  = return TypeUndefined
typeOfWord 8  = return TypeSignedShort
typeOfWord 9  = return TypeSignedLong
typeOfWord 10 = return TypeSignedRational
typeOfWord 11 = return TypeFloat
typeOfWord 12 = return TypeDouble
typeOfWord _  = fail "Invalid TIF directory type"

data TiffTag = TagPhotometricInterpretation
             | TagCompression
             | TagImageWidth
             | TagImageLength
             | TagXResolution
             | TagYResolution
             | TagResolutionUnit
             | TagRowPerStrip
             | TagStripByteCounts
             | TagStripOffsets
             | TagBitsPerSample
             | TagColorMap
             | TagSamplesPerPixel
             | TagArtist
             | TagDocumentName
             | TagSoftware
             | TagPlanarConfiguration
             | TagOrientation
             | TagUnknown Word16
             deriving (Eq, Show)

tagOfWord16 :: Word16 -> TiffTag
tagOfWord16 = aux
  where 
        aux 256 = TagImageWidth
        aux 257 = TagImageLength
        aux 258 = TagBitsPerSample
        aux 259 = TagCompression
        aux 262 = TagPhotometricInterpretation
        aux 269 = TagDocumentName
        aux 273 = TagStripOffsets
        aux 274 = TagOrientation
        aux 277 = TagSamplesPerPixel
        aux 278 = TagRowPerStrip
        aux 279 = TagStripByteCounts
        aux 282 = TagXResolution
        aux 283 = TagYResolution
        aux 284 = TagPlanarConfiguration
        aux 296 = TagResolutionUnit
        aux 305 = TagSoftware
        aux 315 = TagArtist
        aux 320 = TagColorMap
        aux v = TagUnknown v

data ExtendedDirectoryData =
      ExtendedDataNone
    | ExtendedDataAscii !B.ByteString
    | ExtendedDataShort !(V.Vector Word16)
    | ExtendedDataLong  !(V.Vector Word32)
    deriving (Eq, Show)

data ImageFileDirectory = ImageFileDirectory
    { ifdIdentifier :: !TiffTag
    , ifdType       :: !IfdType
    , ifdCount      :: !Word32
    , ifdOffset     :: !Word32
    , ifdExtended   :: !ExtendedDirectoryData
    }
    deriving (Eq, Show)

unLong :: String -> ExtendedDirectoryData -> Get (V.Vector Word32)
unLong _ (ExtendedDataShort v) = pure $ V.map fromIntegral v
unLong _ (ExtendedDataLong v) = pure v
unLong errMessage _ = fail errMessage

cleanImageFileDirectory :: ImageFileDirectory -> ImageFileDirectory
cleanImageFileDirectory ifd@(ImageFileDirectory { ifdCount = 1 }) = aux $ ifdType ifd
    where aux TypeShort = ifd { ifdOffset = ifdOffset ifd `unsafeShiftR` 16 }
          aux _ = ifd
cleanImageFileDirectory ifd = ifd

getImageFileDirectory :: Endianness -> Get ImageFileDirectory
getImageFileDirectory endianness =
  ImageFileDirectory <$> (tagOfWord16 <$> getWord16)
                     <*> (getWord16 >>= typeOfWord)
                     <*> getWord32
                     <*> getWord32
                     <*> pure ExtendedDataNone
    where getWord16 = getWord16Endian endianness
          getWord32 = getWord32Endian endianness

getImageFileDirectories :: Endianness -> Get [ImageFileDirectory]
getImageFileDirectories endianness = do
    count <- getWord16
    replicateM (fromIntegral count) (getImageFileDirectory endianness)
   where getWord16 = getWord16Endian endianness

fetchExtended :: Endianness -> [ImageFileDirectory] -> Get [ImageFileDirectory]
fetchExtended endian = mapM fetcher
  where align ImageFileDirectory { ifdOffset = offset } = do
            readed <- bytesRead
            skip . fromIntegral $ fromIntegral offset - readed

        getWord16 = getWord16Endian endian
        getWord32 = getWord32Endian endian

        update ifd v = ifd { ifdExtended = v }
        getVec count = V.replicateM (fromIntegral count)

        fetcher ifd@ImageFileDirectory { ifdType = TypeAscii, ifdCount = count } | count > 1 =
            align ifd >> (update ifd . ExtendedDataAscii <$> getByteString (fromIntegral count))
        fetcher ifd@ImageFileDirectory { ifdType = TypeShort, ifdCount = count } | count > 1 =
            align ifd >> (update ifd . ExtendedDataShort <$> getVec count getWord16)
        fetcher ifd@ImageFileDirectory { ifdType = TypeLong, ifdCount = count } | count > 1 =
            align ifd >> (update ifd . ExtendedDataLong <$> getVec count getWord32)

        fetcher ifd = pure ifd

findIFD :: String -> TiffTag -> [ImageFileDirectory]
        -> Get ImageFileDirectory
findIFD errorMessage tag lst =
    case [v | v <- lst, ifdIdentifier v == tag] of
        [] -> fail errorMessage
        (x:_) -> pure x

findIFDData :: String -> TiffTag -> [ImageFileDirectory] -> Get Word32
findIFDData msg tag lst = ifdOffset <$> findIFD msg tag lst

findIFDExt :: String -> TiffTag -> [ImageFileDirectory] -> Get ExtendedDirectoryData
findIFDExt msg tag lst = ifdExtended <$> findIFD msg tag lst

data TiffInfo = TiffInfo
    { tiffHeader        :: TiffHeader
    , tiffWidth         :: Word32
    , tiffHeight        :: Word32
    , tiffSampleCount   :: Word32
    , tiffBitsPerSample :: V.Vector Word32
    , tiffCompression   :: TiffCompression
    , tiffStripSize     :: V.Vector Word32
    , tiffOffsets       :: V.Vector Word32
    }
    deriving (Eq, Show)

unPackCompression :: Word32 -> Get TiffCompression
unPackCompression 1 = pure CompressionNone
unPackCompression 2 = pure CompressionModifiedRLE
unPackCompression 32273 = pure CompressionPackBit
unPackCompression _ = fail "Unknown compression scheme"

getTiffInfo :: Get TiffInfo
getTiffInfo = do
    hdr <- get
    readed <- bytesRead
    skip . fromIntegral $ fromIntegral (hdrOffset hdr) - readed
    let endian = hdrEndianness hdr

    ifd <- fmap cleanImageFileDirectory <$> getImageFileDirectories endian
    cleaned <- fetchExtended endian ifd

    let dataFind str tag = findIFDData str tag cleaned
        extFind str tag = findIFDExt str tag cleaned

    TiffInfo hdr
        <$> dataFind "Can't find width" TagImageWidth
        <*> dataFind "Can't find height" TagImageLength
        <*> dataFind "Can't find sample per pixel" TagSamplesPerPixel
        <*> (extFind "Can't find bit per sample" TagBitsPerSample
                     >>= unLong "Can't find bit depth")
        <*> (dataFind "Can't find Compression" TagCompression
                     >>= unPackCompression)
        <*> (extFind "Can't find byte counts" TagStripByteCounts
                     >>= unLong "Can't find bit per sample")
        <*> (extFind "Strip offsets missing" TagStripOffsets
                     >>= unLong "Can't find strip offsets")

unpack :: B.ByteString -> TiffInfo -> Either String DynamicImage
unpack file _ = Left "Failure to unpack TIFF file"

decodeTiff :: B.ByteString -> Either String DynamicImage
decodeTiff file = runGetStrict getTiffInfo file >>= unpack file

tiffDebug :: B.ByteString -> IO ()
tiffDebug tiffFile = putStrLn $ groom tiffInfo
  where tiffInfo = flip runGetStrict tiffFile getTiffInfo
