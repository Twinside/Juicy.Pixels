{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
module Codec.Picture.Tiff( decodeTiff ) where

import Control.Applicative( (<$>), (<*>), pure )
import Control.Monad( when, replicateM )
import Control.Monad.ST( ST, runST )
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
import Data.Binary.Put( Put
                      , putWord16le, putWord16be
                      , putWord32le, putWord32be )

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as M
import Data.Word( Word32 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU

import Debug.Trace
import Text.Groom

import Codec.Picture.InternalHelper
import Codec.Picture.BitWriter
import Codec.Picture.Types
import Codec.Picture.Gif.LZW

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

data TiffPlanarConfiguration =
      PlanarConfigContig    -- = 1
    | PlanarConfigSeparate  -- = 2
    deriving (Eq, Show)

planarConfgOfConstant :: Word32 -> Get TiffPlanarConfiguration
planarConfgOfConstant 0 = pure PlanarConfigContig
planarConfgOfConstant 1 = pure PlanarConfigContig
planarConfgOfConstant 2 = pure PlanarConfigSeparate
planarConfgOfConstant v = fail $ "Unknown planar constant (" ++ show v ++ ")"

data TiffCompression =
      CompressionNone           -- 1
    | CompressionModifiedRLE    -- 2
    | CompressionLZW            -- 5
    | CompressionJPEG           -- 6
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

{-
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
 -- -}

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
             | TagSampleFormat
             | TagInkSet
             | TagSubfileType
             | TagFillOrder
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
        aux 332 = TagInkSet
        aux 339 = TagSampleFormat
        aux v = TagUnknown v

data ExtendedDirectoryData =
      ExtendedDataNone
    | ExtendedDataAscii !B.ByteString
    | ExtendedDataShort !(V.Vector Word16)
    | ExtendedDataLong  !(V.Vector Word32)
    deriving (Eq, Show)

data TiffSampleFormat =
      TiffSampleUint
    | TiffSampleInt
    | TiffSampleDouble
    | TiffSampleUnknown
    deriving (Eq, Show)

unpackSampleFormat :: Word32 -> Get TiffSampleFormat
unpackSampleFormat = aux
  where aux 1 = pure TiffSampleUint
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
        { ifdCount = 1, ifdOffset = ofs } ->
               pure . ExtendedDataShort . V.singleton $ fromIntegral ofs
      ImageFileDirectory { ifdExtended = v } -> pure v


findIFDExtDefaultData :: [Word32] -> TiffTag -> [ImageFileDirectory] -> Get [Word32]
findIFDExtDefaultData d tag lst =
    case [v | v <- lst, ifdIdentifier v == tag] of
        [] -> pure d
        (x:_) -> V.toList <$> unLong "Can't unlong" (ifdExtended x)

-- It's temporary, remove once tiff decoding is better
-- handled.
instance Show (Image PixelRGB16) where
    show _ = "Image PixelRGB16"

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
    }
    deriving Show

data TiffColorspace =
      TiffMonochromeWhite0 -- ^ 0
    | TiffMonochrome       -- ^ 1
    | TiffRGB              -- ^ 2
    | TiffPaleted          -- ^ 3
    | TiffTransparencyMask -- ^ 4
    | TiffCMYK             -- ^ 5
    | TiffYCbCr            -- ^ 6
    | TiffCIELab           -- ^ 8
    deriving (Eq, Show)

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
        aux _ = fail "Unrecognized color space"

unPackCompression :: Word32 -> Get TiffCompression
unPackCompression 0 = pure CompressionNone
unPackCompression 1 = pure CompressionNone
unPackCompression 2 = pure CompressionModifiedRLE
unPackCompression 5 = pure CompressionLZW
unPackCompression 6 = pure CompressionJPEG
unPackCompression 32773 = pure CompressionPackBit
unPackCompression v = fail $ "Unknown compression scheme " ++ show v

copyByteString :: B.ByteString -> M.STVector s Word8 -> Int -> Int -> (Word32, Word32)
               -> ST s Int
copyByteString str vec stride startWrite (from, count) = inner startWrite fromi
  where fromi = fromIntegral from
        maxi = fromi + fromIntegral count

        inner writeIdx i | i >= maxi = pure writeIdx
        inner writeIdx i = do
            let v = str `BU.unsafeIndex` i
            {-(vec `M.unsafeWrite` writeIdx) v-}
            (vec `M.write` writeIdx) v
            inner (writeIdx + stride) $ i + 1

unpackPackBit :: B.ByteString -> M.STVector s Word8 -> Int -> Int
              -> (Word32, Word32)
              -> ST s Int
unpackPackBit str outVec stride writeIndex (offset, size) = loop fromi writeIndex
  where fromi = fromIntegral offset
        maxi = fromi + fromIntegral size

        replicateByte writeIdx _     0 = pure writeIdx
        replicateByte writeIdx v count = do
            (outVec `M.write` writeIdx) v
            replicateByte (writeIdx + stride) v $ count - 1

        loop i writeIdx | i >= maxi = pure writeIdx
        loop i writeIdx = choice
          where v = fromIntegral (str `BU.unsafeIndex` i) :: Int8

                choice
                    -- data
                    | 0    <= v = do
                        copyByteString str outVec stride writeIdx
                                        (fromIntegral $ i + 1, fromIntegral v + 1)
                            >>= loop (i + 2 + fromIntegral v)
                    -- run
                    | -127 <= v = do
                        let nextByte = str `BU.unsafeIndex` (i + 1)
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
  outAlloc _ = M.new

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

getTiffInfo :: Get TiffInfo
getTiffInfo = do
    hdr <- get
    readed <- bytesRead
    skip . fromIntegral $ fromIntegral (hdrOffset hdr) - readed
    let endian = hdrEndianness hdr

    ifd <- fmap cleanImageFileDirectory <$> getImageFileDirectories endian
    cleaned <- (\a -> trace (groom a) a) <$> fetchExtended endian ifd

    let dataFind str tag = findIFDData str tag cleaned
        dataDefault def tag = findIFDDefaultData def tag cleaned
        extFind str tag = findIFDExt str tag cleaned
        extDefault def tag = findIFDExtDefaultData def tag cleaned

    (\a -> trace (groom a) a) <$> (TiffInfo hdr
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
            )

unpack :: B.ByteString -> TiffInfo -> Either String DynamicImage
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

unpack file nfo@TiffInfo { tiffColorspace = TiffMonochromeWhite0 } = do
    img <- unpack file (nfo { tiffColorspace = TiffMonochrome })
    case img of
      ImageY8 i -> pure . ImageY8 $ pixelMap (maxBound -) i
      ImageY16 i -> pure . ImageY16 $ pixelMap (maxBound -) i
      _ -> fail "Unexpected decoded image"

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

unpack file nfo@TiffInfo { tiffColorspace = TiffRGB
                         , tiffBitsPerSample = lst
                         , tiffSampleFormat = format }
  | lst == V.fromList [2, 2, 2] && all (TiffSampleUint ==) format =
        pure . ImageRGB8 . pixelMap (colorMap (64 *)) $ gatherStrips Pack2 file nfo
  | lst == V.fromList [4, 4, 4] && all (TiffSampleUint ==) format =
        pure . ImageRGB8 . pixelMap (colorMap (16 *)) $ gatherStrips Pack4 file nfo
  | lst == V.fromList [8, 8, 8] && all (TiffSampleUint ==) format =
        pure . ImageRGB8 $ gatherStrips (0 :: Word8) file nfo
  | lst == V.fromList [16, 16, 16] && all (TiffSampleUint ==) format =
        pure . ImageRGB16 $ gatherStrips (0 :: Word16) file nfo
unpack file nfo@TiffInfo { tiffColorspace = TiffMonochrome
                         , tiffBitsPerSample = lst
                         , tiffSampleFormat = format }
  -- some files are a little bit borked...
  | lst == V.fromList [8, 8, 8] && all (TiffSampleUint ==) format =
        pure . ImageRGB8 $ gatherStrips (0 :: Word8) file nfo

unpack _ tiffInfo = trace (groom tiffInfo) $ fail "Failure to unpack TIFF file"

-- | Transform a raw tiff image to an image, without modifying
-- the underlying pixel type.
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
decodeTiff file = runGetStrict getTiffInfo file >>= unpack file

