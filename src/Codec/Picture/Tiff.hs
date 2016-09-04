{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-- | Module implementing TIFF decoding.
--
-- Supported compression schemes:
--
--   * Uncompressed
--
--   * PackBits
--
--   * LZW
--
-- Supported bit depth:
--
--   * 2 bits
--
--   * 4 bits
--
--   * 8 bits
--
--   * 16 bits
--
module Codec.Picture.Tiff( decodeTiff
                         , decodeTiffWithMetadata
                         , decodeTiffWithPaletteAndMetadata
                         , TiffSaveable
                         , encodeTiff
                         , writeTiff
                         ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>), (<*>), pure )
import Data.Monoid( mempty )
#endif

import Control.Arrow( first )
import Control.Monad( when, foldM_, unless, forM_ )
import Control.Monad.ST( ST, runST )
import Control.Monad.Writer.Strict( execWriter, tell, Writer )
import Data.Int( Int8 )
import Data.Word( Word8, Word16, Word32 )
import Data.Bits( (.&.), (.|.), unsafeShiftL, unsafeShiftR )
import Data.Binary.Get( Get )
import Data.Binary.Put( runPut )

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lb
import qualified Data.ByteString.Unsafe as BU

import Foreign.Storable( sizeOf )

import Codec.Picture.Metadata.Exif
import Codec.Picture.Metadata( Metadatas )
import Codec.Picture.InternalHelper
import Codec.Picture.BitWriter
import Codec.Picture.Types
import Codec.Picture.Gif.LZW
import Codec.Picture.Tiff.Types
import Codec.Picture.Tiff.Metadata
import Codec.Picture.VectorByteConversion( toByteString )

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
  , tiffExtraSample        :: Maybe ExtraSample
  , tiffPredictor          :: Predictor
  , tiffMetadatas          :: Metadatas
  }

unLong :: String -> ExifData -> Get (V.Vector Word32)
unLong _ (ExifShorts v) = pure $ V.map fromIntegral v
unLong _ (ExifLongs v) = pure v
unLong errMessage _ = fail errMessage

findIFD :: String -> ExifTag -> [ImageFileDirectory]
        -> Get ImageFileDirectory
findIFD errorMessage tag lst =
  case [v | v <- lst, ifdIdentifier v == tag] of
    [] -> fail errorMessage
    (x:_) -> pure x

findPalette :: [ImageFileDirectory] -> Get (Maybe (Image PixelRGB16))
findPalette ifds =
    case [v | v <- ifds, ifdIdentifier v == TagColorMap] of
        (ImageFileDirectory { ifdExtended = ExifShorts vec }:_) ->
            pure . Just . Image pixelCount 1 $ VS.generate (V.length vec) axx
                where pixelCount = V.length vec `div` 3
                      axx v = vec `V.unsafeIndex` (idx + color * pixelCount)
                          where (idx, color) = v `divMod` 3

        _ -> pure Nothing

findIFDData :: String -> ExifTag -> [ImageFileDirectory] -> Get Word32
findIFDData msg tag lst = ifdOffset <$> findIFD msg tag lst

findIFDDefaultData :: Word32 -> ExifTag -> [ImageFileDirectory] -> Get Word32
findIFDDefaultData d tag lst =
    case [v | v <- lst, ifdIdentifier v == tag] of
        [] -> pure d
        (x:_) -> pure $ ifdOffset x

findIFDExt :: String -> ExifTag -> [ImageFileDirectory] -> Get ExifData
findIFDExt msg tag lst = do
    val <- findIFD msg tag lst
    case val of
      ImageFileDirectory
        { ifdCount = 1, ifdOffset = ofs, ifdType = TypeShort } ->
               pure . ExifShorts . V.singleton $ fromIntegral ofs
      ImageFileDirectory
        { ifdCount = 1, ifdOffset = ofs, ifdType = TypeLong } ->
               pure . ExifLongs  . V.singleton $ fromIntegral ofs
      ImageFileDirectory { ifdExtended = v } -> pure v


findIFDExtDefaultData :: [Word32] -> ExifTag -> [ImageFileDirectory]
                      -> Get [Word32]
findIFDExtDefaultData d tag lst =
    case [v | v <- lst, ifdIdentifier v == tag] of
        [] -> pure d
        (ImageFileDirectory { ifdExtended = ExifNone }:_) -> return d
        (x:_) -> V.toList <$> unLong errorMessage (ifdExtended x)
            where errorMessage =
                    "Can't parse tag " ++ show tag ++ " " ++ show (ifdExtended x)

-- It's temporary, remove once tiff decoding is better
-- handled.
{-  
instance Show (Image PixelRGB16) where
    show _ = "Image PixelRGB16"
-}
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
                    | 0    <= v =
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
                        -> Int  -- ^ Line size in pixels
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
        where unpacker (idx, stripSampleCount, offset, packedSize) = do
                  let (writeIdx, tempStride)  = offsetStride comp idx 1
                  _ <- uncompressAt compression str tempVec tempStride
                                    writeIdx (offset, packedSize)
                  let typ :: M.MVector s a -> a
                      typ = const undefined
                      sampleSize = sizeOf (typ outVec)
                  mergeBackTempBuffer comp endianness tempVec (width * sampleCount)
                                      idx (fromIntegral $ stripSampleCount * sampleSize) 1 outVec


              fullStripSampleCount = rowPerStrip * width * sampleCount
              startWriteOffset = V.generate stripCount (fullStripSampleCount *)
              stripSampleCounts = V.map strip startWriteOffset
                  where
                      strip start = min fullStripSampleCount (width * height * sampleCount - start)

              sizes = V.zip4 startWriteOffset stripSampleCounts
                             (tiffOffsets nfo) (tiffStripSize nfo)

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

  when (tiffPredictor nfo == PredictorHorizontalDifferencing) $ do
    let f _ c1 c2 = c1 + c2
    forM_ [0 .. height - 1] $ \y ->
      forM_ [1 .. width - 1] $ \x -> do
        p <- readPixel mutableImage (x - 1) y
        q <- readPixel mutableImage x y
        writePixel mutableImage x y $ mixWith f p q

  unsafeFreezeImage mutableImage

ifdSingleLong :: ExifTag -> Word32 -> Writer [ImageFileDirectory] ()
ifdSingleLong tag = ifdMultiLong tag . V.singleton

ifdSingleShort :: Endianness -> ExifTag -> Word16
               -> Writer [ImageFileDirectory] ()
ifdSingleShort endian tag = ifdMultiShort endian tag . V.singleton . fromIntegral

ifdMultiLong :: ExifTag -> V.Vector Word32 -> Writer [ImageFileDirectory] ()
ifdMultiLong tag v = tell . pure $ ImageFileDirectory
        { ifdIdentifier = tag
        , ifdType       = TypeLong
        , ifdCount      = fromIntegral $ V.length v
        , ifdOffset     = offset
        , ifdExtended   = extended
        }
  where (offset, extended)
                | V.length v > 1 = (0, ExifLongs v)
                | otherwise = (V.head v, ExifNone)

ifdMultiShort :: Endianness -> ExifTag -> V.Vector Word32
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
                | size > 2 = (0, ExifShorts $ V.map fromIntegral v)
                | size == 2 =
                    let v1 = fromIntegral $ V.head v
                        v2 = fromIntegral $ v `V.unsafeIndex` 1
                    in
                    case endian of
                      EndianLittle -> (v2 `unsafeShiftL` 16 .|. v1, ExifNone)
                      EndianBig -> (v1 `unsafeShiftL` 16 .|. v2, ExifNone)

                | otherwise = case endian of
                    EndianLittle -> (V.head v, ExifNone)
                    EndianBig -> (V.head v `unsafeShiftL` 16, ExifNone)

instance BinaryParam B.ByteString TiffInfo where
  putP rawData nfo = putP rawData (tiffHeader nfo, list) where
    endianness = hdrEndianness $ tiffHeader nfo

    ifdShort = ifdSingleShort endianness
    ifdShorts = ifdMultiShort endianness

    list = execWriter $ do
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

      maybe (return ())
            (ifdShort TagExtraSample . codeOfExtraSample)
          $ tiffExtraSample nfo

      let subSampling = tiffYCbCrSubsampling nfo
      unless (V.null subSampling) $
           ifdShorts TagYCbCrSubsampling subSampling

  getP rawData = do
    (hdr, cleaned) <- getP rawData

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
        <*> pure Nothing
        <*> (dataDefault 1 TagPredictor
                     >>= predictorOfConstant)
        <*> pure (extractTiffMetadata cleaned)

palette16Of :: Image PixelRGB16 -> Palette' PixelRGB16
palette16Of p = Palette'
    { _paletteSize = imageWidth p
    , _paletteData = imageData p
    }

unpack :: B.ByteString -> TiffInfo -> Either String PalettedImage
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
      pure . PalettedRGB16 (gatherStrips (0 :: Word8) file nfo) $ palette16Of p
  | lst == V.singleton 4 && format == [TiffSampleUint] =
      pure . PalettedRGB16 (gatherStrips Pack4 file nfo) $ palette16Of p
  | lst == V.singleton 2 && format == [TiffSampleUint] =
      pure . PalettedRGB16 (gatherStrips Pack2 file nfo) $ palette16Of p

unpack file nfo@TiffInfo { tiffColorspace = TiffCMYK
                         , tiffBitsPerSample = lst
                         , tiffSampleFormat = format }
  | lst == V.fromList [8, 8, 8, 8] && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageCMYK8 $ gatherStrips (0 :: Word8) file nfo

  | lst == V.fromList [16, 16, 16, 16] && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageCMYK16 $ gatherStrips (0 :: Word16) file nfo

unpack file nfo@TiffInfo { tiffColorspace = TiffMonochromeWhite0 } = do
    img <- unpack file (nfo { tiffColorspace = TiffMonochrome })
    case img of
      TrueColorImage (ImageY8 i) -> pure . TrueColorImage . ImageY8 $ pixelMap (maxBound -) i
      TrueColorImage (ImageY16 i) -> pure . TrueColorImage . ImageY16 $ pixelMap (maxBound -) i
      TrueColorImage (ImageYA8 i) -> let negative (PixelYA8 y a) = PixelYA8 (maxBound - y) a
                    in pure . TrueColorImage . ImageYA8 $ pixelMap negative i
      TrueColorImage (ImageYA16 i) -> let negative (PixelYA16 y a) = PixelYA16 (maxBound - y) a
                     in pure . TrueColorImage . ImageYA16 $ pixelMap negative i
      _ -> fail "Unsupported color type used with colorspace MonochromeWhite0"

unpack file nfo@TiffInfo { tiffColorspace = TiffMonochrome
                         , tiffBitsPerSample = lst
                         , tiffSampleFormat = format }
  | lst == V.singleton 2 && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageY8 . pixelMap (colorMap (0x55 *)) $ gatherStrips Pack2 file nfo
  | lst == V.singleton 4 && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageY8 . pixelMap (colorMap (0x11 *)) $ gatherStrips Pack4 file nfo
  | lst == V.singleton 8 && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageY8 $ gatherStrips (0 :: Word8) file nfo
  | lst == V.singleton 12 && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageY16 . pixelMap (colorMap expand12to16) $ gatherStrips Pack12 file nfo
  | lst == V.singleton 16 && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageY16 $ gatherStrips (0 :: Word16) file nfo
  | lst == V.singleton 32 && all (TiffSampleUint ==) format =
        let toWord16 v = fromIntegral $ v `unsafeShiftR` 16
            img = gatherStrips (0 :: Word32) file nfo :: Image Pixel32
        in
        pure . TrueColorImage . ImageY16 $ pixelMap toWord16 img
  | lst == V.fromList [2, 2] && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageYA8 . pixelMap (colorMap (0x55 *)) $ gatherStrips Pack2 file nfo
  | lst == V.fromList [4, 4] && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageYA8 . pixelMap (colorMap (0x11 *)) $ gatherStrips Pack4 file nfo
  | lst == V.fromList [8, 8] && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageYA8 $ gatherStrips (0 :: Word8) file nfo
  | lst == V.fromList [12, 12] && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageYA16 . pixelMap (colorMap expand12to16) $ gatherStrips Pack12 file nfo
  | lst == V.fromList [16, 16] && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageYA16 $ gatherStrips (0 :: Word16) file nfo
    where
      expand12to16 x = x `unsafeShiftL` 4 + x `unsafeShiftR` (12 - 4)

unpack file nfo@TiffInfo { tiffColorspace = TiffYCbCr
                         , tiffBitsPerSample = lst
                         , tiffPlaneConfiguration = PlanarConfigContig
                         , tiffSampleFormat = format }
  | lst == V.fromList [8, 8, 8] && all (TiffSampleUint ==) format =
    pure . TrueColorImage . ImageYCbCr8 $ gatherStrips cbcrConf  file nfo
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
        pure . TrueColorImage . ImageRGB8 . pixelMap (colorMap (0x55 *)) $ gatherStrips Pack2 file nfo
  | lst == V.fromList [4, 4, 4] && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageRGB8 . pixelMap (colorMap (0x11 *)) $ gatherStrips Pack4 file nfo
  | lst == V.fromList [8, 8, 8] && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageRGB8 $ gatherStrips (0 :: Word8) file nfo
  | lst == V.fromList [8, 8, 8, 8] && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageRGBA8 $ gatherStrips (0 :: Word8) file nfo
  | lst == V.fromList [16, 16, 16] && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageRGB16 $ gatherStrips (0 :: Word16) file nfo
  | lst == V.fromList [16, 16, 16, 16] && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageRGBA16 $ gatherStrips (0 :: Word16) file nfo
unpack file nfo@TiffInfo { tiffColorspace = TiffMonochrome
                         , tiffBitsPerSample = lst
                         , tiffSampleFormat = format }
  -- some files are a little bit borked...
  | lst == V.fromList [8, 8, 8] && all (TiffSampleUint ==) format =
        pure . TrueColorImage . ImageRGB8 $ gatherStrips (0 :: Word8) file nfo

unpack _ _ = fail "Failure to unpack TIFF file"

-- | Decode a tiff encoded image while preserving the underlying
-- pixel type (except for Y32 which is truncated to 16 bits).
--
-- This function can output the following images:
--
--  * 'ImageY8'
--
--  * 'ImageY16'
--
--  * 'ImageYA8'
--
--  * 'ImageYA16'
--
--  * 'ImageRGB8'
--
--  * 'ImageRGB16'
--
--  * 'ImageRGBA8'
--
--  * 'ImageRGBA16'
--
--  * 'ImageCMYK8'
--
--  * 'ImageCMYK16'
--
decodeTiff :: B.ByteString -> Either String DynamicImage
decodeTiff = fmap fst . decodeTiffWithMetadata 

-- | Like 'decodeTiff' but also provides some metdata present
-- in the Tiff file.
--
-- The metadata extracted are the 'Codec.Picture.Metadata.DpiX' &
-- 'Codec.Picture.Metadata.DpiY' information alongside the EXIF informations.
decodeTiffWithMetadata :: B.ByteString -> Either String (DynamicImage, Metadatas)
decodeTiffWithMetadata str = first palettedToTrueColor <$> decodeTiffWithPaletteAndMetadata str

-- | Decode TIFF and provide separated palette and metadata
decodeTiffWithPaletteAndMetadata :: B.ByteString -> Either String (PalettedImage, Metadatas)
decodeTiffWithPaletteAndMetadata file = runGetStrict (getP file) file >>= go
  where
    go tinfo = (, tiffMetadatas tinfo) <$> unpack file tinfo
    

-- | Class defining which pixel types can be serialized in a
-- Tiff file.
class (Pixel px) => TiffSaveable px where
  colorSpaceOfPixel :: px -> TiffColorspace

  extraSampleCodeOfPixel :: px -> Maybe ExtraSample
  extraSampleCodeOfPixel _ = Nothing

  subSamplingInfo   :: px -> V.Vector Word32
  subSamplingInfo _ = V.empty

instance TiffSaveable Pixel8 where
  colorSpaceOfPixel _ = TiffMonochrome

instance TiffSaveable Pixel16 where
  colorSpaceOfPixel _ = TiffMonochrome

instance TiffSaveable PixelYA8 where
  colorSpaceOfPixel _ = TiffMonochrome
  extraSampleCodeOfPixel _ = Just ExtraSampleUnassociatedAlpha

instance TiffSaveable PixelYA16 where
  colorSpaceOfPixel _ = TiffMonochrome
  extraSampleCodeOfPixel _ = Just ExtraSampleUnassociatedAlpha

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
  extraSampleCodeOfPixel _ = Just ExtraSampleUnassociatedAlpha

instance TiffSaveable PixelRGBA16 where
  colorSpaceOfPixel _ = TiffRGB
  extraSampleCodeOfPixel _ = Just ExtraSampleUnassociatedAlpha

instance TiffSaveable PixelYCbCr8 where
  colorSpaceOfPixel _ = TiffYCbCr
  subSamplingInfo _ = V.fromListN 2 [1, 1]

-- | Transform an image into a Tiff encoded bytestring, ready to be
-- written as a file.
encodeTiff :: forall px. (TiffSaveable px) => Image px -> Lb.ByteString
encodeTiff img = runPut $ putP rawPixelData hdr
  where intSampleCount = componentCount (undefined :: px)
        sampleCount = fromIntegral intSampleCount

        sampleType = undefined :: PixelBaseComponent px
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
            , tiffSampleCount        = fromIntegral sampleCount
            , tiffRowPerStrip        = fromIntegral $ imageHeight img
            , tiffPlaneConfiguration = PlanarConfigContig
            , tiffSampleFormat       = [TiffSampleUint]
            , tiffBitsPerSample      = V.replicate intSampleCount bitPerSample
            , tiffCompression        = CompressionNone
            , tiffStripSize          = V.singleton imageSize
            , tiffOffsets            = V.singleton headerSize
            , tiffPalette            = Nothing
            , tiffYCbCrSubsampling   = subSamplingInfo (undefined :: px)
            , tiffExtraSample        = extraSampleCodeOfPixel (undefined :: px)
            , tiffPredictor          = PredictorNone -- not used when writing
            , tiffMetadatas          = mempty
            }

-- | Helper function to directly write an image as a tiff on disk.
writeTiff :: (TiffSaveable pixel) => FilePath -> Image pixel -> IO ()
writeTiff path img = Lb.writeFile path $ encodeTiff img

{-# ANN module "HLint: ignore Reduce duplication" #-}

