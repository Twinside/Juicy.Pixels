{-# LANGUAGE CPP #-}
module Codec.Picture.Tiff.Metadata( extractTiffMetadata ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mempty )
import Data.Foldable( foldMap )
import Control.Applicative( (<$>) )
#endif

import Data.Foldable( find )
import qualified Data.Foldable as F
import Data.Monoid( (<>) )
import Codec.Picture.Metadata( Metadatas )
import qualified Data.ByteString.Char8 as B
import qualified Codec.Picture.Metadata as Met
import Codec.Picture.Tiff.Types
import Codec.Picture.Metadata.Exif

extractTiffStringMetadata :: [ImageFileDirectory] -> Metadatas
extractTiffStringMetadata = Met.insert Met.Format Met.SourceTiff . foldMap go where
  strMeta k = Met.singleton k . B.unpack
  exif ifd =
    Met.singleton (Met.Exif $ ifdIdentifier ifd) $ ifdExtended ifd
  inserter acc (k, v) = Met.insert (Met.Exif k) v acc
  exifShort ifd =
    Met.singleton (Met.Exif $ ifdIdentifier ifd) . (ExifShort . fromIntegral) $ ifdOffset ifd

  go :: ImageFileDirectory -> Metadatas
  go ifd = case (ifdIdentifier ifd, ifdExtended ifd) of
    (TagArtist, ExifString v) -> strMeta Met.Author v
    (TagBitsPerSample, _) -> mempty
    (TagColorMap, _) -> mempty
    (TagCompression, _) -> mempty
    (TagCopyright, ExifString v) -> strMeta Met.Copyright v
    (TagDocumentName, ExifString v) -> strMeta Met.Title v
    (TagExifOffset, ExifIFD lst) -> F.foldl' inserter mempty lst
    (TagImageDescription, ExifString v) -> strMeta Met.Description v
    (TagImageLength, _) -> Met.singleton Met.Height . fromIntegral $ ifdOffset ifd
    (TagImageWidth, _) -> Met.singleton Met.Width . fromIntegral $ ifdOffset ifd
    (TagJPEGACTables, _) -> mempty
    (TagJPEGDCTables, _) -> mempty
    (TagJPEGInterchangeFormat, _) -> mempty
    (TagJPEGInterchangeFormatLength, _) -> mempty
    (TagJPEGLosslessPredictors, _) -> mempty
    (TagJPEGPointTransforms, _) -> mempty
    (TagJPEGQTables, _) -> mempty
    (TagJPEGRestartInterval, _) -> mempty
    (TagJpegProc, _) -> mempty
    (TagModel, v) -> Met.singleton (Met.Exif TagModel) v
    (TagOrientation, _) -> exifShort ifd
    (TagResolutionUnit, _) -> mempty
    (TagRowPerStrip, _) -> mempty
    (TagSamplesPerPixel, _) -> mempty
    (TagSoftware, ExifString v) -> strMeta Met.Software v
    (TagStripByteCounts, _) -> mempty
    (TagStripOffsets, _) -> mempty
    (TagTileByteCount, _) -> mempty
    (TagTileLength, _) -> mempty
    (TagTileOffset, _) -> mempty
    (TagTileWidth, _) -> mempty
    (TagUnknown _, _) -> exif ifd
    (TagXResolution, _) -> mempty
    (TagYCbCrCoeff, _) -> mempty
    (TagYCbCrPositioning, _) -> mempty
    (TagYCbCrSubsampling, _) -> mempty
    (TagYResolution, _) -> mempty
    _ -> mempty

byTag :: ExifTag -> ImageFileDirectory -> Bool
byTag t ifd = ifdIdentifier ifd == t

data TiffResolutionUnit
  = ResolutionUnitUnknown
  | ResolutionUnitInch
  | ResolutionUnitCentimeter

unitOfIfd :: ImageFileDirectory -> TiffResolutionUnit
unitOfIfd ifd = case (ifdType ifd, ifdOffset ifd) of
  (TypeShort, 1) -> ResolutionUnitUnknown
  (TypeShort, 2) -> ResolutionUnitInch
  (TypeShort, 3) -> ResolutionUnitCentimeter
  _ -> ResolutionUnitUnknown

extractTiffDpiMetadata :: [ImageFileDirectory] -> Metadatas
extractTiffDpiMetadata lst = go where
  go = case unitOfIfd <$> find (byTag TagResolutionUnit) lst of
    Nothing -> mempty
    Just ResolutionUnitUnknown -> mempty
    Just ResolutionUnitCentimeter -> findDpis Met.dotsPerCentiMeterToDotPerInch mempty
    Just ResolutionUnitInch -> findDpis id mempty

  findDpis toDpi =
     findDpi Met.DpiX TagXResolution toDpi . findDpi Met.DpiY TagYResolution toDpi

  findDpi k tag toDpi metas = case find (byTag tag) lst of
    Nothing -> metas
    Just ImageFileDirectory { ifdExtended = ExifRational num den } ->
      Met.insert k (toDpi . fromIntegral $ num `div` den) metas
    Just _ -> metas

extractTiffMetadata :: [ImageFileDirectory] -> Metadatas
extractTiffMetadata lst = extractTiffDpiMetadata lst <> extractTiffStringMetadata lst

