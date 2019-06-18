{-# LANGUAGE CPP #-}
module Codec.Picture.Tiff.Internal.Metadata
    ( extractTiffMetadata
    , encodeTiffStringMetadata
    , exifOffsetIfd
    ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mempty )
import Data.Foldable( foldMap )
import Control.Applicative( (<$>) )
#endif

import Data.Bits( unsafeShiftL, (.|.) )
import Data.Foldable( find )
import Data.List( sortBy )
import Data.Function( on )
import qualified Data.Foldable as F
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid( (<>) )
#endif
import Codec.Picture.Metadata( Metadatas )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Codec.Picture.Metadata as Met
import qualified Data.Vector.Generic as V
import Codec.Picture.Tiff.Internal.Types
import Codec.Picture.Metadata( extractExifMetas )
import Codec.Picture.Metadata.Exif

exifOffsetIfd :: ImageFileDirectory
exifOffsetIfd = ImageFileDirectory
  { ifdIdentifier = TagExifOffset
  , ifdCount = 1
  , ifdType = TypeLong
  , ifdOffset = 0
  , ifdExtended = ExifNone
  }

typeOfData :: ExifData -> IfdType
typeOfData d = case d of
  ExifNone -> error "Impossible - typeOfData : ExifNone"
  ExifIFD _exifs -> error "Impossible - typeOfData : ExifIFD"
  ExifLong _l -> TypeLong
  ExifLongs _l -> TypeLong
  ExifShort _s -> TypeShort
  ExifShorts _s -> TypeShort
  ExifString _str -> TypeAscii
  ExifUndefined _undef -> TypeUndefined
  ExifRational _r1 _r2 -> TypeRational
  ExifSignedRational _sr1 _sr2 -> TypeSignedRational

makeIfd :: ExifTag -> ExifData -> ImageFileDirectory
makeIfd t (ExifShort v) = ImageFileDirectory
  { ifdIdentifier = t
  , ifdType = TypeShort
  , ifdCount = 1
  , ifdOffset = fromIntegral v `unsafeShiftL` 16
  , ifdExtended = ExifNone
  }
makeIfd t (ExifLong v) = ImageFileDirectory 
  { ifdIdentifier = t
  , ifdType = TypeLong
  , ifdCount = 1
  , ifdOffset = fromIntegral v
  , ifdExtended = ExifNone
  }
makeIfd t d@(ExifShorts v)
  | size == 2 = ImageFileDirectory
    { ifdIdentifier = t
    , ifdType = TypeShort
    , ifdCount = 2
    , ifdOffset = combined
    , ifdExtended = ExifNone
    }
  | otherwise = ImageFileDirectory
    { ifdIdentifier = t
    , ifdType = TypeShort
    , ifdCount = size
    , ifdOffset = 0
    , ifdExtended = d
    }
  where
    size = fromIntegral $ F.length v
    at i = fromIntegral $ v V.! i
    combined = (at 0  `unsafeShiftL` 16) .|. at 1
makeIfd t d@(ExifLongs v)
  | size == 1 = ImageFileDirectory
    { ifdIdentifier = t
    , ifdType = TypeLong
    , ifdCount = 1
    , ifdOffset = v V.! 0
    , ifdExtended = ExifNone
    }
  | otherwise = ImageFileDirectory
    { ifdIdentifier = t
    , ifdType = TypeLong
    , ifdCount = size
    , ifdOffset = 0
    , ifdExtended = d
    }
  where size = fromIntegral $ F.length v
makeIfd t s@(ExifString str) = ImageFileDirectory
    { ifdIdentifier = t
    , ifdType = TypeAscii
    , ifdCount = fromIntegral $ BC.length str
    , ifdOffset = 0
    , ifdExtended = s
    }
makeIfd t s@(ExifUndefined str)
  | size > 4 = ImageFileDirectory
    { ifdIdentifier = t
    , ifdType = TypeUndefined
    , ifdCount = size
    , ifdOffset = 0
    , ifdExtended = s
    }
  | otherwise = ImageFileDirectory
    { ifdIdentifier = t
    , ifdType = TypeUndefined
    , ifdCount = size
    , ifdOffset = ofs
    , ifdExtended = ExifNone
    }
  where
    size = fromIntegral $ BC.length str
    at ix
      | fromIntegral ix < size = fromIntegral $ B.index str ix `unsafeShiftL` (4 - (8 * ix))
      | otherwise = 0
    ofs = at 0 .|. at 1 .|. at 2 .|. at 3
makeIfd t d = ImageFileDirectory
  { ifdIdentifier = t
  , ifdType = typeOfData d
  , ifdCount = 1
  , ifdOffset = 0
  , ifdExtended = d
  }

encodeTiffStringMetadata :: Metadatas -> [ImageFileDirectory]
encodeTiffStringMetadata metas = sortBy (compare `on` word16OfTag . ifdIdentifier) $ allTags where
  keyStr tag k = case Met.lookup k metas of
    Nothing -> mempty
    Just v -> pure . makeIfd tag . ExifString $ BC.pack v
  allTags = copyright <> artist <> title <> description <> software <> allPureExif

  allPureExif = fmap (uncurry makeIfd) $ extractExifMetas metas

  copyright = keyStr TagCopyright Met.Copyright
  artist = keyStr TagArtist Met.Author
  title = keyStr TagDocumentName Met.Title
  description = keyStr TagImageDescription Met.Description
  software = keyStr TagSoftware Met.Software

extractTiffStringMetadata :: [ImageFileDirectory] -> Metadatas
extractTiffStringMetadata = Met.insert Met.Format Met.SourceTiff . foldMap go where
  strMeta k = Met.singleton k . BC.unpack
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
    (TagMake, v) -> Met.singleton (Met.Exif TagMake) v
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

