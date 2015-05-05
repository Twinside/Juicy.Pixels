{-# LANGUAGE CPP #-}
module Codec.Picture.Tiff.Metadata( extractTiffMetadata ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mempty )
import Data.Foldable( foldMap )
import Control.Applicative( (<$>), (<*>), pure )
#endif

import Data.Foldable( find )
import Data.Monoid( (<>) )
import Codec.Picture.Metadata( Metadatas )
import qualified Data.ByteString.Char8 as B
import qualified Codec.Picture.Metadata as Met
import Codec.Picture.Tiff.Types

extractTiffStringMetadata :: [ImageFileDirectory] -> Metadatas
extractTiffStringMetadata = foldMap go where
  strMeta k = Met.singleton k . B.unpack

  go ifd = case (ifdIdentifier ifd, ifdExtended ifd) of
    (TagCopyright, ExtendedDataAscii v) -> strMeta Met.Copyright v
    (TagArtist, ExtendedDataAscii v) -> strMeta Met.Author v
    (TagDocumentName, ExtendedDataAscii v) -> strMeta Met.Title v
    (TagSoftware, ExtendedDataAscii v) -> strMeta Met.Software v
    (TagImageDescription, ExtendedDataAscii v) -> strMeta Met.Description v
    _ -> mempty

byTag :: TiffTag -> ImageFileDirectory -> Bool
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
    Just ImageFileDirectory { ifdExtended = ExtendedDataRational num den } ->
      Met.insert k (toDpi . fromIntegral $ num `div` den) metas
    Just _ -> metas

extractTiffMetadata :: [ImageFileDirectory] -> Metadatas
extractTiffMetadata lst = extractTiffDpiMetadata lst <> extractTiffStringMetadata lst

