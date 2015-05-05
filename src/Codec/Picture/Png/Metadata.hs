{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Codec.Picture.Png.Metadata( extractMetadatas ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mempty )
import Data.Foldable( foldMap )
#endif

import Data.Binary( Binary( get, put ) )
import Data.Binary.Get( getLazyByteStringNul )
import Data.Binary.Put( putLazyByteString, putWord8 )
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Monoid( (<>) )

import Codec.Picture.InternalHelper
import qualified Codec.Picture.Metadata as Met
import Codec.Picture.Metadata ( Metadatas, dotsPerMeterToDotPerInch )
import Codec.Picture.Png.Type

getGamma :: [L.ByteString] -> Metadatas
getGamma [] = mempty
getGamma (g:_) = foldMap unpackGamma $ runGet get g
  where
    unpackGamma gamma = Met.singleton Met.Gamma (getPngGamma gamma)

getDpis :: [L.ByteString] -> Metadatas
getDpis [] = mempty
getDpis (b:_) = foldMap unpackPhys $ runGet get b
  where
    unpackPhys PngPhysicalDimension { pngUnit = PngUnitUnknown } =
      Met.insert Met.DpiX 72 $ Met.singleton Met.DpiY 72
    unpackPhys phy@PngPhysicalDimension { pngUnit = PngUnitMeter } =
      Met.insert Met.DpiX dpx $ Met.singleton Met.DpiY dpy
        where
          dpx = dotsPerMeterToDotPerInch . fromIntegral $ pngDpiX phy
          dpy = dotsPerMeterToDotPerInch . fromIntegral $ pngDpiY phy

data PngText = PngText
  { pngKeyword :: !L.ByteString
  , pngData    :: !L.ByteString
  }
  deriving Show

instance Binary PngText where
  get = PngText <$> getLazyByteStringNul <*> getRemainingLazyBytes
  put (PngText kw pdata) = do
    putLazyByteString kw
    putWord8 0
    putLazyByteString pdata

textToMetadata :: PngText -> Metadatas
textToMetadata ptext = case pngKeyword ptext of
  "Title" -> strValue Met.Title
  "Author" -> strValue Met.Author
  "Description" -> strValue Met.Description
  "Copyright" -> strValue Met.Copyright
  {-"Creation Time" -> strValue Creation-}
  "Software" -> strValue Met.Software
  "Disclaimer" -> strValue Met.Disclaimer
  "Warning" -> strValue Met.Warning
  "Source" -> strValue Met.Source
  "Comment" -> strValue Met.Comment
  other -> 
    Met.singleton
      (Met.Unknown $ L.unpack other)
      (Met.String . L.unpack $ pngData ptext)
  where
    strValue k = Met.singleton k . L.unpack $ pngData ptext

getTexts :: [L.ByteString] -> Metadatas
getTexts = foldMap (foldMap textToMetadata . runGet get)

extractMetadatas :: PngRawImage -> Metadatas
extractMetadatas img = getDpis (chunksOf pHYsSignature)
                    <> getGamma (chunksOf gammaSignature)
                    <> getTexts (chunksOf tEXtSignature)
  where
    chunksOf = chunksWithSig img

