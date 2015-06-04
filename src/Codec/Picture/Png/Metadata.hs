{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Codec.Picture.Png.Metadata( extractMetadatas
                                 , encodeMetadatas
                                 ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>), (<*>), pure )
import Data.Monoid( Monoid, mempty )
import Data.Foldable( foldMap )
#endif

import Data.Maybe( fromMaybe )
import Data.Binary( Binary( get, put ), encode )
import Data.Binary.Get( getLazyByteStringNul )
import Data.Binary.Put( putLazyByteString, putWord8 )
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Monoid( (<>) )

import Codec.Picture.InternalHelper
import qualified Codec.Picture.Metadata as Met
import Codec.Picture.Metadata ( Metadatas
                              , dotsPerMeterToDotPerInch
                              , Elem( (:=>) ) )
import Codec.Picture.Png.Type

#if !MIN_VERSION_base(4,7,0)
eitherFoldMap :: Monoid m => (a -> m) -> Either e a -> m
eitherFoldMap f v = case v of
  Left _ -> mempty
  Right a -> f a
#else
eitherFoldMap :: Monoid m => (a -> m) -> Either e a -> m
eitherFoldMap = foldMap
#endif

getGamma :: [L.ByteString] -> Metadatas
getGamma [] = mempty
getGamma (g:_) = eitherFoldMap unpackGamma $ runGet get g
  where
    unpackGamma gamma = Met.singleton Met.Gamma (getPngGamma gamma)

getDpis :: [L.ByteString] -> Metadatas
getDpis [] = mempty
getDpis (b:_) = eitherFoldMap unpackPhys $ runGet get b
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
getTexts = foldMap (eitherFoldMap textToMetadata . runGet get) where
 

extractMetadatas :: PngRawImage -> Metadatas
extractMetadatas img = getDpis (chunksOf pHYsSignature)
                    <> getGamma (chunksOf gammaSignature)
                    <> getTexts (chunksOf tEXtSignature)
  where
    chunksOf = chunksWithSig img

encodePhysicalMetadata :: Metadatas -> [PngRawChunk]
encodePhysicalMetadata metas = fromMaybe [] $ do
  dx <- Met.lookup Met.DpiX metas
  dy <- Met.lookup Met.DpiY metas
  let to = fromIntegral . Met.dotPerInchToDotsPerMeter
      dim = PngPhysicalDimension (to dx) (to dy) PngUnitMeter
  pure [mkRawChunk pHYsSignature $ encode dim]

encodeSingleMetadata :: Metadatas -> [PngRawChunk]
encodeSingleMetadata = Met.foldMap go where
  go :: Elem Met.Keys -> [PngRawChunk]
  go v = case v of
    Met.Exif _ :=> _ -> mempty
    Met.DpiX :=> _ -> mempty
    Met.DpiY :=> _ -> mempty
    Met.Width :=> _ -> mempty
    Met.Height :=> _ -> mempty
    Met.Format :=> _ -> mempty
    Met.Gamma       :=> g ->
      pure $ mkRawChunk gammaSignature . encode $ PngGamma g
    Met.Title       :=> tx -> txt "Title" (L.pack tx)
    Met.Description :=> tx -> txt "Description" (L.pack tx)
    Met.Author      :=> tx -> txt "Author" (L.pack tx)
    Met.Copyright   :=> tx -> txt "Copyright" (L.pack tx)
    Met.Software    :=> tx -> txt "Software" (L.pack tx)
    Met.Comment     :=> tx -> txt "Comment" (L.pack tx)
    Met.Disclaimer  :=> tx -> txt "Disclaimer" (L.pack tx)
    Met.Source      :=> tx -> txt "Source" (L.pack tx)
    Met.Warning     :=> tx -> txt "Warning" (L.pack tx)
    Met.Unknown k   :=> Met.String tx -> txt (L.pack k) (L.pack tx)
    Met.Unknown _   :=> _ -> mempty

  txt k c = pure . mkRawChunk tEXtSignature . encode $ PngText k c

encodeMetadatas :: Metadatas -> [PngRawChunk]
encodeMetadatas m = encodePhysicalMetadata m <> encodeSingleMetadata m

