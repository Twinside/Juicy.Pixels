{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Codec.Picture.Png.Internal.Metadata( extractMetadatas
                                 , encodeMetadatas
                                 ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>), (<*>), pure )
import Data.Monoid( Monoid, mempty )
import Data.Foldable( foldMap )
#endif

import Data.Maybe( fromMaybe )
import Data.Binary( Binary( get, put ), encode )
import Data.Binary.Get( getLazyByteStringNul, getWord8 )
import Data.Binary.Put( putLazyByteString, putWord8 )
import qualified Data.ByteString.Lazy.Char8 as L
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid( (<>) )
#endif

import qualified Codec.Compression.Zlib as Z

import Codec.Picture.InternalHelper
import qualified Codec.Picture.Metadata as Met
import Codec.Picture.Metadata ( Metadatas
                              , dotsPerMeterToDotPerInch
                              , Elem( (:=>) ) )
import Codec.Picture.Png.Internal.Type

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

data PngZText = PngZText
  { pngZKeyword :: !L.ByteString
  , pngZData    :: !L.ByteString
  }
  deriving Show

instance Binary PngZText where
  get = PngZText <$> getLazyByteStringNul <* getCompressionType <*> (Z.decompress <$> getRemainingLazyBytes)
    where
      getCompressionType = do
        0 <- getWord8
        return ()
  put (PngZText kw pdata) = do
    putLazyByteString kw
    putWord8 0
    putWord8 0 -- compression type
    putLazyByteString (Z.compress pdata)

aToMetadata :: (a -> L.ByteString) -> (a -> L.ByteString) -> a -> Metadatas
aToMetadata pkeyword pdata ptext = case pkeyword ptext of
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
      (Met.String . L.unpack $ pdata ptext)
  where
    strValue k = Met.singleton k . L.unpack $ pdata ptext

textToMetadata :: PngText -> Metadatas
textToMetadata = aToMetadata pngKeyword pngData

ztxtToMetadata :: PngZText -> Metadatas
ztxtToMetadata = aToMetadata pngZKeyword pngZData

getTexts :: [L.ByteString] -> Metadatas
getTexts = foldMap (eitherFoldMap textToMetadata . runGet get)

getZTexts :: [L.ByteString] -> Metadatas
getZTexts = foldMap (eitherFoldMap ztxtToMetadata . runGet get)

extractMetadatas :: PngRawImage -> Metadatas
extractMetadatas img = getDpis (chunksOf pHYsSignature)
                    <> getGamma (chunksOf gammaSignature)
                    <> getTexts (chunksOf tEXtSignature)
                    <> getZTexts (chunksOf zTXtSignature)
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
    Met.ColorSpace  :=> _ -> mempty
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

