{-# LANGUAGE CPP #-}
module Codec.Picture.Jpg.Metadata ( extractMetadatas, encodeMetadatas ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( pure )
import Data.Monoid( mempty )
import Data.Word( Word )
#endif

import Data.Word( Word16 )
import Data.Maybe( fromMaybe )
import qualified Codec.Picture.Metadata as Met
import Codec.Picture.Metadata( Metadatas )
import Codec.Picture.Jpg.Types

scalerOfUnit :: JFifUnit -> Met.Keys Word -> Word16 -> Metadatas -> Metadatas
scalerOfUnit unit k v = case unit of
  JFifUnitUnknown -> id
  JFifPixelsPerInch -> Met.insert k (fromIntegral v)
  JFifPixelsPerCentimeter ->
    Met.insert k (Met.dotsPerCentiMeterToDotPerInch $ fromIntegral v)

extractMetadatas :: JpgJFIFApp0 -> Metadatas
extractMetadatas jfif = 
    inserter Met.DpiX (_jfifDpiX jfif)
        $ inserter Met.DpiY (_jfifDpiY jfif) mempty
  where
    inserter = scalerOfUnit $ _jfifUnit jfif


encodeMetadatas :: Metadatas -> [JpgFrame]
encodeMetadatas metas = fromMaybe [] $ do
  dpiX <- Met.lookup Met.DpiX metas
  dpiY <- Met.lookup Met.DpiY metas
  pure . pure . JpgJFIF $ JpgJFIFApp0
    { _jfifUnit      = JFifPixelsPerInch
    , _jfifDpiX      = fromIntegral dpiX
    , _jfifDpiY      = fromIntegral dpiY
    , _jfifThumbnail = Nothing
    }

