module Codec.Picture.Jpg.Metadata ( extractMetadatas ) where

import Data.Word( Word16 )
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
        $ inserter Met.DpiY (_jfifDpiY jfif)
        $ mempty
  where
    inserter = scalerOfUnit $ _jfifUnit jfif
