{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- | This module expose a common "metadata" storage for various image
-- type. Different format can generate different metadatas, and write
-- only a part of them.
--
-- Since version 3.2.5
--
module Codec.Picture.Metadata( -- * Types
                               Metadatas
                             , Keys( .. )
                             , Value( .. )
                             , Elem( .. )
                             , SourceFormat( .. )

                               -- * Functions
                             , Codec.Picture.Metadata.lookup
                             , empty
                             , insert
                             , delete
                             , singleton

                               -- * Folding
                             , foldl'
                             , Codec.Picture.Metadata.foldMap

                              -- * Helper functions
                             , mkDpiMetadata
                             , mkSizeMetadata
                             , basicMetadata
                             , simpleMetadata

                               -- * Conversion functions
                             , dotsPerMeterToDotPerInch
                             , dotPerInchToDotsPerMeter 
                             , dotsPerCentiMeterToDotPerInch
                             ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( Monoid, mempty, mappend )
import Data.Word( Word )
#endif


import Control.DeepSeq( NFData( .. ) )
import qualified Data.Foldable as F

import Codec.Picture.Metadata.Exif

#if MIN_VERSION_base(4,7,0)
import Data.Typeable( (:~:)( Refl ) )
type Equiv = (:~:)
#else
data Equiv a b where
    Refl :: Equiv a a
#endif

-- | Type describing the original file format of the ilfe.
data SourceFormat
  = SourceJpeg
  | SourceGif
  | SourceBitmap
  | SourceTiff
  | SourcePng
  | SourceHDR
  | SourceTGA
  deriving (Eq, Show)

instance NFData SourceFormat where
  rnf a = a `seq` ()

-- | Store various additional information about an image. If
-- something is not recognized, it can be stored in an unknown tag.
--
--   * 'DpiX' Dot per inch on this x axis.
--
--   * 'DpiY' Dot per inch on this y axis.
--
--   * 'Width' Image width in pixel. Relying on the metadata for this
--          information can avoid the full decompression of the image.
--          Ignored for image writing.
--
--   * 'Height' Image height in pixels. Relyiung on the metadata for this
--          information can void the full decomrpession of the image.
--          Ignored for image writing.
--
--   * 'Unknown' unlikely to be decoded, but usefull for metadata writing
--
--   * 'Exif' Exif tag and associated data.
--
data Keys a where
  Gamma       :: Keys Double
  Format      :: Keys SourceFormat
  DpiX        :: Keys Word
  DpiY        :: Keys Word
  Width       :: Keys Word
  Height      :: Keys Word
  Title       :: Keys String
  Description :: Keys String
  Author      :: Keys String
  Copyright   :: Keys String
  Software    :: Keys String
  Comment     :: Keys String
  Disclaimer  :: Keys String
  Source      :: Keys String
  Warning     :: Keys String
  Exif        :: !ExifTag -> Keys ExifData
  Unknown     :: !String -> Keys Value

deriving instance Show (Keys a)
deriving instance Eq (Keys a)
{-deriving instance Ord (Keys a)-}

-- | Encode values for unknown information
data Value
  = Int    !Int
  | Double !Double
  | String !String
  deriving (Eq, Show)

instance NFData Value where
  rnf v = v `seq` () -- everything is strict, so it's OK

-- | Element describing a metadata and it's (typed) associated
-- value.
data Elem k =
  forall a. (Show a, NFData a) => !(k a) :=> a

deriving instance Show (Elem Keys)

instance NFData (Elem Keys) where
  rnf (_ :=> v) = rnf v `seq` ()

keyEq :: Keys a -> Keys b -> Maybe (Equiv a b)
keyEq a b = case (a, b) of
  (Gamma, Gamma) -> Just Refl
  (DpiX, DpiX) -> Just Refl
  (DpiY, DpiY) -> Just Refl
  (Width, Width) -> Just Refl
  (Height, Height) -> Just Refl
  (Title, Title) -> Just Refl
  (Description, Description) -> Just Refl
  (Author, Author) -> Just Refl
  (Copyright, Copyright) -> Just Refl
  (Software, Software) -> Just Refl
  (Comment, Comment) -> Just Refl
  (Disclaimer, Disclaimer) -> Just Refl
  (Source, Source) -> Just Refl
  (Warning, Warning) -> Just Refl
  (Format, Format) -> Just Refl
  (Unknown v1, Unknown v2) | v1 == v2 -> Just Refl
  (Exif t1, Exif t2) | t1 == t2 -> Just Refl
  _ -> Nothing

-- | Dependent storage used for metadatas.
-- All metadatas of a given kind are unique within
-- this container.
    --
-- The current data structure is based on list,
-- so bad performances can be expected.
newtype Metadatas = Metadatas
  { getMetadatas :: [Elem Keys]
  }
  deriving (Show, NFData)

instance Monoid Metadatas where
  mempty = empty
  mappend = union

-- | Right based union
union :: Metadatas -> Metadatas -> Metadatas
union m1 = F.foldl' go m1 . getMetadatas where
  go acc el@(k :=> _) = Metadatas $ el : getMetadatas (delete k acc)

-- | Strict left fold of the metadatas
foldl' :: (acc -> Elem Keys -> acc) -> acc -> Metadatas -> acc
foldl' f initAcc = F.foldl' f initAcc . getMetadatas

-- | foldMap equivalent for metadatas.
foldMap :: Monoid m => (Elem Keys -> m) -> Metadatas -> m
foldMap f = foldl' (\acc v -> acc `mappend` f v) mempty

-- | Remove an element of the given keys from the metadatas.
-- If not present does nothing.
delete :: Keys a -> Metadatas -> Metadatas
delete k = Metadatas . go . getMetadatas where
  go [] = []
  go (el@(k2 :=> _) : rest) = case keyEq k k2 of
    Nothing -> el : go rest
    Just Refl -> rest

-- | Search a metadata with the given key.
lookup :: Keys a -> Metadatas -> Maybe a
lookup k = go . getMetadatas where
  go [] = Nothing
  go ((k2 :=> v) : rest) = case keyEq k k2 of
    Nothing -> go rest
    Just Refl -> Just v

-- | Insert an element in the metadatas, if an element with
-- the same key is present, it is overwritten.
insert :: (Show a, NFData a) => Keys a -> a -> Metadatas -> Metadatas
insert k val metas =
  Metadatas $ (k :=> val) : getMetadatas (delete k metas)

-- | Create metadatas with a single element.
singleton :: (Show a, NFData a) => Keys a -> a -> Metadatas
singleton k val = Metadatas [k :=> val]

-- | Empty metadatas. Favor 'mempty'
empty :: Metadatas
empty = Metadatas mempty

-- | Conversion from dpm to dpi
dotsPerMeterToDotPerInch :: Word -> Word
dotsPerMeterToDotPerInch z = z * 254 `div` 10000

-- | Conversion from dpi to dpm
dotPerInchToDotsPerMeter :: Word -> Word
dotPerInchToDotsPerMeter z = (z * 10000) `div` 254

-- | Conversion dpcm -> dpi
dotsPerCentiMeterToDotPerInch :: Word -> Word
dotsPerCentiMeterToDotPerInch z = z * 254 `div` 100

-- | Create metadatas indicating the resolution, with DpiX == DpiY
mkDpiMetadata :: Word -> Metadatas
mkDpiMetadata w =
  Metadatas [DpiY :=> w, DpiX :=> w]

-- | Create metadatas holding width and height information.
mkSizeMetadata :: Integral n => n -> n -> Metadatas
mkSizeMetadata w h = 
  Metadatas [ Width :=> fromIntegral w, Height :=> fromIntegral h ]

-- | Create simple metadatas with Format, Width & Height
basicMetadata :: Integral nSize => SourceFormat -> nSize -> nSize -> Metadatas
basicMetadata f w h =
  Metadatas [ Format :=> f
            , Width :=> fromIntegral w
            , Height :=> fromIntegral h
            ]

-- | Create simple metadatas with Format, Width, Height, DpiX & DpiY
simpleMetadata :: (Integral nSize, Integral nDpi)
               => SourceFormat -> nSize -> nSize -> nDpi -> nDpi -> Metadatas
simpleMetadata f w h dpiX dpiY =
  Metadatas [ Format :=> f
            , Width :=> fromIntegral w
            , Height :=> fromIntegral h
            , DpiX :=> fromIntegral dpiX
            , DpiY :=> fromIntegral dpiY
            ]

