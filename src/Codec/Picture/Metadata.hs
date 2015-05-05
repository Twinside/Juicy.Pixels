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
module Codec.Picture.Metadata( Metadatas
                             , Keys( .. )
                             , Value( .. )

                             , Codec.Picture.Metadata.lookup
                             , dotsPerMeterToDotPerInch
                             , dotsPerCentiMeterToDotPerInch
                             , empty
                             , insert
                             , delete
                             , singleton
                             ) where

import Control.DeepSeq( NFData( .. ) )
import Data.Typeable( (:~:)( Refl ) )
import qualified Data.Foldable as F

-- | Store various additional information about an image. If
-- something is not recognize, it can be stored in an unknown tag.
--
--   * 'DpiX' Dot per inch on this x axis.
--
--   * 'DpiY' Dot per inch on this y axis.
--
data Keys a where
  Gamma       :: Keys Double
  DpiX        :: Keys Word
  DpiY        :: Keys Word
  Title       :: Keys String
  Description :: Keys String
  Author      :: Keys String
  Copyright   :: Keys String
  Software    :: Keys String
  Comment     :: Keys String
  Disclaimer  :: Keys String
  Source      :: Keys String
  Warning     :: Keys String
  Unknown     :: !String -> Keys Value

deriving instance Show (Keys a)
deriving instance Eq (Keys a)
deriving instance Ord (Keys a)

data Value
  = Int    !Int
  | Double !Double
  | String !String
  deriving (Eq, Show)

instance NFData Value where
  rnf v = v `seq` () -- everything is strict, so it's OK

data Elem k = forall a. (Show a, NFData a) => !(k a) :=> a

deriving instance Show (Elem Keys)

instance NFData (Elem Keys) where
  rnf (_ :=> v) = rnf v `seq` ()

keyEq :: Keys a -> Keys b -> Maybe (a :~: b)
keyEq a b = case (a, b) of
  (Gamma, Gamma) -> Just Refl
  (DpiX, DpiX) -> Just Refl
  (DpiY, DpiY) -> Just Refl
  (Title, Title) -> Just Refl
  (Description, Description) -> Just Refl
  (Author, Author) -> Just Refl
  (Copyright, Copyright) -> Just Refl
  (Software, Software) -> Just Refl
  (Comment, Comment) -> Just Refl
  (Disclaimer, Disclaimer) -> Just Refl
  (Source, Source) -> Just Refl
  (Warning, Warning) -> Just Refl
  (Unknown v1, Unknown v2) | v1 == v2 -> Just Refl
  _ -> Nothing

-- | Dependent storage used for metadatas.
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

delete :: Keys a -> Metadatas -> Metadatas
delete k = Metadatas . go . getMetadatas where
  go [] = []
  go (el@(k2 :=> _) : rest) = case keyEq k k2 of
    Nothing -> el : go rest
    Just Refl -> rest

lookup :: Keys a -> Metadatas -> Maybe a
lookup k = go . getMetadatas where
  go [] = Nothing
  go ((k2 :=> v) : rest) = case keyEq k k2 of
    Nothing -> go rest
    Just Refl -> Just v

insert :: (Show a, NFData a) => Keys a -> a -> Metadatas -> Metadatas
insert k val metas =
  Metadatas $ (k :=> val) : getMetadatas (delete k metas)

singleton :: (Show a, NFData a) => Keys a -> a -> Metadatas
singleton k val = Metadatas [k :=> val]

empty :: Metadatas
empty = Metadatas mempty

dotsPerMeterToDotPerInch :: Word -> Word
dotsPerMeterToDotPerInch z = z * 254 `div` 10000

dotsPerCentiMeterToDotPerInch :: Word -> Word
dotsPerCentiMeterToDotPerInch z = z * 254 `div` 100

