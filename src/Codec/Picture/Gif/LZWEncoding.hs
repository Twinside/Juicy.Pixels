{-# LANGUAGE BangPatterns #-}
module Codec.Picture.Gif.LZWEncoding where

import Control.Applicative( (<$>) )
import Data.Maybe( fromMaybe )
import Data.Monoid( mempty )
import Data.Word( Word8 )
import qualified Data.IntMap.Strict as I
import qualified Data.Vector.Storable as V

type Trie = I.IntMap TrieNode

data TrieNode = TrieNode
    { trieIndex :: {-# UNPACK #-} !Int
    , trieSub   :: !Trie
    }

emptyNode :: TrieNode
emptyNode = TrieNode
    { trieIndex = -1
    , trieSub = mempty
    }

initialTrie :: Trie
initialTrie = I.fromList
    [(i, emptyNode { trieIndex = i }) | i <- [0 .. 255]]

lookupUpdate :: V.Vector Word8 -> Int -> Int -> Trie -> (Int, Trie)
lookupUpdate vector freeIndex firstIndex trie =
    matchUpdate $ go trie 0 firstIndex 
  where
    matchUpdate (ix, sub) = (ix, fromMaybe trie sub)
    maxi = V.length vector
    go !prev !prevIndex !index
      | index >= maxi = (prevIndex, Nothing)
      | otherwise = case I.lookup val prev of
          Just (TrieNode ix subTable) ->
              let (ix', subTable') = go subTable ix $ index + 1 in
              (ix', updateNode val . TrieNode ix <$> subTable')
          Nothing | index == maxi -> (prevIndex, Nothing)
                  | otherwise ->
                    (prevIndex, Just $ updateNode nextVal newNode)
      where val = fromIntegral $ vector `V.unsafeIndex` index
            nextVal = fromIntegral $ vector `V.unsafeIndex` (index + 1)
            newNode = emptyNode { trieIndex = freeIndex }
            updateNode at node = I.insert at node prev 


