{-# LANGUAGE BangPatterns #-}
module Codec.Picture.Gif.LZWEncoding( lzwEncode ) where

import Control.Applicative( (<$>) )
import Control.Monad.ST( runST )
import qualified Data.ByteString.Lazy as L
import Data.Maybe( fromMaybe )
import Data.Monoid( mempty )
import Data.Word( Word8 )
import qualified Data.IntMap.Strict as I
import qualified Data.Vector.Storable as V

import Codec.Picture.BitWriter

{-import Debug.Trace-}
{-import Text.Printf-}

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

lookupUpdate :: V.Vector Word8 -> Int -> Int -> Trie -> (Int, Int, Trie)
lookupUpdate vector freeIndex firstIndex trie =
    matchUpdate $ go trie 0 firstIndex 
  where
    matchUpdate (lzwOutputIndex, nextReadIndex, sub) =
        (lzwOutputIndex, nextReadIndex, fromMaybe trie sub)

    maxi = V.length vector
    go !currentTrie !prevIndex !index
      | index >= maxi = (prevIndex, index, Nothing)
      | otherwise = case I.lookup val currentTrie of
          Just (TrieNode ix subTable) ->
              let (lzwOutputIndex, nextReadIndex, newTable) =
                        go subTable ix $ index + 1
                  tableUpdater t =
                      I.insert val (TrieNode ix t) currentTrie
              in
              (lzwOutputIndex, nextReadIndex, tableUpdater <$> newTable)

          Nothing | index == maxi -> (prevIndex, index, Nothing)
                  | otherwise -> (prevIndex, index, Just $ I.insert val newNode currentTrie)

      where val = fromIntegral $ vector `V.unsafeIndex` index
            newNode = emptyNode { trieIndex = freeIndex }

lzwEncode :: Int -> V.Vector Word8 -> L.ByteString
lzwEncode initialKeySize vec = runST $ do
    bitWriter <- newWriteStateRef 

    let go (codeSize, _) readIndex _ | readIndex >= maxi =
            writeBitsGif bitWriter (fromIntegral endOfInfo) codeSize
        go (codeSize, writeIndex) readIndex trie = do
            let (indexToWrite, endIndex, trie') =
                    lookuper writeIndex readIndex trie
            writeBitsGif bitWriter (fromIntegral indexToWrite) codeSize
            go (updateCodeSize codeSize writeIndex)
               endIndex trie'

    writeBitsGif bitWriter (fromIntegral clearCode) startCodeSize
    go (startCodeSize, firstFreeIndex) 0 initialTrie

    finalizeBoolWriter bitWriter
  where
    maxi = V.length vec

    startCodeSize = initialKeySize + 1

    clearCode = 2 ^ initialKeySize :: Int
    endOfInfo = clearCode + 1
    firstFreeIndex = endOfInfo + 1
    
    lookuper = lookupUpdate vec
    updateCodeSize codeSize writeIdx
        | writeIdx == 2 ^ codeSize =
                (min 12 $ codeSize + 1, writeIdx + 1)
        | otherwise = (codeSize, writeIdx + 1)

