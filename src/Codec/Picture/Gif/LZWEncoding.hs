{-# LANGUAGE BangPatterns #-}
module Codec.Picture.Gif.LZWEncoding( lzwEncode ) where

import Control.Applicative( (<$>) )
import Control.Monad.ST( runST )
import Data.Bits( (.|.), (.&.)
                , unsafeShiftL
                , unsafeShiftR
                , testBit, setBit
                )
import qualified Data.ByteString.Lazy as L
import Data.List( foldl' )
import Data.Maybe( fromMaybe )
import Data.Monoid( mempty )
import Data.Word( Word8, Word32 )
import qualified Data.IntMap.Strict as I
import qualified Data.Vector.Storable as V

import Codec.Picture.BitWriter

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

byteReverseTable :: V.Vector Word32
byteReverseTable = V.generate 256 inverseBits
  where inverseBits v = foldl' (shift v) 0 [0 .. 7]
        shift v acc ix
          | v `testBit` ix = acc `setBit` (7 - ix)
          | otherwise = acc

reverseIndex :: Word32 -> Word32
reverseIndex v = (rb1 `unsafeShiftL` 8) .|. rb2
  where b1 = fromIntegral $ v .&. 0xFF
        b2 = fromIntegral $ (v `unsafeShiftR` 8) .&. 0xFF

        rb1 = byteReverseTable `V.unsafeIndex` b1
        rb2 = byteReverseTable `V.unsafeIndex` b2

lookupUpdate :: V.Vector Word8 -> Int -> Int -> Trie -> (Int, Int, Trie)
lookupUpdate vector freeIndex firstIndex trie =
    matchUpdate $ go trie 0 firstIndex 
  where
    matchUpdate (lzwOutputIndex, nextReadIndex, sub) =
        (lzwOutputIndex, nextReadIndex, fromMaybe trie sub)

    maxi = V.length vector
    go !prev !prevIndex !index
      | index >= maxi = (prevIndex, index, Nothing)
      | otherwise = case I.lookup val prev of
          Just (TrieNode ix subTable) ->
              let (lzwOutputIndex, nextReadIndex, subTable') =
                        go subTable ix $ index + 1 in
              (lzwOutputIndex, nextReadIndex, updateNode val . TrieNode ix <$> subTable')
          Nothing | index == maxi -> (prevIndex, index, Nothing)
                  | otherwise ->
                    (prevIndex, index, Just $ updateNode nextVal newNode)
      where val = fromIntegral $ vector `V.unsafeIndex` index
            nextVal = fromIntegral $ vector `V.unsafeIndex` (index + 1)
            newNode = emptyNode { trieIndex = freeIndex }
            updateNode at node = I.insert at node prev 

-- Untested? untested.
lzwEncode :: V.Vector Word8 -> L.ByteString
lzwEncode vec = runST $ do
    bitWriter <- newWriteStateRef 

    let go _ _ readIndex _ | readIndex >= maxi = return ()
        go codeSize writeIndex readIndex trie = do
            let (indexToWrite, endIndex, trie') =
                    lookuper writeIndex readIndex trie
                reversedBits = reverseIndex $ fromIntegral indexToWrite
            writeBits' bitWriter reversedBits codeSize
            go (updateCodeSize codeSize writeIndex)
               (writeIndex + 1) endIndex trie'

    go 9 0x100 0 initialTrie
    finalizeBoolWriter bitWriter
  where
    maxi = V.length vec
    
    lookuper = lookupUpdate vec
    updateCodeSize codeSize writeIdx
        | writeIdx == 2 ^ codeSize = min 12 $ codeSize + 1
        | otherwise = codeSize


