{-# LANGUAGE Rank2Types #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
module Codec.Picture.BitWriter( BoolWriter
                              , BoolReader
                              , BoolWriteState
                              , writeBits
                              , byteAlign
                              , getNextBit
                              , setDecodedString
                              , runBoolWriter
                              ) where

import Control.Monad( when )
import Control.Monad.ST( ST, runST )
import qualified Control.Monad.Trans.State.Strict as S

import Data.Word( Word8, Word32 )
import Data.Serialize( Put, putWord8, runPut )
import Data.Bits( (.&.), (.|.), shiftR, shiftL )

import qualified Data.ByteString as B

runBoolWriter :: (forall s. BoolWriter s b) -> B.ByteString
runBoolWriter writer = runPut p
    where state = S.execStateT (writer >> flushWriter) (return (), 0, 0)
          (p ,_ , _) = runST state

-- | Current bit index, current value, string
type BoolState = (Int, Word8, B.ByteString)
type BoolReader s a = S.StateT BoolState (ST s) a

type BoolWriteState = (Put, Word8, Int)
type BoolWriter s a = S.StateT BoolWriteState (ST s) a

-- |  Drop all bit until the bit of indice 0, usefull to parse restart
-- marker, as they are byte aligned, but Huffman might not.
byteAlign :: BoolReader s ()
byteAlign = do
  (idx, _, chain) <- S.get
  when (idx /= 7) (setDecodedString chain)

{-# INLINE getNextBit #-}
getNextBit :: BoolReader s Bool
getNextBit = do
    (idx, v, chain) <- S.get
    let val = (v .&. (1 `shiftL` idx)) /= 0
    if idx == 0
      then setDecodedString chain
      else S.put (idx - 1, v, chain)
    return val


flushWriter :: BoolWriter s ()
flushWriter = do
    (p, val, count) <- S.get

    when (count /= 0)
         (let realVal = val `shiftL` (8 - count)
          in S.put (p >> putWord8 realVal, 0, 0))

writeBits :: Word32 -> Int -> BoolWriter s ()
writeBits bitData bitCount = S.get >>= serialize
  where serialize (str, currentWord, count)
            | bitCount + count == 8 =
                let newVal = fromIntegral $ (currentWord `shiftR` bitCount) .|. fromIntegral bitData
                in S.put (str >> putWord8 newVal, 0, 0)

            | bitCount + count < 8 =
                let newVal = fromIntegral $ (currentWord `shiftL` bitCount)
                in S.put (str, newVal .|. fromIntegral bitData, count + bitCount)

            | otherwise =
                let leftBitCount = 8 - count
                    highPart = fromIntegral $ bitData `shiftR` (bitCount - leftBitCount)
                    prevPart = currentWord `shiftL` leftBitCount

                    nextMask = 1 `shiftR` (bitCount - leftBitCount) - 1
                    newData = bitData .&. nextMask
                    newCount = bitCount - leftBitCount

                    toWrite = fromIntegral $ prevPart .|. highPart
                in S.put (str >> putWord8 toWrite, 0, 0) 
                        >> writeBits newData newCount

-- | Bitify a list of things to decode.
setDecodedString :: B.ByteString -> BoolReader s ()
setDecodedString str = case B.uncons str of
     Nothing        -> S.put (maxBound, 0, B.empty)
     Just (0xFF, rest) -> case B.uncons rest of
            Nothing                  -> S.put (maxBound, 0, B.empty)
            Just (0x00, afterMarker) -> S.put (7, 0xFF, afterMarker)
            Just (_   , afterMarker) -> setDecodedString afterMarker
     Just (v, rest) -> S.put (       7, v,    rest)
