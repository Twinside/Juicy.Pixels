{-# LANGUAGE Rank2Types #-}
-- | This module implement helper functions to read & write data
-- at bits level.
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
import Data.Bits( Bits, (.&.), (.|.), shiftR, shiftL )

import Debug.Trace
import Text.Printf
import qualified Data.ByteString as B

(.<<.), (.>>.) :: (Bits a) => a -> Int -> a
(.<<.) = shiftL
(.>>.) = shiftR

-- | Run the writer and get the serialized data.
runBoolWriter :: (forall s. BoolWriter s b) -> B.ByteString
runBoolWriter writer = runPut p
    where state = S.execStateT (writer >> flushWriter) (return (), 0, 0)
          (p ,_ , _) = runST state

-- | Current bit index, current value, string
type BoolState = (Int, Word8, B.ByteString)

-- | Type used to read bits
type BoolReader s a = S.StateT BoolState (ST s) a

-- | Current serializer, bit buffer, bit count 
type BoolWriteState = (Put, Word8, Int)

-- | Type used to write bits
type BoolWriter s a = S.StateT BoolWriteState (ST s) a

-- | Drop all bit until the bit of indice 0, usefull to parse restart
-- marker, as they are byte aligned, but Huffman might not.
byteAlign :: BoolReader s ()
byteAlign = do
  (idx, _, chain) <- S.get
  when (idx /= 7) (setDecodedString chain)

-- | Return the next bit in the input stream.
{-# INLINE getNextBit #-}
getNextBit :: BoolReader s Bool
getNextBit = do
    (idx, v, chain) <- S.get
    let val = (v .&. (1 `shiftL` idx)) /= 0
    if idx == 0
      then setDecodedString chain
      else S.put (idx - 1, v, chain)
    return val

-- | If some bits are not serialized yet, write
-- them in the MSB of a word.
flushWriter :: BoolWriter s ()
flushWriter = do
    (p, val, count) <- S.get

    when (count /= 0)
         (let realVal = val `shiftL` (8 - count)
          in S.put (p >> putWord8 realVal, 0, 0))

-- | Append some data bits to a Put monad.
writeBits :: Word32     -- ^ The real data to be stored. Actual data should be in the LSB
          -> Int        -- ^ Number of bit to write from 1 to 32
          -> BoolWriter s ()
writeBits bitData bitCount = trace (printf "w %0X count:%d" bitData bitCount) $
 S.get >>= serialize
  where dumpByte 0xFF = putWord8 0xFF >> putWord8 0x00
        dumpByte    i = putWord8 i

        cleanMask = (1 `shiftL` bitCount) - 1 :: Word32
        cleanData = bitData .&. cleanMask     :: Word32

        serialize :: (Put, Word8, Int) -> BoolWriter s ()
        serialize (str, currentWord, count)
            | bitCount + count == 8 =
                let newVal = fromIntegral $
                        (currentWord .<<. bitCount) .|. fromIntegral cleanData
                in S.put (str >> dumpByte newVal, 0, 0)

            | bitCount + count < 8 =
                let newVal = currentWord .<<. bitCount
                in S.put (str, newVal .|. fromIntegral cleanData, count + bitCount)

            | otherwise =
                let leftBitCount = 8 - count :: Int
                    highPart = cleanData .>>. (bitCount - leftBitCount) :: Word32
                    prevPart = (fromIntegral currentWord) .<<. leftBitCount :: Word32

                    nextMask = (1 .<<. (bitCount - leftBitCount)) - 1 :: Word32
                    newData = cleanData .&. nextMask :: Word32
                    newCount = bitCount - leftBitCount :: Int

                    toWrite = fromIntegral $ prevPart .|. highPart :: Word8
                in S.put (str >> dumpByte toWrite, 0, 0) 
                        >> writeBits newData newCount

-- | Bitify a list of things to decode.
setDecodedString :: B.ByteString -> BoolReader s ()
setDecodedString str = case B.uncons str of
     Nothing        -> S.put (maxBound, 0, B.empty)
     Just (0xFF, rest) -> case B.uncons rest of
            Nothing                  -> S.put (maxBound, 0, B.empty)
            Just (0x00, afterMarker) -> S.put (7, 0xFF, afterMarker)
            Just (_   , afterMarker) -> setDecodedString afterMarker
     Just (v, rest) -> S.put {- . trace (printf "READ:%02X" ve -} $ (       7, v,    rest)

