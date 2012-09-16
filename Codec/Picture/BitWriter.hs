{-# LANGUAGE Rank2Types #-}
-- | This module implement helper functions to read & write data
-- at bits level.
module Codec.Picture.BitWriter( BoolWriter
                              , BoolReader
                              , writeBits
                              , byteAlignJpg
                              , getNextBits
                              , getNextBitJpg
                              , setDecodedString
                              , setDecodedStringJpg
                              , runBoolWriter
                              , runBoolReader
                              ) where

import Control.Monad( when )
import Control.Monad.ST( ST )
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.Class( MonadTrans( .. ) )
import Data.Word( Word8, Word32 )
-- import Data.Serialize( Put, runPut )
import Data.Serialize.Builder( Builder, empty, append, singleton, toByteString )
import Data.Bits( Bits, (.&.), (.|.), shiftR, shiftL )

import qualified Data.ByteString as B

import Debug.Trace
{-# INLINE (.>>.) #-}
{-# INLINE (.<<.) #-}
(.<<.), (.>>.) :: (Bits a) => a -> Int -> a
(.<<.) = shiftL
(.>>.) = shiftR


--------------------------------------------------
----            Reader
--------------------------------------------------
-- | Current bit index, current value, string
type BoolState = (Int, Word8, B.ByteString)

-- | Type used to read bits
type BoolReader s a = S.StateT BoolState (ST s) a

runBoolReader :: BoolReader s a -> ST s a
runBoolReader action = S.evalStateT action (0, 0, B.empty)

-- | Bitify a list of things to decode.
setDecodedString :: B.ByteString -> BoolReader s ()
setDecodedString str = case B.uncons str of
     Nothing        -> S.put (      0, 0, B.empty)
     Just (v, rest) -> S.put (       0, v,    rest)

-- | Drop all bit until the bit of indice 0, usefull to parse restart
-- marker, as they are byte aligned, but Huffman might not.
byteAlignJpg :: BoolReader s ()
byteAlignJpg = do
  (idx, _, chain) <- S.get
  when (idx /= 7) (setDecodedStringJpg chain)

{-# INLINE getNextBitJpg #-}
getNextBitJpg :: BoolReader s Bool
getNextBitJpg = do
    (idx, v, chain) <- S.get
    let val = (v .&. (1 `shiftL` idx)) /= 0
    if idx == 0
      then setDecodedStringJpg chain
      else S.put (idx - 1, v, chain)
    return val

{-# INLINE getNextBits #-}
getNextBits :: Int -> BoolReader s Word32
getNextBits count = trace "=" $ aux 0 count
  where aux acc 0 = return acc
        aux acc n = do
            bit <- getNextBit
            let shifted = acc .<<. 1
                nextVal | bit = acc .|. (1 .<<. (count - n))
                        | otherwise = acc -- shifted
            trace (if bit then "1" else "0") aux nextVal (n - 1)

{-# INLINE getNextBit #-}
getNextBit :: BoolReader s Bool
getNextBit = do
    (idx, v, chain) <- S.get
    let val = (v .&. (1 `shiftL` idx)) /= 0
    if idx == 7
      then setDecodedString chain
      else S.put (idx + 1, v, chain)
    return val

-- | Bitify a list of things to decode. Handle Jpeg escape
-- code (0xFF 0x00), thus should be only used in JPEG decoding.
setDecodedStringJpg :: B.ByteString -> BoolReader s ()
setDecodedStringJpg str = case B.uncons str of
     Nothing        -> S.put (maxBound, 0, B.empty)
     Just (0xFF, rest) -> case B.uncons rest of
            Nothing                  -> S.put (maxBound, 0, B.empty)
            Just (0x00, afterMarker) -> S.put (7, 0xFF, afterMarker)
            Just (_   , afterMarker) -> setDecodedStringJpg afterMarker
     Just (v, rest) -> S.put (       7, v,    rest)

--------------------------------------------------
----            Writer
--------------------------------------------------

-- | Run the writer and get the serialized data.
runBoolWriter :: BoolWriter s b -> ST s B.ByteString
runBoolWriter writer = do
     let finalWriter = writer >> flushWriter
     PairS _ (BoolWriteState builder _ _) <-
            run finalWriter (BoolWriteState empty 0 0)
     return $ toByteString builder

-- | Current serializer, bit buffer, bit count 
data BoolWriteState = BoolWriteState !Builder
                                     {-# UNPACK #-} !Word8
                                     {-# UNPACK #-} !Int

data BoolWriterT m a = BitPut { run :: BoolWriteState -> m (PairS a) }

type BoolWriter s a = BoolWriterT (ST s) a

data PairS a = PairS a {-# UNPACK #-} !BoolWriteState

-- | If some bits are not serialized yet, write
-- them in the MSB of a word.
flushWriter :: BoolWriter s ()
flushWriter = BitPut $ \st@(BoolWriteState p val count) -> return . PairS () $
    let realVal = val `shiftL` (8 - count)
        new_context =  BoolWriteState (append p (singleton realVal)) 0 0
    in if count == 0 then st else new_context

instance MonadTrans BoolWriterT where
    lift a = BitPut $ \s ->
        a >>= \b -> return $ PairS b s

instance Monad m => Monad (BoolWriterT m) where
  m >>= k = BitPut $ \s -> do
    PairS a s' <- run m s
    PairS b s'' <-  run (k a) s'
    return $ PairS b s''
  return x = BitPut $ \s -> return $ PairS x s

-- | Append some data bits to a Put monad.
writeBits :: Word32     -- ^ The real data to be stored. Actual data should be in the LSB
          -> Int        -- ^ Number of bit to write from 1 to 32
          -> BoolWriter s ()
writeBits = \d c -> BitPut (serialize d c)
  where dumpByte str 0xFF = append (append str (singleton 0xFF)) $ singleton 0x00
        dumpByte str    i = append str (singleton i)

        serialize bitData bitCount (BoolWriteState str currentWord count)
            | bitCount + count == 8 =
                let newVal = fromIntegral $
                        (currentWord .<<. bitCount) .|. fromIntegral cleanData
                in return . PairS () $ BoolWriteState (dumpByte str newVal) 0 0

            | bitCount + count < 8 =
                let newVal = currentWord .<<. bitCount
                in return . PairS () $ BoolWriteState str (newVal .|. fromIntegral cleanData)
                                              (count + bitCount)

            | otherwise =
                let leftBitCount = 8 - count :: Int
                    highPart = cleanData .>>. (bitCount - leftBitCount) :: Word32
                    prevPart = fromIntegral currentWord .<<. leftBitCount :: Word32

                    nextMask = (1 .<<. (bitCount - leftBitCount)) - 1 :: Word32
                    newData = cleanData .&. nextMask :: Word32
                    newCount = bitCount - leftBitCount :: Int

                    toWrite = fromIntegral $ prevPart .|. highPart :: Word8
                in serialize newData newCount (BoolWriteState (dumpByte str toWrite) 0 0)

              where cleanMask = (1 `shiftL` bitCount) - 1 :: Word32
                    cleanData = bitData .&. cleanMask     :: Word32

