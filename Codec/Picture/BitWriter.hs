{-# LANGUAGE Rank2Types #-}
-- | This module implement helper functions to read & write data
-- at bits level.
module Codec.Picture.BitWriter( BoolWriter
                              , BoolReader
                              , BoolState( .. )
                              , writeBits
                              , byteAlignJpg
                              , getNextBits
                              , getNextBitJpg
                              , setDecodedString
                              , setDecodedStringJpg
                              , pushByte
                              , runBoolWriter
                              , runBoolReader
                              ) where

import Control.Monad( when )
import Control.Monad.ST( ST )
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.Class( MonadTrans( .. ) )
import Data.Word( Word8, Word32 )
import Data.Bits( Bits, (.&.), (.|.), shiftR, shiftL )

import qualified Data.Vector.Storable.Mutable as M
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString as B

--------------------------------------------------
----            Reader
--------------------------------------------------
-- | Current bit index, current value, string
data BoolState = BoolState {-# UNPACK #-} !Int
                           {-# UNPACK #-} !Word8
                           !B.ByteString

-- | Type used to read bits
type BoolReader s a = S.StateT BoolState (ST s) a

runBoolReader :: BoolReader s a -> ST s a
runBoolReader action = S.evalStateT action $ BoolState 0 0 B.empty

-- | Bitify a list of things to decode.
setDecodedString :: B.ByteString -> BoolReader s ()
setDecodedString str = case B.uncons str of
     Nothing        -> S.put $ BoolState      0 0 B.empty
     Just (v, rest) -> S.put $ BoolState      0 v    rest

-- | Drop all bit until the bit of indice 0, usefull to parse restart
-- marker, as they are byte aligned, but Huffman might not.
byteAlignJpg :: BoolReader s ()
byteAlignJpg = do
  BoolState idx _ chain <- S.get
  when (idx /= 7) (setDecodedStringJpg chain)

{-# INLINE getNextBitJpg #-}
getNextBitJpg :: BoolReader s Bool
getNextBitJpg = do
    BoolState idx v chain <- S.get
    let val = (v .&. (1 `shiftL` idx)) /= 0
    if idx == 0
      then setDecodedStringJpg chain
      else S.put $ BoolState (idx - 1) v chain
    return val

{-# INLINE getNextBits #-}
getNextBits :: Int -> BoolReader s Word32
getNextBits count = aux 0 count
  where aux acc 0 = return acc
        aux acc n = do
            bit <- getNextBit
            let nextVal | bit = acc .|. (1 `shiftL` (count - n))
                        | otherwise = acc
            aux nextVal (n - 1)

{-# INLINE getNextBit #-}
getNextBit :: BoolReader s Bool
getNextBit = do
    BoolState idx v chain <- S.get
    let val = (v .&. (1 `shiftL` idx)) /= 0
    if idx == 7
      then setDecodedString chain
      else S.put $ BoolState (idx + 1) v chain
    return val

-- | Bitify a list of things to decode. Handle Jpeg escape
-- code (0xFF 0x00), thus should be only used in JPEG decoding.
setDecodedStringJpg :: B.ByteString -> BoolReader s ()
setDecodedStringJpg str = case B.uncons str of
     Nothing        -> S.put $ BoolState maxBound 0 B.empty
     Just (0xFF, rest) -> case B.uncons rest of
            Nothing                  -> S.put $ BoolState maxBound 0 B.empty
            Just (0x00, afterMarker) -> S.put $ BoolState 7 0xFF afterMarker
            Just (_   , afterMarker) -> setDecodedStringJpg afterMarker
     Just (v, rest) -> S.put $ BoolState 7 v rest

--------------------------------------------------
----            Writer
--------------------------------------------------
defaultBufferSize :: Int
defaultBufferSize = 256 * 1024

-- | Run the writer and get the serialized data.
runBoolWriter :: BoolWriter s b -> ST s B.ByteString
runBoolWriter writer = do
    origMv <- M.new defaultBufferSize
    st <- S.execStateT (writer >> flushWriter) (BoolWriteState origMv [] 0 0 0)
    st' <- forceBufferFlushing st
    return . B.concat $ strings st'

-- | Current serializer, bit buffer, bit count
data BoolWriteState s = BoolWriteState
        { wordWrite    :: M.MVector s Word8
        , strings      :: ![B.ByteString]
        , writtenWords :: {-# UNPACK #-} !Int
        , bitAcc       :: {-# UNPACK #-} !Word8
        , bitReaded    :: {-# UNPACK #-} !Int
        }

type BoolWriter s a = S.StateT (BoolWriteState s) (ST s) a

forceBufferFlushing :: BoolWriteState s -> ST s (BoolWriteState s)
forceBufferFlushing st@(BoolWriteState { wordWrite = vec
                                       , writtenWords = count
                                       , strings = lst
                                       }) = do
    nmv <- M.new defaultBufferSize
    str <- byteStringFromVector vec count
    return $ st { wordWrite = nmv
                , strings = lst ++ [str]
                , writtenWords = 0
                }

flushCurrentBuffer :: BoolWriteState s -> ST s (BoolWriteState s)
flushCurrentBuffer st | writtenWords st < M.length (wordWrite st) = return st
flushCurrentBuffer st = forceBufferFlushing st

-- Data.Vector.Storable.Mutable
-- unsafeToForeignPtr0 :: Storable a => MVector s a -> (ForeignPtr a, Int)
--
-- Data.ByteString.Unsafe
-- unsafePackCStringFinalizer :: Ptr Word8 -> Int -> IO () -> IO ByteString
--
-- Data.Vector.Storable.Internal
-- getPtr :: ForeignPtr a -> Ptr a
byteStringFromVector :: M.MVector s Word8 -> Int -> ST s B.ByteString
byteStringFromVector vec size = do
    frozen <- VS.unsafeFreeze vec
    return . B.pack . take size $ VS.toList frozen

setBitCount :: Word8 -> Int -> BoolWriter s ()
setBitCount acc count = S.modify $ \s ->
    s { bitAcc = acc, bitReaded = count }

resetBitCount :: BoolWriter s ()
resetBitCount = setBitCount 0 0

pushByte :: Word8 -> BoolWriter s ()
pushByte v = do
    st <- S.get
    st'@(BoolWriteState { writtenWords = idx })
        <- lift $ flushCurrentBuffer st
    lift $ M.write (wordWrite st') idx v
    S.put $ st' { writtenWords = idx + 1 }

-- | If some bits are not serialized yet, write
-- them in the MSB of a word.
flushWriter :: BoolWriter s ()
flushWriter = do
    st <- S.get
    let count = bitReaded st
    when (count > 0)
         (do let newContext = st { bitAcc = 0, bitReaded = 0 }
             S.put newContext
             pushByte $ bitAcc st `shiftL` (8 - count))

-- | Append some data bits to a Put monad.
writeBits :: Word32     -- ^ The real data to be stored. Actual data should be in the LSB
          -> Int        -- ^ Number of bit to write from 1 to 32
          -> BoolWriter s ()
writeBits d c = do
    currWord <- S.gets bitAcc
    currCount <- S.gets bitReaded
    serialize d c currWord currCount
  where dumpByte 0xFF = pushByte 0xFF >> pushByte 0x00
        dumpByte    i = pushByte i

        serialize bitData bitCount currentWord count
            | bitCount + count == 8 = do
                     resetBitCount
                     dumpByte (fromIntegral $ (currentWord `shiftL` bitCount) .|.
                                                fromIntegral cleanData)

            | bitCount + count < 8 =
                let newVal = currentWord `shiftL` bitCount
                in setBitCount (newVal .|. fromIntegral cleanData) $ count + bitCount

            | otherwise =
                let leftBitCount = 8 - count :: Int
                    highPart = cleanData `shiftR` (bitCount - leftBitCount) :: Word32
                    prevPart = fromIntegral currentWord `shiftL` leftBitCount :: Word32

                    nextMask = (1 `shiftL` (bitCount - leftBitCount)) - 1 :: Word32
                    newData = cleanData .&. nextMask :: Word32
                    newCount = bitCount - leftBitCount :: Int

                    toWrite = fromIntegral $ prevPart .|. highPart :: Word8
                in resetBitCount >> dumpByte toWrite >> serialize newData newCount 0 0

              where cleanMask = (1 `shiftL` bitCount) - 1 :: Word32
                    cleanData = bitData .&. cleanMask     :: Word32

