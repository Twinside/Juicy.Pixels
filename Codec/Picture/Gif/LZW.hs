module Codec.Picture.Gif.LZW( decodeLzw, lzw ) where

import Data.Word( Word8 )
import Control.Applicative( (<$>) )

{-import Control.Monad.ST( ST )-}
import Control.Monad.Trans.Class( MonadTrans, lift )
import Control.Monad.Primitive ( PrimState, PrimMonad )

import Foreign.Storable ( Storable )

import qualified Data.ByteString as B
import qualified Data.Vector.Storable.Mutable as M

import Codec.Picture.BitWriter

import Debug.Trace
import Text.Printf

--------------------------------------------------
----            My LZW
--------------------------------------------------
{-# INLINE (.!!!.) #-}
(.!!!.) :: (PrimMonad m, Storable a)
        => M.STVector (PrimState m) a -> Int -> m a
(.!!!.) = M.read
          -- M.unsafeRead

{-# INLINE (..!!!..) #-}
(..!!!..) :: (MonadTrans t, PrimMonad m, Storable a)
        => M.STVector (PrimState m) a -> Int -> t m a
(..!!!..) v idx = lift $ v .!!!. idx

{-# INLINE (.<-.) #-}
(.<-.) :: (PrimMonad m, Storable a)
       => M.STVector (PrimState m) a -> Int -> a -> m ()
(.<-.) = M.write 
         -- M.unsafeWrite

{-# INLINE (..<-..) #-}
(..<-..) :: (MonadTrans t, PrimMonad m, Storable a)
         => M.STVector (PrimState m) a -> Int -> a -> t m ()
(..<-..) v idx = lift . (v .<-. idx)


duplicateData :: (MonadTrans t, PrimMonad m, Storable a)
              => M.STVector (PrimState m) a -> M.STVector (PrimState m) a
              -> Int -> Int -> Int -> t m ()
duplicateData src dest sourceIndex size destIndex = trace (printf "[%d:] <- [%3d:]:%d" destIndex sourceIndex size) $ lift $ aux 0
  where aux n | n == size = return ()
        aux n = do
          src .!!!. sourceIndex >>= (dest .<-. destIndex)
          aux (n + 1)

rangeSetter :: (PrimMonad m, Storable a, Num a)
            => Int -> M.STVector (PrimState m) a
            -> m (M.STVector (PrimState m) a)
rangeSetter count vec = aux 0
  where aux n | n == count = return vec
        aux n = (vec .<-. n) (fromIntegral n) >> aux (n + 1)

decodeLzw :: B.ByteString -> Int -> Int -> M.STVector s Word8
          -> BoolReader s ()
decodeLzw str maxBitKey initialKey outVec = do
    setDecodedString . trace (concatMap (printf "%2X ") $ B.unpack $ str) $ str
    trace "Mooh" $ lzw maxBitKey initialKey outVec

-- | Gif image constraint from spec-gif89a, code size max : 12 bits.
lzw :: Int -> Int -> M.STVector s Word8
    -> BoolReader s ()
lzw nMaxBitKeySize initialKeySize outVec = do
    -- Allocate buffer of maximum size.
    lzwOffsetTable <- lift (M.new tableEntryCount) >>= resetArray
    lzwSizeTable <- lift $ M.new tableEntryCount
    lift $ lzwSizeTable `M.set` 1
    lzwData <- lift (M.new maxDataSize) >>= resetArray
    let loop outWriteIdx writeIdx dicWriteIdx codeSize code
          | code == endOfInfo = trace "END" $ return ()
          | code == clearCode = trace "RESET" $
              getNextCode startCodeSize >>=
                loop outWriteIdx firstFreeIndex firstFreeIndex startCodeSize

          | otherwise = do
              dataOffset <- lzwOffsetTable ..!!!.. code
              dataSize <- lzwSizeTable  ..!!!.. code

              dicIdx <- if outWriteIdx /= 0
                 then do firstVal <- lzwData ..!!!.. dataOffset
                         (lzwData ..<-.. dicWriteIdx) firstVal
                         return $ dicWriteIdx + 1
                 else return dicWriteIdx

              trace (printf "write:%4d dicIdx:%4d size:%4d code:%4d (s:%3d, o:%4d)"
                                        outWriteIdx dicIdx codeSize code dataSize dataOffset) $ duplicateData lzwData outVec dataOffset dataSize outWriteIdx

              duplicateData lzwData lzwData dataOffset dataSize dicIdx

              (lzwSizeTable ..<-.. writeIdx) $ dataSize + 1
              (lzwOffsetTable ..<-.. writeIdx) dicIdx

              getNextCode codeSize >>=
                loop (outWriteIdx + dataSize)
                     (writeIdx + 1)
                     (dicIdx + dataSize) (updateCodeSize codeSize $ writeIdx + 1)

    getNextCode startCodeSize >>=
        loop 0 firstFreeIndex firstFreeIndex startCodeSize

  where tableEntryCount =  2 ^ min 12 nMaxBitKeySize
        maxDataSize = tableEntryCount `div` 2 * (1 + tableEntryCount)

        initialElementCount = 2 ^ initialKeySize :: Int
        clearCode = initialElementCount
        endOfInfo = clearCode + 1
        startCodeSize = initialKeySize + 1

        firstFreeIndex = endOfInfo + 1

        resetArray a = lift $ rangeSetter initialElementCount a

        updateCodeSize codeSize writeIdx
            | writeIdx == 2 ^ codeSize = min 12 $ codeSize + 1
            | otherwise = codeSize

        getNextCode s = fromIntegral <$> getNextBits s


