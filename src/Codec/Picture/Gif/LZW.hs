{-# LANGUAGE CPP #-}
module Codec.Picture.Gif.LZW( decodeLzw, decodeLzwTiff ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>) )
#endif

import Data.Word( Word8 )
import Control.Monad( when, unless )

import Data.Bits( (.&.) )

import Control.Monad.ST( ST )
import Control.Monad.Trans.Class( MonadTrans, lift )

import Foreign.Storable ( Storable )

import qualified Data.ByteString as B
import qualified Data.Vector.Storable.Mutable as M

import Codec.Picture.BitWriter

{-# INLINE (.!!!.) #-}
(.!!!.) :: (Storable a) => M.STVector s a -> Int -> ST s a
(.!!!.) = M.unsafeRead 
        {-M.read-}

{-# INLINE (..!!!..) #-}
(..!!!..) :: (MonadTrans t, Storable a)
          => M.STVector s a -> Int -> t (ST s) a
(..!!!..) v idx = lift $ v .!!!. idx

{-# INLINE (.<-.) #-}
(.<-.) :: (Storable a) => M.STVector s a -> Int -> a -> ST s ()
(.<-.) = M.unsafeWrite 
         {-M.write-}

{-# INLINE (..<-..) #-}
(..<-..) :: (MonadTrans t, Storable a)
         => M.STVector s a -> Int -> a -> t (ST s) ()
(..<-..) v idx = lift . (v .<-. idx)


duplicateData :: (MonadTrans t, Storable a)
              => M.STVector s a -> M.STVector s a
              -> Int -> Int -> Int -> t (ST s) ()
duplicateData src dest sourceIndex size destIndex = lift $ aux sourceIndex destIndex
  where endIndex = sourceIndex + size
        aux i _ | i == endIndex  = return ()
        aux i j = do
          src .!!!. i >>= (dest .<-. j)
          aux (i + 1) (j + 1)

rangeSetter :: (Storable a, Num a)
            => Int -> M.STVector s a
            -> ST s (M.STVector s a)
rangeSetter count vec = aux 0
  where aux n | n == count = return vec
        aux n = (vec .<-. n) (fromIntegral n) >> aux (n + 1)

decodeLzw :: B.ByteString -> Int -> Int -> M.STVector s Word8
          -> BoolReader s ()
decodeLzw str maxBitKey initialKey outVec = do
    setDecodedString str
    lzw GifVariant maxBitKey initialKey 0 outVec

isOldTiffLZW :: B.ByteString -> Bool
isOldTiffLZW str = firstByte == 0 && secondByte == 1
    where firstByte = str `B.index` 0
          secondByte = (str `B.index` 1) .&. 1

decodeLzwTiff :: B.ByteString -> M.STVector s Word8 -> Int
              -> BoolReader s()
decodeLzwTiff str outVec initialWriteIdx = do
    if isOldTiffLZW str then
      setDecodedString str
    else
      setDecodedStringMSB str
    let variant | isOldTiffLZW str = OldTiffVariant
                | otherwise = TiffVariant
    lzw variant 12 9 initialWriteIdx outVec

data TiffVariant =
      GifVariant
    | TiffVariant
    | OldTiffVariant
    deriving Eq

-- | Gif image constraint from spec-gif89a, code size max : 12 bits.
lzw :: TiffVariant -> Int -> Int -> Int -> M.STVector s Word8
    -> BoolReader s ()
lzw variant nMaxBitKeySize initialKeySize initialWriteIdx outVec = do
    -- Allocate buffer of maximum size.
    lzwData <- lift (M.replicate maxDataSize 0) >>= resetArray
    lzwOffsetTable <- lift (M.replicate tableEntryCount 0) >>= resetArray
    lzwSizeTable <- lift $ M.replicate tableEntryCount 0
    lift $ lzwSizeTable `M.set` 1

    let firstVal code = do
            dataOffset <- lzwOffsetTable ..!!!.. code
            lzwData ..!!!.. dataOffset

        writeString at code = do
            dataOffset <- lzwOffsetTable ..!!!.. code
            dataSize   <- lzwSizeTable   ..!!!.. code

            when (at + dataSize <= maxWrite) $
                 duplicateData lzwData outVec dataOffset dataSize at

            return dataSize

        addString pos at code val = do
            dataOffset <- lzwOffsetTable ..!!!.. code
            dataSize   <- lzwSizeTable   ..!!!.. code

            when (pos < tableEntryCount) $ do
              (lzwOffsetTable ..<-.. pos) at
              (lzwSizeTable ..<-.. pos) $ dataSize + 1

            when (at + dataSize + 1 <= maxDataSize) $ do
              duplicateData lzwData lzwData dataOffset dataSize at
              (lzwData ..<-.. (at + dataSize)) val

            return $ dataSize + 1

        maxWrite = M.length outVec
        loop outWriteIdx writeIdx dicWriteIdx codeSize oldCode code
          | outWriteIdx >= maxWrite = return ()
          | code == endOfInfo = return ()
          | code == clearCode = do
              toOutput <- getNextCode startCodeSize
              unless (toOutput == endOfInfo) $ do
                dataSize <- writeString outWriteIdx toOutput
                getNextCode startCodeSize >>=
                  loop (outWriteIdx + dataSize)
                       firstFreeIndex firstFreeIndex startCodeSize toOutput

          | otherwise =  do
              (written, dicAdd) <-
                   if code >= writeIdx then do
                     c <- firstVal oldCode
                     wroteSize <- writeString outWriteIdx oldCode
                     (outVec ..<-.. (outWriteIdx + wroteSize)) c
                     addedSize <- addString writeIdx dicWriteIdx oldCode c
                     return (wroteSize + 1, addedSize)
                   else do
                     wroteSize <- writeString outWriteIdx code
                     c <- firstVal code
                     addedSize <- addString writeIdx dicWriteIdx oldCode c
                     return (wroteSize, addedSize)

              let new_code_size = updateCodeSize codeSize $ writeIdx + 1
              getNextCode new_code_size >>=
                loop (outWriteIdx + written)
                     (writeIdx + 1)
                     (dicWriteIdx + dicAdd)
                     new_code_size
                     code

    getNextCode startCodeSize >>=
        loop initialWriteIdx firstFreeIndex firstFreeIndex startCodeSize 0

  where tableEntryCount =  2 ^ min 12 nMaxBitKeySize
        maxDataSize = tableEntryCount `div` 2 * (1 + tableEntryCount) + 1

        isNewTiff = variant == TiffVariant
        (switchOffset,  isTiffVariant) = case variant of
            GifVariant -> (0, False)
            TiffVariant -> (1, True)
            OldTiffVariant -> (0, True)

        initialElementCount = 2 ^ initialKeySize :: Int
        clearCode | isTiffVariant = 256
                  | otherwise = initialElementCount

        endOfInfo | isTiffVariant = 257
                  | otherwise = clearCode + 1

        startCodeSize 
                  | isTiffVariant = initialKeySize
                  | otherwise = initialKeySize + 1

        firstFreeIndex = endOfInfo + 1

        resetArray a = lift $ rangeSetter initialElementCount a

        updateCodeSize codeSize writeIdx
            | writeIdx == 2 ^ codeSize - switchOffset = min 12 $ codeSize + 1
            | otherwise = codeSize

        getNextCode s 
            | isNewTiff = fromIntegral <$> getNextBitsMSBFirst s
            | otherwise = fromIntegral <$> getNextBitsLSBFirst s

