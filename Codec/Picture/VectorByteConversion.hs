{-# LANGUAGE ScopedTypeVariables #-}
module Codec.Picture.VectorByteConversion( blitVector, toByteString  ) where

import Data.Word( Word8 )
import Data.Vector.Storable( Vector, unsafeToForeignPtr )
import Foreign.ForeignPtr.Safe( castForeignPtr )
import Foreign.Storable( Storable, sizeOf )

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as S

blitVector :: Vector Word8 -> Int -> Int -> B.ByteString
blitVector vec atIndex blitSize = S.PS ptr (offset + atIndex) blitSize
  where (ptr, offset, _length) = unsafeToForeignPtr vec

toByteString :: forall a. (Storable a) => Vector a -> B.ByteString
toByteString vec = S.PS (castForeignPtr ptr) offset (len * size)
  where (ptr, offset, len) = unsafeToForeignPtr vec
        size = sizeOf (undefined :: a)

