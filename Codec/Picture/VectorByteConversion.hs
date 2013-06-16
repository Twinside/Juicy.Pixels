module Codec.Picture.VectorByteConversion( blitVector ) where

import Data.Word( Word8 )
import Data.Vector.Storable( Vector, unsafeToForeignPtr )

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as S

blitVector :: Vector Word8 -> Int -> Int -> B.ByteString
blitVector vec atIndex blitSize = S.PS ptr (offset + atIndex) blitSize
  where (ptr, offset, _length) = unsafeToForeignPtr vec

