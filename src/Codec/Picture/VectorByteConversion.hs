{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
module Codec.Picture.VectorByteConversion( blitVector
                                         , toByteString
                                         , imageFromUnsafePtr ) where

import Data.Word( Word8 )
import Data.Vector.Storable( Vector, unsafeToForeignPtr, unsafeFromForeignPtr0 )
import Foreign.Storable( Storable, sizeOf )

#if !MIN_VERSION_base(4,8,0)
import Foreign.ForeignPtr.Safe( ForeignPtr, castForeignPtr )
#else
import Foreign.ForeignPtr( ForeignPtr, castForeignPtr )
#endif


import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as S

import Codec.Picture.Types

mkBS :: ForeignPtr Word8 -> Int -> Int -> S.ByteString
#if MIN_VERSION_bytestring(0,11,0)
mkBS fptr off = S.BS (fptr `S.plusForeignPtr` off)
#else
mkBS = S.PS
#endif

blitVector :: Vector Word8 -> Int -> Int -> B.ByteString
blitVector vec atIndex = mkBS ptr (offset + atIndex)
  where (ptr, offset, _length) = unsafeToForeignPtr vec

toByteString :: forall a. (Storable a) => Vector a -> B.ByteString
toByteString vec = mkBS (castForeignPtr ptr) offset (len * size)
  where (ptr, offset, len) = unsafeToForeignPtr vec
        size = sizeOf (undefined :: a)

-- | Import a image from an unsafe pointer
-- The pointer must have a size of width * height * componentCount px
imageFromUnsafePtr :: forall px
                    . (Pixel px, (PixelBaseComponent px) ~ Word8)
                   => Int -- ^ Width in pixels
                   -> Int -- ^ Height in pixels
                   -> ForeignPtr Word8 -- ^ Pointer to the raw data
                   -> Image px
imageFromUnsafePtr width height ptr =
    Image width height $ unsafeFromForeignPtr0 ptr size
      where compCount = componentCount (undefined :: px)
            size = width * height * compCount

