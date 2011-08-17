{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
-- | Module providing the basic types for image manipulation in the library.
module Codec.Picture.Types( Image
                          , Pixel2
                          , Pixel8
                          , Pixel24
                          , Pixel24Alpha
                          , rgb
                          , rgba
                          ) where

import Control.Monad.ST
import Data.Word
import Data.Array.Unboxed
import Data.Array.Base

import GHC.ST( ST(..) )
import GHC.Exts
import GHC.Word		( Word8(..) )
type Image a = UArray (Word32, Word32) a

type Pixel2 = Bool
type Pixel8 = Word8
data Pixel24 = Pixel24 !Word8 !Word8 !Word8
data Pixel24Alpha = Pixel24Alpha !Word8 !Word8 !Word8 !Word8

rgb :: Word8 -> Word8 -> Word8 -> Pixel24
rgb = Pixel24

rgba :: Word8 -> Word8 -> Word8 -> Word8 -> Pixel24Alpha
rgba = Pixel24Alpha

instance MArray (STUArray s) Pixel24Alpha (ST s) where
    {-# INLINE getBounds #-}
    getBounds (STUArray l u _ _) = return (l,u)
    {-# INLINE getNumElements #-}
    getNumElements (STUArray _ _ n _) = return n
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ (l,u) = unsafeNewArraySTUArray_ (l,u) (*# 4#)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = newArray arrBounds (Pixel24Alpha 0 0 0 255)
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ _ marr#) (I# i#) = ST $ \s1# ->
        case i# *# 4# of { idx# ->
        case readWord8Array# marr# idx# s1# of { (# s2#, r# #) ->
        case readWord8Array# marr# (idx# +# 1#) s2# of { (# s3#, g# #) ->
        case readWord8Array# marr# (idx# +# 2#) s3# of { (# s4#, b# #) ->
        case readWord8Array# marr# (idx# +# 3#) s4# of { (# s5#, a# #) ->
            (# s5#, Pixel24Alpha (W8# r#) (W8# g#) (W8# b#) (W8# a#) #)
        } } } } }

    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ _ marr#) (I# i#) (Pixel24Alpha (W8# r) (W8# g) (W8# b) (W8# a)) =
       ST $ \s1# ->
        case i# *# 3# of { idx# ->
        case writeWord8Array# marr# idx# r s1# of { s2# ->
        case writeWord8Array# marr# (idx# +# 1#) g s2# of { s3# ->
        case writeWord8Array# marr# (idx# +# 2#) b s3# of { s4# ->
        case writeWord8Array# marr# (idx# +# 3#) a s3# of { s5# ->
        (# s5#, () #) } } } } }

instance MArray (STUArray s) Pixel24 (ST s) where
    {-# INLINE getBounds #-}
    getBounds (STUArray l u _ _) = return (l,u)
    {-# INLINE getNumElements #-}
    getNumElements (STUArray _ _ n _) = return n
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ (l,u) = unsafeNewArraySTUArray_ (l,u) (*# 3#)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = newArray arrBounds (Pixel24 0 0 0)
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ _ marr#) (I# i#) = ST $ \s1# ->
        case i# *# 4# of { idx# ->
        case readWord8Array# marr# idx# s1# of { (# s2#, r# #) ->
        case readWord8Array# marr# (idx# +# 1#) s2# of { (# s3#, g# #) ->
        case readWord8Array# marr# (idx# +# 2#) s3# of { (# s4#, b# #) ->
            (# s4#, Pixel24 (W8# r#) (W8# g#) (W8# b#) #)
        } } } }

    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ _ marr#) (I# i#) (Pixel24 (W8# r) (W8# g) (W8# b)) = ST $ \s1# ->
        case i# *# 3# of { idx# ->
        case writeWord8Array# marr# idx# r s1# of { s2# ->
        case writeWord8Array# marr# (idx# +# 1#) g s2# of { s3# ->
        case writeWord8Array# marr# (idx# +# 2#) b s3# of { s4# ->
        (# s4#, () #) } } } }

instance IArray UArray Pixel24 where
    {-# INLINE bounds #-}
    bounds (UArray l u _ _) = (l,u)
    {-# INLINE numElements #-}
    numElements (UArray _ _ n _) = n
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies $ Pixel24 0 0 0)
#ifdef __GLASGOW_HASKELL__
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ _ arr#) (I# i#) = 
        case i# *# 3# of { idx# ->
            Pixel24 (W8# (indexWord8Array# arr# idx#))
                    (W8# (indexWord8Array# arr# (idx# +# 1#)))
                    (W8# (indexWord8Array# arr# (idx# +# 2#))) }
#endif
#ifdef __HUGS__
    unsafeAt = unsafeAtBArray
#endif
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f initialValue lu ies = runST (unsafeAccumArrayUArray f initialValue lu ies)
