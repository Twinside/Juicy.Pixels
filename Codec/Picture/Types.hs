{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnboxedTuples #-}
-- | Module providing the basic types for image manipulation in the library.
-- Defining the types used to store all those _Juicy Pixels_
module Codec.Picture.Types( -- * Types
                            -- ** Image types
                            Image
                          , DynamicImage( .. )
                            -- ** Pixel types
                          , Pixel2
                          , Pixel8
                          , PixelYA8( .. )
                          , PixelRGB8( .. )
                          , PixelRGBA8( .. )
                            -- * Helper functions
                          , swapBlueRed 
                          ) where

import Control.Applicative
import Control.Monad.ST
import Data.Word
import Data.Array.Unboxed
import Data.Array.Base
import Data.Serialize
import GHC.ST( ST(..) )
import GHC.Exts
import GHC.Word		( Word8(..) )

-- | Image or pixel buffer, the coordinates are assumed to start
-- from the upper-left corner of the image, with the horizontal
-- position first, then the vertical one.
type Image a = UArray (Word32, Word32) a

-- | Type allowing the loading of an image with different pixel
-- structures
data DynamicImage =
       -- | A greyscale image.
       ImageY8   (Image Pixel8)
       -- | An image in greyscale with an alpha channel.
     | ImageYA8  (Image PixelYA8)
       -- | An image in true color.
     | ImageRGB8 (Image PixelRGB8)
       -- | An image in true color and an alpha channel.
     | ImageRGBA8 (Image PixelRGBA8)

type Pixel2 = Bool

-- | Simple alias for greyscale value in 8 bits.
type Pixel8 = Word8

-- | Pixel type storing Luminance (Y) and alpha information
-- on 8 bits.
-- Value are stored in the following order :
--
--  * Luminance
--
--  * Alpha
--
data PixelYA8 = PixelYA8 !Word8  -- Luminance
                         !Word8  -- Alpha value

-- | Pixel type storing classic pixel on 8 bits
-- Value are stored in the following order :
--
--  * Red
--
--  * Green
--
--  * Blue
--
data PixelRGB8 = PixelRGB8 !Word8 -- Red
                           !Word8 -- Green
                           !Word8 -- Blue

-- | Pixel storing data in the YCbCr colorspace,
-- value are stored in teh following order :
--
--  * Y (luminance)
--
--  * Cr
--
--  * Cb
--
data PixelYCbCr = PixelYCbCr !Word8 -- Y luminance
                             !Word8 -- Cr red difference
                             !Word8 -- Cb blue difference

-- | Pixel type storing a classic pixel, with an alpha component.
-- Values are stored in the following order
--
--  * Red
--
--  * Green
--
--  * Blue
--
-- * Alpha
data PixelRGBA8 = PixelRGBA8 !Word8 -- Red
                             !Word8 -- Green
                             !Word8 -- Blue
                             !Word8 -- Alpha

instance Serialize PixelYA8 where
    put (PixelYA8 y a) = put y >> put a
    get = PixelYA8 <$> get <*> get

instance Serialize PixelRGB8 where
    put (PixelRGB8 r g b) = put r >> put g >> put b
    get = PixelRGB8 <$> get <*> get <*> get

instance Serialize PixelYCbCr where
    put (PixelYCbCr y cb cr) = put y >> put cb >> put cr
    get = PixelYCbCr <$> get <*> get <*> get

-- | Helper function to let put color in the "windows" order
-- used in the Bitmap file format.
swapBlueRed :: PixelRGBA8 -> PixelRGBA8
swapBlueRed (PixelRGBA8 r g b a) = PixelRGBA8 b g r a

instance Serialize PixelRGBA8 where
    put (PixelRGBA8 r g b a) = put b >> put g >> put r >> put a
    get = PixelRGBA8 <$> get <*> get <*> get <*> get

instance MArray (STUArray s) PixelRGBA8 (ST s) where
    {-# INLINE getBounds #-}
    getBounds (STUArray l u _ _) = return (l,u)
    {-# INLINE getNumElements #-}
    getNumElements (STUArray _ _ n _) = return n
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ (l,u) = unsafeNewArraySTUArray_ (l,u) (*# 4#)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = newArray arrBounds (PixelRGBA8 0 0 0 255)
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ _ marr#) (I# i#) = ST $ \s1# ->
        case i# *# 4# of { idx# ->
        case readWord8Array# marr# idx# s1# of { (# s2#, r# #) ->
        case readWord8Array# marr# (idx# +# 1#) s2# of { (# s3#, g# #) ->
        case readWord8Array# marr# (idx# +# 2#) s3# of { (# s4#, b# #) ->
        case readWord8Array# marr# (idx# +# 3#) s4# of { (# s5#, a# #) ->
            (# s5#, PixelRGBA8 (W8# r#) (W8# g#) (W8# b#) (W8# a#) #)
        } } } } }

    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ _ marr#) (I# i#) (PixelRGBA8 (W8# r) (W8# g) (W8# b) (W8# a)) =
       ST $ \s1# ->
        case i# *# 4# of { idx# ->
        case writeWord8Array# marr# idx# r s1# of { s2# ->
        case writeWord8Array# marr# (idx# +# 1#) g s2# of { s3# ->
        case writeWord8Array# marr# (idx# +# 2#) b s3# of { s4# ->
        case writeWord8Array# marr# (idx# +# 3#) a s4# of { s5# ->
        (# s5#, () #) } } } } }


instance MArray (STUArray s) PixelYCbCr (ST s) where
    {-# INLINE getBounds #-}
    getBounds (STUArray l u _ _) = return (l,u)
    {-# INLINE getNumElements #-}
    getNumElements (STUArray _ _ n _) = return n
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ (l,u) = unsafeNewArraySTUArray_ (l,u) (*# 3#)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = newArray arrBounds (PixelYCbCr 0 0 0)
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ _ marr#) (I# i#) = ST $ \s1# ->
        case i# *# 3# of { idx# ->
        case readWord8Array# marr# idx# s1# of { (# s2#, r# #) ->
        case readWord8Array# marr# (idx# +# 1#) s2# of { (# s3#, g# #) ->
        case readWord8Array# marr# (idx# +# 2#) s3# of { (# s4#, b# #) ->
            (# s4#, PixelYCbCr (W8# r#) (W8# g#) (W8# b#) #)
        } } } }

    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ _ marr#) (I# i#) (PixelYCbCr (W8# y) (W8# cb) (W8# cr)) = ST $ \s1# ->
        case i# *# 3# of { idx# ->
        case writeWord8Array# marr# idx# y s1# of { s2# ->
        case writeWord8Array# marr# (idx# +# 1#) cb s2# of { s3# ->
        case writeWord8Array# marr# (idx# +# 2#) cr s3# of { s4# ->
        (# s4#, () #) } } } }

instance IArray UArray PixelYCbCr where
    {-# INLINE bounds #-}
    bounds (UArray l u _ _) = (l,u)
    {-# INLINE numElements #-}
    numElements (UArray _ _ n _) = n
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies $ PixelYCbCr 0 0 0)
#ifdef __GLASGOW_HASKELL__
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ _ arr#) (I# i#) = 
        case i# *# 3# of { idx# ->
            PixelYCbCr (W8# (indexWord8Array# arr# idx#))
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

instance MArray (STUArray s) PixelRGB8 (ST s) where
    {-# INLINE getBounds #-}
    getBounds (STUArray l u _ _) = return (l,u)
    {-# INLINE getNumElements #-}
    getNumElements (STUArray _ _ n _) = return n
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ (l,u) = unsafeNewArraySTUArray_ (l,u) (*# 3#)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = newArray arrBounds (PixelRGB8 0 0 0)
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ _ marr#) (I# i#) = ST $ \s1# ->
        case i# *# 3# of { idx# ->
        case readWord8Array# marr# idx# s1# of { (# s2#, r# #) ->
        case readWord8Array# marr# (idx# +# 1#) s2# of { (# s3#, g# #) ->
        case readWord8Array# marr# (idx# +# 2#) s3# of { (# s4#, b# #) ->
            (# s4#, PixelRGB8 (W8# r#) (W8# g#) (W8# b#) #)
        } } } }

    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ _ marr#) (I# i#) (PixelRGB8 (W8# r) (W8# g) (W8# b)) = ST $ \s1# ->
        case i# *# 3# of { idx# ->
        case writeWord8Array# marr# idx# r s1# of { s2# ->
        case writeWord8Array# marr# (idx# +# 1#) g s2# of { s3# ->
        case writeWord8Array# marr# (idx# +# 2#) b s3# of { s4# ->
        (# s4#, () #) } } } }

instance IArray UArray PixelRGB8 where
    {-# INLINE bounds #-}
    bounds (UArray l u _ _) = (l,u)
    {-# INLINE numElements #-}
    numElements (UArray _ _ n _) = n
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies $ PixelRGB8 0 0 0)
#ifdef __GLASGOW_HASKELL__
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ _ arr#) (I# i#) = 
        case i# *# 3# of { idx# ->
            PixelRGB8 (W8# (indexWord8Array# arr# idx#))
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

instance IArray UArray PixelRGBA8 where
    {-# INLINE bounds #-}
    bounds (UArray l u _ _) = (l,u)
    {-# INLINE numElements #-}
    numElements (UArray _ _ n _) = n
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies $ PixelRGBA8 0 0 0 255)
#ifdef __GLASGOW_HASKELL__
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ _ arr#) (I# i#) = 
        case i# *# 4# of { idx# ->
            PixelRGBA8
                    (W8# (indexWord8Array# arr# idx#))
                    (W8# (indexWord8Array# arr# (idx# +# 1#)))
                    (W8# (indexWord8Array# arr# (idx# +# 2#)))
                    (W8# (indexWord8Array# arr# (idx# +# 3#))) }
#endif
#ifdef __HUGS__
    unsafeAt = unsafeAtBArray
#endif
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f initialValue lu ies =
        runST (unsafeAccumArrayUArray f initialValue lu ies)

instance MArray (STUArray s) PixelYA8 (ST s) where
    {-# INLINE getBounds #-}
    getBounds (STUArray l u _ _) = return (l,u)
    {-# INLINE getNumElements #-}
    getNumElements (STUArray _ _ n _) = return n
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ (l,u) = unsafeNewArraySTUArray_ (l,u) (*# 2#)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = newArray arrBounds (PixelYA8 0 255)
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ _ marr#) (I# i#) = ST $ \s1# ->
        case i# *# 2# of { idx# ->
        case readWord8Array# marr# idx# s1# of { (# s2#, y# #) ->
        case readWord8Array# marr# (idx# +# 1#) s2# of { (# s3#, a# #) ->
            (# s3#, PixelYA8 (W8# y#) (W8# a#) #)
        } } }

    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ _ marr#) (I# i#) (PixelYA8 (W8# y) (W8# a)) = ST $ \s1# ->
        case i# *# 2# of { idx# ->
        case writeWord8Array# marr# idx# y s1# of { s2# ->
        case writeWord8Array# marr# (idx# +# 1#) a s2# of { s3# ->
        (# s3#, () #) } } }

instance IArray UArray PixelYA8 where
    {-# INLINE bounds #-}
    bounds (UArray l u _ _) = (l,u)
    {-# INLINE numElements #-}
    numElements (UArray _ _ n _) = n
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies $ PixelYA8 0 255)
#ifdef __GLASGOW_HASKELL__
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ _ arr#) (I# i#) = 
        case i# *# 2# of { idx# ->
            PixelYA8 (W8# (indexWord8Array# arr# idx#))
                     (W8# (indexWord8Array# arr# (idx# +# 1#))) }
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

