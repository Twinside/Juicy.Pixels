module Codec.Picture.Jpg.Types( MutableMacroBlock
                              , createEmptyMutableMacroBlock
                              , mutate
                              , (!!!),  (.!!!.), (.<-.)
                              ) where

import Control.Monad.ST( ST )
import Data.Int( Int16 )
import Foreign.Storable ( Storable )
import Control.Monad.Primitive ( PrimState, PrimMonad )
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

{-# INLINE (!!!) #-}
(!!!) :: (Storable e) => V.Vector e -> Int -> e
(!!!) = V.unsafeIndex

{-# INLINE (.!!!.) #-}
(.!!!.) :: (PrimMonad m, Storable a)
        => M.STVector (PrimState m) a -> Int -> m a
(.!!!.) = M.unsafeRead

{-# INLINE (.<-.) #-}
(.<-.) :: (PrimMonad m, Storable a)
       => M.STVector (PrimState m) a -> Int -> a -> m ()
(.<-.) = M.unsafeWrite

-- | Macroblock that can be transformed.
type MutableMacroBlock s a = M.STVector s a

{-# INLINE createEmptyMutableMacroBlock #-}
-- | Create a new macroblock with the good array size
createEmptyMutableMacroBlock :: ST s (MutableMacroBlock s Int16)
createEmptyMutableMacroBlock = M.replicate 64 0

{-# INLINE mutate #-}
-- | Return the transformed block
mutate :: (Int -> Int16 -> Int16)   -- ^ The updating function
       -> MutableMacroBlock s Int16 -> ST s (MutableMacroBlock s Int16)
mutate f block = update 0 >> return block
   where updateVal i = (block .!!!. i) >>= (block .<-. i) . f i

         update 63 = updateVal 63
         update n  = updateVal n >> update (n + 1)
