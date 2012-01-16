module Codec.Picture.Jpg.Types( MutableMacroBlock
                              , createEmptyMutableMacroBlock
                              ) where

import Control.Monad.ST( ST )
import Data.Int( Int16 )
import qualified Data.Vector.Storable.Mutable as M

-- | Macroblock that can be transformed.
type MutableMacroBlock s a = M.STVector s a

{-# INLINE createEmptyMutableMacroBlock #-}
-- | Create a new macroblock with the good array size
createEmptyMutableMacroBlock :: ST s (MutableMacroBlock s Int16)
createEmptyMutableMacroBlock = M.replicate 64 0

