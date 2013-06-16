module Codec.Picture.Jpg.Types( MutableMacroBlock
                              , createEmptyMutableMacroBlock
                              , printMacroBlock
                              ) where

import Control.Monad.ST( ST )
import Foreign.Storable ( Storable )
import qualified Data.Vector.Storable.Mutable as M

import Text.Printf

-- | Macroblock that can be transformed.
type MutableMacroBlock s a = M.STVector s a

{-# INLINE createEmptyMutableMacroBlock #-}
-- | Create a new macroblock with the good array size
createEmptyMutableMacroBlock :: (Storable a, Num a) => ST s (MutableMacroBlock s a)
createEmptyMutableMacroBlock = M.replicate 64 0

printMacroBlock :: (Storable a, PrintfArg a)
                => MutableMacroBlock s a -> ST s String
printMacroBlock block = pLn 0
    where pLn 64 = return "===============================\n"
          pLn i = do
              v <- block `M.unsafeRead` i
              vn <- pLn (i+1)
              return $ printf (if i `mod` 8 == 0 then "\n%5d " else "%5d ") v ++ vn

