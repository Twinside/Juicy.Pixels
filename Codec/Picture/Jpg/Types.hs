module Codec.Picture.Jpg.Types( MutableMacroBlock
                              , createEmptyMutableMacroBlock
                              , mutate
                              , printMacroBlock
                              , (!!!),  (.!!!.), (.<-.)
                              ) where

import Control.Monad.ST( ST )
import Foreign.Storable ( Storable )
import Control.Monad.Primitive ( PrimState, PrimMonad )
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import Data.Vector.Storable( (!) )

import Text.Printf

{-# INLINE (!!!) #-}
(!!!) :: (Storable e) => V.Vector e -> Int -> e
(!!!) = (!) -- V.unsafeIndex

{-# INLINE (.!!!.) #-}
(.!!!.) :: (PrimMonad m, Storable a)
        => M.STVector (PrimState m) a -> Int -> m a
(.!!!.) = M.read -- M.unsafeRead

{-# INLINE (.<-.) #-}
(.<-.) :: (PrimMonad m, Storable a)
       => M.STVector (PrimState m) a -> Int -> a -> m ()
(.<-.) = M.write -- M.unsafeWrite

-- | Macroblock that can be transformed.
type MutableMacroBlock s a = M.STVector s a

{-# INLINE createEmptyMutableMacroBlock #-}
-- | Create a new macroblock with the good array size
createEmptyMutableMacroBlock :: (Storable a, Num a) => ST s (MutableMacroBlock s a)
createEmptyMutableMacroBlock = M.replicate 64 0

{-# INLINE mutate #-}
-- | Return the transformed block
mutate :: Storable a
       => (Int -> a -> a)   -- ^ The updating function
       -> MutableMacroBlock s a -> ST s (MutableMacroBlock s a)
mutate f block = update 0 >> return block
   where updateVal i = (block .!!!. i) >>= (block .<-. i) . f i

         update 63 = updateVal 63
         update n  = updateVal n >> update (n + 1)

printMacroBlock :: (Storable a, PrintfArg a)
                => MutableMacroBlock s a -> ST s String
printMacroBlock block = pLn 0
    where pLn 64 = return "===============================\n"
          pLn i = do
              v <- block .!!!. i
              vn <- pLn (i+1)
              return $ printf (if i `mod` 8 == 0 then "\n%5d " else "%5d ") v ++ vn

