module Codec.Picture.InternalHelper ( runGet
                                    , runGetStrict
                                    , decode
                                    , getRemainingBytes ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Binary( Binary( get ) )
import Data.Binary.Get( Get, runGetOrFail, getRemainingLazyByteString )

decode :: (Binary a) => B.ByteString -> Either String a
decode = runGetStrict get

runGet :: Get a -> L.ByteString -> Either String a
runGet act = unpack . runGetOrFail act
    where unpack (Left (_, _, str)) = Left str
          unpack (Right (_, _, element)) = Right element

runGetStrict :: Get a -> B.ByteString -> Either String a
runGetStrict act buffer = runGet act $ L.fromChunks [buffer]

getRemainingBytes :: Get B.ByteString
getRemainingBytes = do
    rest <- getRemainingLazyByteString 
    return $ case L.toChunks rest of
        [] -> B.empty
        [a] -> a
        lst -> B.concat lst

