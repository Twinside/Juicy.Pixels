module Codec.Picture.InternalHelper ( runGet, runGetStrict, decode ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Binary( Binary( get ) )
import Data.Binary.Get( Get, runGetOrFail )

decode :: (Binary a) => B.ByteString -> Either String a
decode = runGetStrict get

runGet :: Get a -> L.ByteString -> Either String a
runGet act = unpack . runGetOrFail act
    where unpack (Left (_, _, str)) = Left str
          unpack (Right (_, _, element)) = Right element

runGetStrict :: Get a -> B.ByteString -> Either String a
runGetStrict act buffer = runGet act $ L.fromChunks [buffer]

