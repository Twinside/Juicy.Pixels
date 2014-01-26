{-# LANGUAGE CPP #-}
module Codec.Picture.InternalHelper ( runGet
                                    , runGetStrict
                                    , decode
                                    , getRemainingBytes
                                    , getRemainingLazyBytes ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Binary( Binary( get ) )
import Data.Binary.Get( Get
                      , getRemainingLazyByteString
                      )
import qualified Data.Binary.Get as G

#if MIN_VERSION_binary(0,6,4)
#else
import Control.Applicative( (<$>) )
import qualified Control.Exception as E
-- I feel so dirty. :(
import System.IO.Unsafe( unsafePerformIO )
#endif

decode :: (Binary a) => B.ByteString -> Either String a
decode = runGetStrict get

runGet :: Get a -> L.ByteString -> Either String a
#if MIN_VERSION_binary(0,6,4)
runGet act = unpack . G.runGetOrFail act
    where unpack (Left (_, _, str)) = Left str
          unpack (Right (_, _, element)) = Right element
#else
runGet act str = unsafePerformIO $ E.catch
    (Right <$> E.evaluate (G.runGet act str))
    (\msg -> return . Left $ show (msg :: E.SomeException))
#endif

runGetStrict :: Get a -> B.ByteString -> Either String a
runGetStrict act buffer = runGet act $ L.fromChunks [buffer]

getRemainingBytes :: Get B.ByteString
getRemainingBytes = do
    rest <- getRemainingLazyByteString 
    return $ case L.toChunks rest of
        [] -> B.empty
        [a] -> a
        lst -> B.concat lst

getRemainingLazyBytes :: Get L.ByteString
getRemainingLazyBytes = getRemainingLazyByteString 

