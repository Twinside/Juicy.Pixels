
import Codec.Picture.Png
import Codec.Picture.Types
import Codec.Picture.ColorConversion

import System.Environment
import qualified Data.ByteString.Lazy as Lb

main :: IO ()
main = do
    args <- getArgs
    file <- Lb.readFile $ args !! 0
    let img = loadRawPng file
    print $ header img
    mapM_ (\c -> do
            putStrLn $ (show $ chunkType c)
                     ++ " Size: " ++ (show $ chunkLength c)) 
                     $ chunks img
