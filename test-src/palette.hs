module Main where

import           Codec.Picture
import           System.Environment                (getArgs)

-- Some quick and dirty code to test ColorQuant.
main :: IO ()
main = do
  args <- getArgs
  case args of
    (f1:f2:m:l:_) -> do
      dImg <- readImage f1
      let outFile = f2
          m' = read m
          l' = read l
      case dImg of
        Left _ -> putStrLn "Unable to read image."
        Right (ImageRGB8 img) -> savePngImage outFile (ImageRGB8
                               $ (modMedianCutCQ . aDither (masks !! m') l') img)
        Right _ -> putStrLn "Looking for RGB8."
    _ -> putStrLn "Two files required."
