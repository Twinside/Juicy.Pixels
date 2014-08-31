
import Prelude hiding( writeFile )
import Data.ByteString.Lazy( writeFile )
import Codec.Picture( readImage )
import Codec.Picture.Saving( imageToPng )
import System.Environment( getArgs )

main :: IO ()
main = do
  filename:_ <- getArgs
  img <- readImage filename
  case img of
    Left err -> putStrLn err
    Right final ->
      writeFile (filename ++ ".png") $ imageToPng final

