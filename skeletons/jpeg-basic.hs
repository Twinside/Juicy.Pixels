import qualified Data.ByteString.Lazy as LB
import System.Environment( getArgs )
import Codec.Picture
import Codec.Picture.Types

transformImage :: Image PixelYCbCr8 -> Image PixelYCbCr8
transformImage img = img -- Transform the image here

transformRGBImage :: Image PixelRGB8 -> Image PixelRGB8
transformRGBImage img = img -- Transform the RGB image here

main :: IO ()
main = do
    commandArguments <- getArgs
    case commandArguments of
      [] -> putStrLn "Not enough arguments"
      (filename : _) -> do
          dynImg <- readImage filename
          case dynImg of
              Left err -> putStrLn err

              -- the Jpeg decoder return an image in the YCbCr colorspace
              Right (ImageYCbCr8 img) ->
                  LB.writeFile (filename ++ "_transformed.jpg")
                        . encodeJpegAtQuality 100 $ transformImage img

              -- If you prefer to work in the RGB8 colorspace, uncomment
              -- the following
              {-
              Right (ImageYCbCr8 img) ->
                  writePng (filename ++ "_transformed.png")
                            . transformRGBImage $ convertImage img
                -- -}

              Right _ -> putStrLn "Unhandled image colorspace"

