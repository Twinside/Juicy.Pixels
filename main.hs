-- test file, don't care about unused, on the contrary...
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Codec.Picture
import Codec.Picture.Jpg( encodeJpeg )
import Codec.Picture.Gif
import System.Environment

import Data.Serialize
import Data.Word( Word8 )
import Control.Monad( forM_ )
import System.Environment
import System.FilePath
import qualified Data.ByteString as B
import Codec.Picture.Types
import qualified Data.Vector.Storable as V

validTests :: [FilePath]
validTests = 
   ["PngSuite.png", "basi0g01.png", "basi0g02.png", "basi0g04.png", "basi0g08.png",
    "basi0g16.png", "basi2c08.png", "basi2c16.png", "basi3p01.png", "basi3p02.png",
    "basi3p04.png", "basi3p08.png", "basi4a08.png", "basi4a16.png", "basi6a08.png",
    "basi6a16.png", "basn0g01.png", "basn0g02.png", "basn0g04.png", "basn0g08.png",
    "basn0g16.png", "basn2c08.png", "basn2c16.png", "basn3p01.png", "basn3p02.png",
    "basn3p04.png", "basn3p08.png", "basn4a08.png", "basn4a16.png", "basn6a08.png",
    "basn6a16.png", "bgai4a08.png", "bgai4a16.png", "bgan6a08.png", "bgan6a16.png",
    "bgbn4a08.png", "bggn4a16.png", "bgwn6a08.png", "bgyn6a16.png", "ccwn2c08.png",
    "ccwn3p08.png", "cdfn2c08.png", "cdhn2c08.png", "cdsn2c08.png", "cdun2c08.png",
    "ch1n3p04.png", "ch2n3p08.png", "cm0n0g04.png", "cm7n0g04.png", "cm9n0g04.png",
    "cs3n2c16.png", "cs3n3p08.png", "cs5n2c08.png", "cs5n3p08.png", "cs8n2c08.png",
    "cs8n3p08.png", "ct0n0g04.png", "ct1n0g04.png", "cten0g04.png", "ctfn0g04.png",
    "ctgn0g04.png", "cthn0g04.png", "ctjn0g04.png", "ctzn0g04.png", "f00n0g08.png",
    "f00n2c08.png", "f01n0g08.png", "f01n2c08.png", "f02n0g08.png", "f02n2c08.png",
    "f03n0g08.png", "f03n2c08.png", "f04n0g08.png", "f04n2c08.png", "f99n0g04.png",
    "g03n0g16.png", "g03n2c08.png", "g03n3p04.png", "g04n0g16.png", "g04n2c08.png",
    "g04n3p04.png", "g05n0g16.png", "g05n2c08.png", "g05n3p04.png", "g07n0g16.png",
    "g07n2c08.png", "g07n3p04.png", "g10n0g16.png", "g10n2c08.png", "g10n3p04.png",
    "g25n0g16.png", "g25n2c08.png", "g25n3p04.png", "oi1n0g16.png", "oi1n2c16.png",
    "oi2n0g16.png", "oi2n2c16.png", "oi4n0g16.png", "oi4n2c16.png", "oi9n0g16.png",
    "oi9n2c16.png", "pp0n2c16.png", "pp0n6a08.png", "ps1n0g08.png", "ps1n2c16.png",
    "ps2n0g08.png", "ps2n2c16.png", "s01i3p01.png", "s01n3p01.png", "s02i3p01.png",
    "s02n3p01.png", "s03i3p01.png", "s03n3p01.png", "s04i3p01.png", "s04n3p01.png",
    "s05i3p02.png", "s05n3p02.png", "s06i3p02.png", "s06n3p02.png", "s07i3p02.png",
    "s07n3p02.png", "s08i3p02.png", "s08n3p02.png", "s09i3p02.png", "s09n3p02.png",
    "s32i3p04.png", "s32n3p04.png", "s33i3p04.png", "s33n3p04.png", "s34i3p04.png",
    "s34n3p04.png", "s35i3p04.png", "s35n3p04.png", "s36i3p04.png", "s36n3p04.png",
    "s37i3p04.png", "s37n3p04.png", "s38i3p04.png", "s38n3p04.png", "s39i3p04.png",
    "s39n3p04.png", "s40i3p04.png", "s40n3p04.png", "tbbn0g04.png", "tbbn2c16.png",
    "tbbn3p08.png", "tbgn2c16.png", "tbgn3p08.png", "tbrn2c08.png", "tbwn0g16.png",
    "tbwn3p08.png", "tbyn3p08.png", "tp0n0g08.png", "tp0n2c08.png", "tp0n3p08.png",
    "tp1n3p08.png", "z00n2c08.png", "z03n2c08.png", "z06n2c08.png", "z09n2c08.png"]

invalidTests :: [FilePath]
invalidTests = ["xc1n0g08.png", "xc9n2c08.png", "xcrn0g04.png", "xcsn0g01.png", "xd0n2c08.png",
                "xd3n2c08.png", "xdtn0g01.png", "xhdn0g08.png", "xlfn0g04.png", "xs1n0g01.png",
                "xs2n0g01.png", "xs4n0g01.png", "xs7n0g01.png", "xd9n2c08.png"]

{-exportBmpWitness :: IO ()-}
{-exportBmpWitness = writeBitmap "wintess.bmp" $ img 232 241-}
    {-where img w h = array ((0,0), (w - 1, h - 1)) $ pixels w h-}
          {-pixels w h = [((x,y), pixel x y) | y <- [0 .. h-1], x <- [0 .. w-1] ]-}
          {-pixel x y = PixelRGBA8 128 (fromIntegral x) (fromIntegral y) 255-}

greyScaleWitness :: Image Pixel8
greyScaleWitness = img 232 241
    where img w h = Image w h $ V.fromListN (w * h) $ pixels w h
          pixels w h = [pixel x y | y <- [0 .. h-1], x <- [0 .. w-1] ]
          pixel x y = truncate $ sqrt dist
                where xf = fromIntegral $ x - 100 :: Int
                      yf = fromIntegral $ y - 100
                      dist = (fromIntegral $ xf * xf + yf * yf) :: Double
jpegValidTests :: [FilePath]
jpegValidTests = [ "explore_jpeg.jpg"
                 , "16x16jpeg.jpg", "8x8jpeg.jpg", "avatar.jpg"
                 , "fenek.jpg", "JPEG_example_JPG_RIP_001.jpg"
                 , "JPEG_example_JPG_RIP_010.jpg", "JPEG_example_JPG_RIP_025.jpg"
                 , "JPEG_example_JPG_RIP_050.jpg", "JPEG_example_JPG_RIP_100.jpg"
                 , "sheep.jpg"
                 ]
 
bmpValidTests :: [FilePath]
bmpValidTests = ["simple_bitmap_24bits.bmp"]

validationJpegEncode :: Image PixelYCbCr8 -> B.ByteString
validationJpegEncode = encodeJpegAtQuality 100

gifToImg :: FilePath -> IO ()
gifToImg path = do
    rez <- readGifImages path
    case rez of
        Left err -> putStrLn $ "Error : " ++ err
        Right v -> forM_ (zip [0..] v) $ \(i :: Int, img) -> do
            let ycbcr = convertImage img
                jpg = validationJpegEncode ycbcr
                png = encodePng img
                bmp = encodeBitmap img
            putStrLn $ "PixelRGB8 : " ++ path

            putStrLn "-> JPG"
            B.writeFile (path ++ "_" ++ show i ++ "._fromYCbCr8.jpg") jpg
            putStrLn "-> BMP"
            B.writeFile (path ++ "_" ++ show i ++ "._fromYCbCr8.bmp") bmp
            putStrLn "-> PNG"
            B.writeFile (path ++ "_" ++ show i ++ "._fromYCbCr8.png") png

imgToImg :: FilePath -> IO ()
imgToImg path = do
    rez <- readImage path
    case rez of
        Right (ImageYCbCr8 img) -> do
            let rgb = convertImage img :: Image PixelRGB8
                jpg = validationJpegEncode img
                png = encodePng rgb
                bmp = encodeBitmap rgb
            putStrLn $ "YCbCr : " ++ path
            putStrLn "-> JPG"
            B.writeFile (path ++ "._fromYCbCr8.jpg") jpg
            putStrLn "-> BMP"
            B.writeFile (path ++ "._fromYCbCr8.bmp") bmp
            putStrLn "-> PNG"
            B.writeFile (path ++ "._fromYCbCr8.png") png

        Right (ImageRGB8 img) -> do
            let jpg = validationJpegEncode (convertImage img)
                png = encodePng img
                bmp = encodeBitmap img
            putStrLn $ "RGB8 : " ++ path
            putStrLn "-> BMP"
            B.writeFile (path ++ "._fromRGB8.bmp") bmp
            putStrLn "-> JPG"
            B.writeFile (path ++ "._fromRGB8.jpg") jpg
            putStrLn "-> PNG"
            B.writeFile (path ++ "._fromRGB8.png") png

        Right (ImageRGBA8 img) -> do
            let bmp = encodeBitmap img
                jpg = validationJpegEncode (convertImage $ dropAlphaLayer img)
                png = encodePng img
            putStrLn $ "RGBA8 : " ++ path
            putStrLn "-> BMP"
            B.writeFile (path ++ ".fromRGBA8.bmp") bmp
            putStrLn "-> JPG"
            B.writeFile (path ++ ".fromRGBA8.jpg") jpg
            putStrLn "-> PNG"
            B.writeFile (path ++ ".fromRGBA8.png") png

        Right (ImageY8 img) -> do
            let bmp = encodeBitmap img
                jpg = validationJpegEncode . convertImage $ (promoteImage img :: Image PixelRGB8)
                png = encodePng img
            putStrLn $ "Y8 : " ++ path
            putStrLn "-> BMP"
            B.writeFile (path ++ "._fromY8.bmp") bmp
            putStrLn "-> JPG"
            B.writeFile (path ++ "._fromY8.jpg") jpg
            putStrLn "-> PNG"
            B.writeFile (path ++ "._fromY8.png") png

        Right (ImageYA8 img) -> do
            let bmp = encodeBitmap $ (promoteImage img :: Image PixelRGB8)
                png = encodePng $ (promoteImage img :: Image PixelRGBA8)
                jpg = validationJpegEncode $ convertImage
                                (promoteImage $ dropAlphaLayer img :: Image PixelRGB8)
            putStrLn $ "YA8 : " ++ path
            putStrLn "-> BMP"
            B.writeFile (path ++ "._fromYA8.bmp") bmp
            putStrLn "-> JPG"
            B.writeFile (path ++ "._fromYA8.jpg") jpg
            putStrLn "-> PNG"
            B.writeFile (path ++ "._fromYA8.png") png

        Left err ->
            error $ "Error loading " ++ path ++ " " ++ show err

toJpg :: String -> Image PixelRGB8 -> IO ()
toJpg name img = do
    let jpg = validationJpegEncode (convertImage img)
    putStrLn "-> JPG"
    B.writeFile (name ++ "._fromRGB8.jpg") jpg

planeSeparationRGB8Test :: IO ()
planeSeparationRGB8Test = do
    rez <- readImage ("tests" </> "planeseparation.png")
    case rez of
       Left _ -> putStrLn "can't load separation file"
       Right (ImageRGB8 img) -> do
           B.writeFile ("tests" </> "rgb8_red.png") . encodePng $ extractComponent PlaneRed img
           B.writeFile ("tests" </> "rgb8_green.png") . encodePng $ extractComponent PlaneGreen img
           B.writeFile ("tests" </> "rgb8_blue.png") . encodePng $ extractComponent PlaneBlue img

       Right _ -> putStrLn "Wrong image file format"

planeSeparationRGBA8Test :: IO ()
planeSeparationRGBA8Test = do
    rez <- readImage ("tests" </> "planeseparationrgba.png")
    case rez of
       Left _ -> putStrLn "can't load separation file"
       Right (ImageRGBA8 img) -> do
           B.writeFile ("tests" </> "rgba8_red.png") . encodePng $ extractComponent PlaneRed img
           B.writeFile ("tests" </> "rgba8_green.png") . encodePng $ extractComponent PlaneGreen img
           B.writeFile ("tests" </> "rgba8_blue.png") . encodePng $ extractComponent PlaneBlue img
           B.writeFile ("tests" </> "rgba8_alpha.png") . encodePng $ extractComponent PlaneAlpha img

       Right _ -> putStrLn "Wrong image file format"

planeSeparationYA8Test :: IO ()
planeSeparationYA8Test = do
    let img = generateImage generator 256 256
        generator x y = PixelYA8 (fromIntegral $ x `mod` 256) (fromIntegral $ ((y `div` 4) `mod` 2) * 255)
    B.writeFile ("tests" </> "ya8_gray.png") . encodePng $ extractComponent PlaneLuma img
    B.writeFile ("tests" </> "ya8_alpha.png") . encodePng $ extractComponent PlaneAlpha img
    B.writeFile ("tests" </> "ya8_combined.png") $ encodePng img

gifTest :: [FilePath]
gifTest = ["Gif_pixel_cube.gif", "animated.gif", "magceit.gif"]

main :: IO ()
main = do 
    putStrLn ">>>> Valid instances"
    {-toJpg "white" $ generateImage (\_ _ -> PixelRGB8 255 255 255) 16 16-}
    {-toJpg "black" $ generateImage (\_ _ -> PixelRGB8 0 0 0) 16 16-}
    {-toJpg "test" $ generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 255)-}
                                        {-128 128-}
    {-mapM_ (imgToImg . (("tests" </> "bmp") </>)) bmpValidTests-}
    {-mapM_ (imgToImg . (("tests" </> "pngsuite") </>)) ("huge.png" : validTests)-}
    {-mapM_ (imgToImg . (("tests" </> "jpeg") </>)) (jpegValidTests)-}
    {-mapM_ (imgToImg . (("tests" </> "jpeg") </>)) ["huge.jpg" ]-}

    mapM_ (gifToImg . (("tests" </> "gif") </>)) gifTest

    {-planeSeparationRGB8Test -}
    {-planeSeparationRGBA8Test -}
    {-planeSeparationYA8Test -}

    {-putStrLn "\n>>>> invalid instances"-}
    {-mapM_ (convertPngToBmpBad . (("tests" </> "pngsuite") </>)) invalidTests-}
    return ()

