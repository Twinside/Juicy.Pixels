-- test file, don't care about unused, on the contrary...
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
import Codec.Picture
import Codec.Picture.Jpg( encodeJpeg )
import Codec.Picture.Gif
import Codec.Picture.Tiff
import System.Environment

import Data.Binary
import Data.Char( toLower )
import Data.List( isInfixOf )
import Data.Monoid
import Data.Word( Word8 )
import Control.Monad( forM_, liftM )
import System.FilePath
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Codec.Picture.Types
import Codec.Picture.Saving
import Codec.Picture.HDR
import qualified Data.Vector.Storable as V

import Control.Applicative( (<$>) )
import Criterion.Main
import Control.DeepSeq

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
    "tp1n3p08.png", "z00n2c08.png", "z03n2c08.png", "z06n2c08.png", "z09n2c08.png", "megaman.png"]

invalidTests :: [FilePath]
invalidTests = ["xc1n0g08.png", "xc9n2c08.png", "xcrn0g04.png", "xcsn0g01.png", "xd0n2c08.png",
                "xd3n2c08.png", "xdtn0g01.png", "xhdn0g08.png", "xlfn0g04.png", "xs1n0g01.png",
                "xs2n0g01.png", "xs4n0g01.png", "xs7n0g01.png", "xd9n2c08.png"]

{-exportBmpWitness :: IO ()-}
{-exportBmpWitness = writeBitmap "wintess.bmp" $ img 232 241-}
    {-where img w h = array ((0,0), (w - 1, h - 1)) $ pixels w h-}
          {-pixels w h = [((x,y), pixel x y) | y <- [0 .. h-1], x <- [0 .. w-1] ]-}
          {-pixel x y = PixelRGBA8 128 (fromIntegral x) (fromIntegral y) 255-}

greyScaleWitness :: Int -> Image Pixel8
greyScaleWitness offset = img 232 241
    where img w h = Image w h $ V.fromListN (w * h) $ pixels w h
          pixels w h = [pixel (x + offset) (y + offset)
                                | y <- [0 .. h-1], x <- [0 .. w-1] ]
          pixel x y = truncate $ sqrt dist
                where xf = fromIntegral $ x - 100 :: Int
                      yf = fromIntegral $ y - 100
                      dist = (fromIntegral $ xf * xf + yf * yf) :: Double

gifAnimationTest :: IO ()
gifAnimationTest =
    case writeGifImages "Gifanim.gif" LoopingForever img of
      Left err -> putStrLn err
      Right w -> w
  where img = [(greyPalette, 20, greyScaleWitness (i * 10)) | i <- [0 .. 20]]

jpegValidTests :: [FilePath]
jpegValidTests = [ "explore_jpeg.jpg"
                 , "16x16jpeg.jpg"
                 , "8x8jpeg.jpg"
                 , "avatar.jpg"
                 , "fenek.jpg"
                 , "Channel_digital_image_CMYK_color.jpg"
                 , "rgb.jpg"
                 , "img_cmyk.jpg"
                 , "JPEG_example_JPG_RIP_001.jpg"
                 , "JPEG_example_JPG_RIP_010.jpg"
                 , "JPEG_example_JPG_RIP_025.jpg"
                 , "JPEG_example_JPG_RIP_050.jpg"
                 , "JPEG_example_JPG_RIP_100.jpg"
                 , "sheep.jpg"
                 , "MCU0.jpg"
                 , "MCU1.jpg"
                 , "MCU5.jpg"
                 , "MCU10.jpg"
                 , "20100713-0107-interleaved2.jpg"
                 , "MCU35.jpg"
                 , "dunno.jpg"
                 , "mand_prgrsv.jpg"
                 , "bad.jpg"
                 ]
 
tgaValidTests :: [FilePath]
tgaValidTests =
    [ "CBW8.TGA"
    , "CTC16.TGA"
    , "CTC24.TGA"
    , "CTC32.TGA"
    , "FLAG_B16.TGA"
    , "FLAG_B24.TGA"
    , "FLAG_B32.TGA"
    , "FLAG_T16.TGA"
    , "FLAG_T32.TGA"
    , "MARBLES.TGA"

    , "UTC16.TGA"
    , "UTC24.TGA"
    , "UTC32.TGA"
    , "XING_B16.TGA"
    , "XING_B24.TGA"
    , "XING_B32.TGA"
    , "XING_T16.TGA"
    , "XING_T24.TGA"
    , "XING_T32.TGA"

    , "CCM8.TGA"
    , "UBW8.TGA"
    , "UCM8.TGA"
    ]

bmpValidTests :: [FilePath]
bmpValidTests =
    ["simple_bitmap_24bits.bmp"
    ,"simple_bitmap_8bits.bmp"
    ,"eggyra0001.bmp"]

-- "caspian.tif"
tiffValidTests :: [FilePath]
tiffValidTests =
    ["depth/flower-rgb-planar-02.tif"
    ,"depth/flower-rgb-planar-04.tif"
    ,"depth/flower-rgb-planar-08.tif"
    ,"depth/flower-rgb-planar-16.tif"
    ,"depth/flower-rgb-contig-02.tif"
    ,"depth/flower-rgb-contig-04.tif"
    ,"depth/flower-rgb-contig-08.tif"
    ,"depth/flower-rgb-contig-16.tif"
    ,"depth/flower-palette-02.tif"
    ,"depth/flower-palette-04.tif"
    ,"depth/flower-palette-08.tif"
    ,"depth/flower-minisblack-02.tif"
    ,"depth/flower-minisblack-08.tif"
    ,"depth/flower-minisblack-04.tif"
    ,"depth/flower-minisblack-16.tif"
    ,"depth/flower-separated-contig-08.tif"
    ,"depth/flower-separated-contig-16.tif"
    ,"depth/flower-separated-planar-08.tif"
    ,"depth/flower-separated-planar-16.tif"
    ,"depth/flower-minisblack-12.tif"
    ,"quad-lzw.tif"
    ,"pc260001.tif"
    ,"cramps.tif"
    ,"strike.tif"
    ,"ycbcr-cat.tif"
    {-,"zackthecat.tif"-}
    {-"smallliz.tif"-}
    ,"compression/cs3n2c16.tif"
    ,"compression/flower-rgb-contig-16-packbits.tif"
    ,"other/butique-YA8.tif"
    ,"other/butique-YA16.tif"
    ,"horizontal-difference-lzw.tiff" -- produced by "Grab" on Mac OS X
    ]

validationJpegEncode :: Image PixelYCbCr8 -> L.ByteString
validationJpegEncode = encodeJpegAtQuality 100

eitherDo :: Either String (IO ()) -> IO ()
eitherDo (Left str) = putStrLn str
eitherDo (Right action) = action

gifToImg :: FilePath -> IO ()
gifToImg path = do
    rez <- readGifImages path
    case rez of
        Left err -> putStrLn $ "Error : " ++ err
        Right [] -> putStrLn "No image in gif file"
        Right v@(ImageRGB8 _ : _) ->
          forM_ (zip [0..] v) $ \(i :: Int, ImageRGB8 img) -> do
            let ycbcr = convertImage img
                jpg = validationJpegEncode ycbcr
                png = encodePng img
                tga = encodeTga img
                bmp = encodeBitmap img
                tiff = encodeTiff img
            putStrLn $ "PixelRGB8 : " ++ path

            putStrLn "-> BMP"
            L.writeFile (path ++ "_" ++ show i ++ "._fromRGB8.bmp") bmp
            putStrLn "-> PNG"
            L.writeFile (path ++ "_" ++ show i ++ "._fromRGB8.png") png
            putStrLn "-> TGA"
            L.writeFile (path ++ "_" ++ show i ++ "._fromRGB8.tga") tga
            putStrLn "-> JPG"
            L.writeFile (path ++ "_" ++ show i ++ "._fromRGB8.jpg") jpg
            putStrLn "-> Tiff"
            L.writeFile (path ++ "_" ++ show i ++ "._fromRGB8.tiff") tiff

        Right v@(ImageRGBA8 _ : _) ->
          forM_ (zip [0..] v) $ \(i :: Int, ImageRGBA8 img) -> do
            let ycbcr = convertImage $ dropAlphaLayer img
                jpg = validationJpegEncode ycbcr
                png = encodePng img
                tga = encodeTga img
                bmp = encodeBitmap img
                tiff = encodeTiff img
            putStrLn $ "PixelRGB8 : " ++ path

            putStrLn "-> BMP"
            L.writeFile (path ++ "_" ++ show i ++ "._fromRGBA8.bmp") bmp
            putStrLn "-> PNG"
            L.writeFile (path ++ "_" ++ show i ++ "._fromRGBA8.png") png
            putStrLn "-> TGA"
            L.writeFile (path ++ "_" ++ show i ++ "._fromRGBA8.tga") tga
            putStrLn "-> JPG"
            L.writeFile (path ++ "_" ++ show i ++ "._fromRGBA8.jpg") jpg
            putStrLn "-> Tiff"
            L.writeFile (path ++ "_" ++ show i ++ "._fromRGBA8.tiff") tiff

        Right _ ->
            putStrLn "Error : unexpected colorspace from GIF file"


imgToImg :: FilePath -> IO ()
imgToImg path = do
    rez <- readImage path
    case rez of
        Right (ImageYCbCr8 img) -> do
            let rgb = convertImage img :: Image PixelRGB8
                jpg = validationJpegEncode img
                png = encodePng rgb
                tga = encodeTga rgb
                bmp = encodeBitmap rgb
                tiff = encodeTiff img
            putStrLn $ "YCbCr : " ++ path
            putStrLn "-> JPG"
            L.writeFile (path ++ "._fromYCbCr8.jpg") jpg
            putStrLn "-> BMP"
            L.writeFile (path ++ "._fromYCbCr8.bmp") bmp
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromYCbCr8.png") png
            putStrLn "-> Tiff"
            L.writeFile (path ++ "._fromYCbCr8.tiff") tiff
            putStrLn "-> TGA"
            L.writeFile (path ++ "._fromYCbCr8.tga") tga
            putStrLn "-> Gif"
            eitherDo $ writeColorReducedGifImage (path ++ "._fromYCbCr8.gif") rgb

        Right (ImageYF _) -> putStrLn "don't handle HDR image in imgToImg"
        Right (ImageRGBF _) -> putStrLn "don't handle HDR image in imgToImg"
        Right (ImageCMYK16 img) -> do
            let rgbimg :: Image PixelRGB16
                rgbimg = convertImage img
                png = encodePng rgbimg
                tiff = encodeTiff img
            putStrLn $ "CMYK16 : " ++ path
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromCMYK16.png") png
            putStrLn "-> Tiff"
            L.writeFile (path ++ "._fromCMYK16.tiff") tiff

        Right (ImageCMYK8 img) -> do
            let rgbimg :: Image PixelRGB8
                rgbimg = convertImage img
                png = encodePng rgbimg
                tiff = encodeTiff img
            putStrLn $ "CMYK8 : " ++ path
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromCMYK8.png") png
            putStrLn "-> Gif"
            eitherDo $ writeColorReducedGifImage (path ++ "._fromCMYK8.gif") rgbimg
            putStrLn "-> Tiff"
            L.writeFile (path ++ "._fromCMYK8.tiff") tiff

        Right (ImageRGB8 img) -> do
            let jpg = validationJpegEncode (convertImage img)
                png = encodePng img
                bmp = encodeBitmap img
                tga = encodeTga img
                tiff = encodeTiff img
            putStrLn $ "RGB8 : " ++ path
            putStrLn "-> BMP"
            L.writeFile (path ++ "._fromRGB8.bmp") bmp
            putStrLn "-> JPG"
            L.writeFile (path ++ "._fromRGB8.jpg") jpg
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromRGB8.png") png
            putStrLn "-> Gif"
            eitherDo $ writeColorReducedGifImage (path ++ "._fromRGB8.gif") img
            putStrLn "-> Tiff"
            L.writeFile (path ++ "._fromRGB8.tiff") tiff
            putStrLn "-> Tga"
            L.writeFile (path ++ "._fromRGB8.tga") tga

        Right (ImageY16 img) -> do
            let pngFile = encodePng img
                tiffFile = encodeTiff img
            putStrLn $ "Y16 : " ++ path
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromY16.png") pngFile
            putStrLn "-> Tiff"
            L.writeFile (path ++ "._fromY16.tiff") tiffFile

        Right (ImageYA16 img) -> do
            let pngFile = imageToPng $ ImageYA16 img
            putStrLn $ "YA16 : " ++ path
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromYA16.png") pngFile

        Right (ImageRGB16 img) -> do
            let pngFile = encodePng img
                tiffFile = encodeTiff img
            putStrLn $ "RGB16 : " ++ path
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromRGB16.png") pngFile
            putStrLn "-> Tiff"
            L.writeFile (path ++ "._fromRGB16.tiff") tiffFile


        Right (ImageRGBA16 img) -> do
            let pngFile = encodePng img
                tiffFile = encodeTiff img
            putStrLn $ "RGBA16 : " ++ path
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromRGBA16.png") pngFile
            putStrLn "-> Tiff"
            L.writeFile (path ++ "._fromRGBA16.tiff") tiffFile

        Right (ImageRGBA8 img) -> do
            let bmp = encodeBitmap img
                jpg = validationJpegEncode (convertImage $ dropAlphaLayer img)
                png = encodePng img
                tiff = encodeTiff img
                tga = encodeTga img
            putStrLn $ "RGBA8 : " ++ path
            putStrLn "-> BMP"
            L.writeFile (path ++ "._fromRGBA8.bmp") bmp
            putStrLn "-> JPG"
            L.writeFile (path ++ "._fromRGBA8.jpg") jpg
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromRGBA8.png") png
            putStrLn "-> Tiff"
            L.writeFile (path ++ "._fromRGBA8.tiff") tiff
            putStrLn "-> Tga"
            L.writeFile (path ++ "._fromRGBA8.tga") tga

        Right (ImageY8 img) -> do
            let bmp = encodeBitmap img
                jpg = validationJpegEncode . convertImage $ (promoteImage img :: Image PixelRGB8)
                png = encodePng img
                tiff = encodeTiff img
                tga = encodeTiff img
                gif = encodeGifImage img
            putStrLn $ "Y8 : " ++ path
            putStrLn "-> BMP"
            L.writeFile (path ++ "._fromY8.bmp") bmp
            putStrLn "-> JPG"
            L.writeFile (path ++ "._fromY8.jpg") jpg
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromY8.png") png
            putStrLn "-> Tiff"
            L.writeFile (path ++ "._fromY8.tiff") tiff
            putStrLn "-> Tga"
            L.writeFile (path ++ "._fromY8.tga") tga
            putStrLn "-> Gif"
            L.writeFile (path ++ "._fromY8.gif") gif

        Right (ImageYA8 img) -> do
            let bmp = encodeBitmap $ (promoteImage img :: Image PixelRGB8)
                png = encodePng $ (promoteImage img :: Image PixelRGBA8)
                gif = encodeGifImage $ (dropAlphaLayer img)
                jpg = validationJpegEncode $ convertImage
                                (promoteImage $ dropAlphaLayer img :: Image PixelRGB8)
            putStrLn $ "YA8 : " ++ path
            putStrLn "-> BMP"
            L.writeFile (path ++ "._fromYA8.bmp") bmp
            putStrLn "-> JPG"
            L.writeFile (path ++ "._fromYA8.jpg") jpg
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromYA8.png") png
            putStrLn "-> Gif"
            L.writeFile (path ++ "._fromYA8.gif") gif

        Left err ->
            putStrLn $ "Error loading " ++ path ++ " " ++ err

toStandardDef :: Image PixelRGBF -> Image PixelRGB8
toStandardDef img = pixelMap pixelConverter img
  where fix = truncate . (254 *) . max 0.0 . min 1.0
        pixelConverter (PixelRGBF rf gf bf) = PixelRGB8 r g b
          where r = fix rf
                g = fix gf
                b = fix bf

radianceToBitmap :: FilePath -> IO ()
radianceToBitmap path = do
    rez <- readImage path
    case rez of
      Left err -> putStrLn $ "Error loading " ++ path ++ " " ++ err
      Right (ImageRGBF img) -> do
          L.writeFile (path ++ ".bmp") . imageToBitmap $ ImageRGBF img
          writeHDR (path ++ ".hdr") img
          writeRLENewStyleHDR (path ++ ".rle.hdr") img

      Right img -> do
          L.writeFile (path ++ ".bmp") $ imageToBitmap img

toJpg :: String -> Image PixelRGB8 -> IO ()
toJpg name img = do
    let jpg = validationJpegEncode (convertImage img)
    putStrLn "-> JPG"
    L.writeFile (name ++ "._fromRGB8.jpg") jpg

planeSeparationRGB8Test :: IO ()
planeSeparationRGB8Test = do
    rez <- readImage ("tests" </> "planeseparation.png")
    case rez of
       Left _ -> putStrLn "can't load separation file"
       Right (ImageRGB8 img) -> do
           L.writeFile ("tests" </> "rgb8_red.png") . encodePng $ extractComponent PlaneRed img
           L.writeFile ("tests" </> "rgb8_green.png") . encodePng $ extractComponent PlaneGreen img
           L.writeFile ("tests" </> "rgb8_blue.png") . encodePng $ extractComponent PlaneBlue img

       Right _ -> putStrLn "Wrong image file format"

planeSeparationRGBA8Test :: IO ()
planeSeparationRGBA8Test = do
    rez <- readImage ("tests" </> "planeseparationrgba.png")
    case rez of
       Left _ -> putStrLn "can't load separation file"
       Right (ImageRGBA8 img) -> do
           L.writeFile ("tests" </> "rgba8_red.png") . encodePng $ extractComponent PlaneRed img
           L.writeFile ("tests" </> "rgba8_green.png") . encodePng $ extractComponent PlaneGreen img
           L.writeFile ("tests" </> "rgba8_blue.png") . encodePng $ extractComponent PlaneBlue img
           L.writeFile ("tests" </> "rgba8_alpha.png") . encodePng $ extractComponent PlaneAlpha img

       Right _ -> putStrLn "Wrong image file format"

planeSeparationYA8Test :: IO ()
planeSeparationYA8Test = do
    let img = generateImage generator 256 256
        generator x y = PixelYA8 (fromIntegral $ x `mod` 256) (fromIntegral $ ((y `div` 4) `mod` 2) * 255)
    L.writeFile ("tests" </> "ya8_gray.png") . encodePng $ extractComponent PlaneLuma img
    L.writeFile ("tests" </> "ya8_alpha.png") . encodePng $ extractComponent PlaneAlpha img
    L.writeFile ("tests" </> "ya8_combined.png") $ encodePng img

gifTest :: [FilePath]
gifTest = ["Gif_pixel_cube.gif"
          ,"fgs.gif"
          ,"animated.gif"
          ,"interleaved.gif"
          ,"delta.gif"
          ,"delta2.gif"
          ,"rId98.gif"
          ,"magceit.gif"
          ,"2k.gif"
          ,"pikachu.gif"
          ,"huge.gif"
          ]

radianceTest :: [FilePath]
radianceTest = [ "sunrise.hdr", "free_009.hdr"]

testSuite :: IO ()
testSuite = do
    gifAnimationTest 
    putStrLn ">>>> Valid instances"
    toJpg "white" $ generateImage (\_ _ -> PixelRGB8 255 255 255) 16 16
    toJpg "black" $ generateImage (\_ _ -> PixelRGB8 0 0 0) 16 16
    toJpg "test" $ generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 255)
                                        128 128
    planeSeparationRGB8Test 
    planeSeparationRGBA8Test 
    planeSeparationYA8Test 

    mapM_ (imgToImg . (("tests" </> "bmp") </>)) bmpValidTests
    mapM_ (imgToImg . (("tests" </> "pngsuite") </>)) ("huge.png" : validTests)
    mapM_ (imgToImg . (("tests" </> "jpeg") </>)) ("huge.jpg" : jpegValidTests)
    mapM_ (radianceToBitmap . (("tests" </> "radiance") </>)) radianceTest
    mapM_ (gifToImg . (("tests" </> "gif") </>)) gifTest
    mapM_ (imgToImg . (("tests" </> "tiff") </>)) tiffValidTests
    mapM_ (imgToImg . (("tests" </> "tga") </>)) tgaValidTests

jpegToPng :: IO ()
jpegToPng = do
 {-Right rez <- readImage "tests/jpeg/sheep.jpg"-}
 {-savePngImage "control.png" rez-}
 img <- readImage  "tests/jpeg/mand_prgrsv.jpg" -- "tests/jpeg/huge.jpg"
 case img of
   Left err -> do
       putStrLn err
       {-error "Can't decompress img"-}
   Right i -> savePngImage "huge.png" i

pngToJpeg :: IO ()
pngToJpeg = do
  img <- readImage "tests/pngsuite/huge.png"
  case img of
    Left err -> do
        putStrLn err
        {-error "Can't decompress img"-}
    Right i -> saveJpgImage 95 "huge.jpg" i

pngToBmp :: IO ()
pngToBmp = do
  img <- readImage "tests/pngsuite/huge.png"
  case img of
    Right (ImageRGB8 i) -> writeBitmap "huge.bmp" i
    Left err -> do
        putStrLn err
        {-error "Can't decompress img"-}
    _ -> return ()

gifToPng :: IO ()
gifToPng = do
  img <- readImage "tests/gif/2k.gif"
  case img of
    Right (ImageRGB8 i) -> writeBitmap "huge.png" i
    Left err -> do
        putStrLn err
        {-error "Can't decompress img"-}
    _ -> return ()

benchMark :: IO ()
benchMark = do
    putStrLn "Benchmarking"

    hugeJpeg <- B.readFile "tests/jpeg/huge.jpg"
    hugePng <- B.readFile "tests/pngsuite/huge.png"
    hugeGif <- B.readFile "tests/gif/huge.gif"

    let Right decodedImage = decodeImage hugePng

    jpegToPng >> pngToJpeg

    defaultMainWith defaultConfig [
        bgroup "trad"
            [ bench "JPG -> PNG" $ whnfIO jpegToPng
            , bench "PNG -> JPG" $ whnfIO pngToJpeg
            , bench "GIF -> PNG" $ whnfIO gifToPng
            , bench "PNG -> BMP" $ whnfIO pngToBmp
            ],

        bgroup "reading"
            [ bench "Huge jpeg" $ nf decodeImage hugeJpeg
            , bench "Huge png" $ nf decodeImage hugePng
            , bench "Huge gif" $ nf decodeImage hugeGif
            ],

        bgroup "writing"
            [ bench "Huge jpeg" $ whnfIO $ saveJpgImage 50 "s.jpg" decodedImage
            , bench "Huge png" $ whnfIO $ savePngImage "p.png" decodedImage
            ]
        ]
    putStrLn "END"

debug :: IO ()
debug = do
 args <- getArgs
 case args of
   [] -> putStrLn "no filename"
   [_] -> putStrLn "no filename"
   (_:filename:_) -> do
      img <- readImage filename
      case img of
        Left err -> putStrLn err
        Right i ->
            savePngImage (filename ++ ".png") i

myMain :: IO ()
myMain = do
    prog <- liftM (map toLower) getProgName
    args <- getArgs
    case args of
        ("test":_) -> testSuite
        ("debug":_) -> debug
        ("jpegtopng":_) -> jpegToPng
        ("pngtojpeg":_) -> pngToJpeg
        ("pngtobmp":_) -> pngToBmp
        [] | "imagetest"      `isInfixOf` prog -> testSuite
           | "imagebenchmark" `isInfixOf` prog -> benchMark
        _ -> do
            putStrLn ("Unknown command " ++ show args ++ "Launching benchMark")
            benchMark

main :: IO ()
main = myMain

