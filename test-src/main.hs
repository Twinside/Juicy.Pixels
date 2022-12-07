-- test file, don't care about unused, on the contrary...
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
import Codec.Picture
import Codec.Picture.Jpg( JpgEncodable 
                        , decodeJpegWithMetadata
                        , encodeJpeg
                        , encodeDirectJpegAtQualityWithMetadata )
import Codec.Picture.Gif
import Codec.Picture.Tiff
import System.Environment

import Data.Either ( isRight )
import Data.Binary
import Data.Binary.Get (runGetOrFail)
import Data.Bits ( unsafeShiftR, xor )
import Data.Char( toLower )
import Data.List( isInfixOf, isPrefixOf )
import Data.Maybe( maybeToList )
import Data.Monoid
import Data.Word( Word8 )
import Control.Monad( forM_, liftM, when )
import System.FilePath
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Codec.Picture.Jpg.Internal.Types as JpgInternal
import Codec.Picture.Types
import Codec.Picture.Saving
import Codec.Picture.HDR
import Codec.Picture.Bitmap( encodeBitmapWithMetadata )
import Codec.Picture.Png( encodePalettedPngWithMetadata )
import qualified Codec.Picture.Metadata as Met
import qualified Codec.Picture.Metadata.Exif as Met
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

gifAPITest :: IO ()
gifAPITest = do
  testFail noImages1 "No GIF frames" "Empty frames list"
  testFail noImages2 "No image in list" "Empty images list"

  testFail zeroWidthScreen "Invalid screen bounds" "Screen width of zero"
  testFail zeroHeightScreen "Invalid screen bounds" "Screen height of zero"
  testFail tooWideScreen "Invalid screen bounds" "Screen width over 65535"
  testFail tooTallScreen "Invalid screen bounds" "Screen height over 65535"

  testFail zeroWidthFrame "GIF frames with invalid bounds" "Frame width of zero"
  testFail zeroHeightFrame "GIF frames with invalid bounds" "Frame height of zero"
  testFail tooWideFrame "GIF frames with invalid bounds" "Screen width over 65535"
  testFail tooTallFrame "GIF frames with invalid bounds" "Screen height over 65535"

  testFail outOfBoundsXNeg "GIF frames out of screen bounds" "Frame out of screen bounds -X"
  testFail outOfBoundsXPos "GIF frames out of screen bounds" "Frame out of screen bounds +X"
  testFail outOfBoundsYNeg "GIF frames out of screen bounds" "Frame out of screen bounds -Y"
  testFail outOfBoundsYPos "GIF frames out of screen bounds" "Frame out of screen bounds +Y"

  testFail zeroColorGlobalPalette "Invalid global palette size" "Zero color global palette"
  testFail tooLargeGlobalPalette "Invalid global palette size" "Over 256 colors global palette"
  testFail tallGlobalPalette "Invalid global palette size" "Invalid global palette height"

  testFail zeroColorFramePalette "Invalid palette size in GIF frames" "Zero color frame palette"
  testFail tooLargeFramePalette "Invalid palette size in GIF frames" "Over 256 colors frame palette"
  testFail tallFramePalette "Invalid palette size in GIF frames" "Invalid frame palette height"

  testFail bgColorMissing "GIF background index absent" "GIF background color not in global palette"
  testFail trColorMissing "GIF transparent index absent" "GIF transparent color not in frame palette"

  testFail noPalette "GIF image frames with color indexes missing" "Frame with no palette"
  
  testFail pxColorMissing1 "GIF image frames with color indexes missing" "Pixel color index not in palette"
  testFail pxColorMissing2 "GIF image frames with color indexes missing" "Pixel color index not in palette"
  testFail pxColorMissing3 "GIF image frames with color indexes missing" "Pixel color index not in palette"

  where testFail (Right _)  _        desc = putStrLn $ "GIF API test failed: " ++ desc ++ " should cause error"
        testFail (Left msg) expected _
          | expected `isPrefixOf` msg = return ()
          | otherwise                 = putStrLn $ "Unexpected failure message from GIF API: " ++ msg

        palette2b = generateImage (\x _ -> let c = if x == 0 then 0x00 else 0xff in PixelRGB8 c c c) 2 1
        pixels1x1 = generateImage (\_ _ -> 0) 1 1
        okFrames = [GifFrame 0 0 (Just palette2b) Nothing 0 DisposalAny pixels1x1]

        noImages1 = encodeComplexGifImage $ GifEncode 1 1 Nothing Nothing LoopingNever []
        noImages2 = encodeGifImages LoopingNever []

        zeroWidthScreen = encodeComplexGifImage $ GifEncode 0 1 Nothing Nothing LoopingNever okFrames
        zeroHeightScreen = encodeComplexGifImage $ GifEncode 1 0 Nothing Nothing LoopingNever okFrames
        tooWideScreen = encodeComplexGifImage $ GifEncode 0x10000 1 Nothing Nothing LoopingNever okFrames
        tooTallScreen = encodeComplexGifImage $ GifEncode 1 0x10000 Nothing Nothing LoopingNever okFrames

        zeroWidthFrame = encodeComplexGifImage $ GifEncode 1 1 (Just palette2b) Nothing LoopingNever
                         [GifFrame 0 0 Nothing Nothing 0 DisposalAny $ generateImage (\_ _ -> 0) 0 1]
        zeroHeightFrame = encodeComplexGifImage $ GifEncode 1 1 (Just palette2b) Nothing LoopingNever
                          [GifFrame 0 0 Nothing Nothing 0 DisposalAny $ generateImage (\_ _ -> 0) 1 0]
        tooWideFrame = encodeComplexGifImage $ GifEncode 1 1 (Just palette2b) Nothing LoopingNever
                       [GifFrame 0 0 Nothing Nothing 0 DisposalAny $ generateImage (\_ _ -> 0) 0x10000 1]
        tooTallFrame = encodeComplexGifImage $ GifEncode 1 1 (Just palette2b) Nothing LoopingNever
                       [GifFrame 0 0 Nothing Nothing 0 DisposalAny $ generateImage (\_ _ -> 0) 1 0x10000]

        outOfBoundsXNeg = encodeComplexGifImage $ GifEncode 1 1 (Just palette2b) Nothing LoopingNever
                          [GifFrame (-1) 0 Nothing Nothing 0 DisposalAny pixels1x1]
        outOfBoundsXPos = encodeComplexGifImage $ GifEncode 1 1 (Just palette2b) Nothing LoopingNever
                          [GifFrame 1 0 Nothing Nothing 0 DisposalAny pixels1x1]
        outOfBoundsYNeg = encodeComplexGifImage $ GifEncode 1 1 (Just palette2b) Nothing LoopingNever
                          [GifFrame 0 (-1) Nothing Nothing 0 DisposalAny pixels1x1]
        outOfBoundsYPos = encodeComplexGifImage $ GifEncode 1 1 (Just palette2b) Nothing LoopingNever
                          [GifFrame 0 1 Nothing Nothing 0 DisposalAny pixels1x1]

        zeroColorGlobalPalette =
          let palette = generateImage (\_ _ -> PixelRGB8 0 0 0) 0 1
          in encodeComplexGifImage $ GifEncode 1 1 (Just palette) Nothing LoopingNever okFrames
        tooLargeGlobalPalette =
          let palette = generateImage (\_ _ -> PixelRGB8 0 0 0) 257 1
          in encodeComplexGifImage $ GifEncode 1 1 (Just palette) Nothing LoopingNever okFrames
        tallGlobalPalette =
          let palette = generateImage (\_ _ -> PixelRGB8 0 0 0) 1 2
          in encodeComplexGifImage $ GifEncode 1 1 (Just palette) Nothing LoopingNever okFrames

        zeroColorFramePalette =
          let palette = generateImage (\_ _ -> PixelRGB8 0 0 0) 0 1
              frames = [GifFrame 0 0 (Just palette) Nothing 0 DisposalAny pixels1x1]
          in encodeComplexGifImage $ GifEncode 1 1 Nothing Nothing LoopingNever frames
        tooLargeFramePalette =
          let palette = generateImage (\_ _ -> PixelRGB8 0 0 0) 257 1
              frames = [GifFrame 0 0 (Just palette) Nothing 0 DisposalAny pixels1x1]
          in encodeComplexGifImage $ GifEncode 1 1 Nothing Nothing LoopingNever frames
        tallFramePalette =
          let palette = generateImage (\_ _ -> PixelRGB8 0 0 0) 1 2
              frames = [GifFrame 0 0 (Just palette) Nothing 0 DisposalAny pixels1x1]
          in encodeComplexGifImage $ GifEncode 1 1 Nothing Nothing LoopingNever frames

        bgColorMissing = encodeComplexGifImage $ GifEncode 1 1 (Just palette2b) (Just 3) LoopingNever okFrames
        trColorMissing = encodeComplexGifImage $ GifEncode 1 1 Nothing Nothing LoopingNever
                         [GifFrame 0 0 (Just palette2b) (Just 3) 0 DisposalAny pixels1x1]

        noPalette =
          let frames = [GifFrame 0 0 Nothing Nothing 0 DisposalAny pixels1x1]
          in encodeComplexGifImage $ GifEncode 1 1 Nothing Nothing LoopingNever frames

        pxColorMissing1 = -- Global palette
          let pixels = generateImage (\_ _ -> 2) 1 1
              frames = [GifFrame 0 0 Nothing Nothing 0 DisposalAny pixels]
          in encodeComplexGifImage $ GifEncode 1 1 (Just palette2b) Nothing LoopingNever frames
        pxColorMissing2 = -- Local palette
          let pixels = generateImage (\_ _ -> 2) 1 1
              frames = [GifFrame 0 0 (Just palette2b) Nothing 0 DisposalAny pixels]
          in encodeComplexGifImage $ GifEncode 1 1 Nothing Nothing LoopingNever frames
        pxColorMissing3 = -- Both
          let pixels = generateImage (\_ _ -> 2) 1 1
              frames = [GifFrame 0 0 (Just palette2b) Nothing 0 DisposalAny pixels]
          in encodeComplexGifImage $ GifEncode 1 1 (Just palette2b) Nothing LoopingNever frames
        

gifAnimationTest :: IO ()
gifAnimationTest =
    case writeGifImages "Gifanim.gif" LoopingForever img of
      Left err -> putStrLn err
      Right w -> w
  where img = [(greyPalette, 20, greyScaleWitness (i * 10)) | i <- [0 .. 20]]

gifPaletteTest :: IO ()
gifPaletteTest = do
  gifGridTest
  gif1024ColorsTest

gifGridTest :: IO ()
gifGridTest = do
  gifBWGrid
  gifBAGrid
  gifWAGrid

  where palette = generateImage (\x _ -> let c = if x == 0 then 0x00 else 0xff in PixelRGB8 c c c) 2 1
        pixels = generateImage (\x y -> if even x `xor` even y then 0 else 1) 20 20
        writeGif name spec = case writeComplexGifImage ("tests" </> name) spec of
          Left err -> putStrLn err
          Right w -> w
        -- BW global palette, no local palette
        gifBWGrid = writeGif "black-white-grid.gif" $
          GifEncode 20 20 (Just palette) Nothing LoopingNever
          [GifFrame 0 0 Nothing Nothing 0 DisposalAny pixels]
        -- No global palette, BW local palette with white index as transparency
        gifBAGrid = writeGif "black-alpha-grid.gif" $
          GifEncode 20 20 Nothing Nothing LoopingNever
          [GifFrame 0 0 (Just palette) (Just 1) 0 DisposalAny pixels]
        -- No global palette, BW local palette with black index as transparency
        gifWAGrid = writeGif "white-alpha-grid.gif" $
          GifEncode 20 20 Nothing Nothing LoopingNever
          [GifFrame 0 0 (Just palette) (Just 0) 0 DisposalAny pixels]

gif1024ColorsTest :: IO ()
gif1024ColorsTest = case writeComplexGifImage ("tests" </> "1024-colors.gif") spec of
                      Left err -> putStrLn err
                      Right w -> w
  where spec = GifEncode 256 4 Nothing Nothing LoopingNever [reds, greens, blues, greys]
        -- Most viewers will animate this even with the 0 delays
        reds   = GifFrame 0 0 (Just redPalette)   Nothing 0 DisposalDoNot pixels
        greens = GifFrame 0 1 (Just greenPalette) Nothing 0 DisposalDoNot pixels
        blues  = GifFrame 0 2 (Just bluePalette)  Nothing 0 DisposalDoNot pixels
        greys  = GifFrame 0 3 (Just greyPalette)  Nothing 0 DisposalDoNot pixels
        redPalette   = pixelMap (\(PixelRGB8 r _ _) -> PixelRGB8 r 0 0) greyPalette
        greenPalette = pixelMap (\(PixelRGB8 _ g _) -> PixelRGB8 0 g 0) greyPalette
        bluePalette  = pixelMap (\(PixelRGB8 _ _ b) -> PixelRGB8 0 0 b) greyPalette
        pixels = generateImage (\x _ -> fromIntegral x) 256 1

jpegValidTests :: [FilePath]
jpegValidTests = [ "bad_decode.jpg"
                 , "broken.jpg"
                 , "borked_render.jpg"
                 , "inf_loop01.jpg"
                 , "inf_loop02.jpg"
                 , "explore_jpeg.jpg"
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
                 , "richys-groceries.jpg"
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
    ,"simple_bitmap_32bits.bmp"
    ,"bitmap_32bits_type0.bmp"
    ,"eggyra0001.bmp"
    ,"smiley.bmp"
    ,"rle_test.bmp"
    ,"bmpsuite_good/pal1bg.bmp"
    ,"bmpsuite_good/pal8.bmp"
    ,"bmpsuite_good/pal8v5.bmp"
    ,"bmpsuite_good/rgb16.bmp"
    ,"bmpsuite_good/pal1.bmp"
    ,"bmpsuite_good/pal8gs.bmp"
    ,"bmpsuite_good/pal8w124.bmp"
    ,"bmpsuite_good/rgb24.bmp"
    ,"bmpsuite_good/pal1wb.bmp"
    ,"bmpsuite_good/pal8nonsquare.bmp"
    ,"bmpsuite_good/pal8w125.bmp"
    ,"bmpsuite_good/rgb24pal.bmp"
    ,"bmpsuite_good/pal4.bmp"
    ,"bmpsuite_good/pal8os2.bmp"
    ,"bmpsuite_good/pal8w126.bmp"
    ,"bmpsuite_good/rgb32bf.bmp"
    ,"bmpsuite_good/pal4gs.bmp"
    ,"bmpsuite_good/pal8rle.bmp"
    ,"bmpsuite_good/rgb16-565.bmp"
    ,"bmpsuite_good/rgb32bfdef.bmp"
    ,"bmpsuite_good/pal4rle.bmp"
    ,"bmpsuite_good/pal8topdown.bmp"
    ,"bmpsuite_good/rgb16-565pal.bmp"
    ,"bmpsuite_good/rgb32.bmp"
    ,"bmpsuite_good/pal8-0.bmp"
    ,"bmpsuite_good/pal8v4.bmp"
    ,"bmpsuite_good/rgb16bfdef.bmp"
    ]

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
    ,"rad_YF.tif"
    ,"rad_Y32.tif"
    ]

validationJpegEncode :: Image PixelYCbCr8 -> L.ByteString
validationJpegEncode = encodeJpegAtQuality 100

liberalJpegEncode :: (PixelBaseComponent px ~ Word8, JpgEncodable px)
                  => Image px -> L.ByteString
liberalJpegEncode =
    encodeDirectJpegAtQualityWithMetadata 100 mempty

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
    rez <- readImageWithMetadata path
    case rez of
        Right (ImageYCbCr8 img, met) -> do
            let rgb = convertImage img :: Image PixelRGB8
                jpg = validationJpegEncode img
                png = encodePng rgb
                tga = encodeTga rgb
                bmp = encodeBitmap rgb
                tiff = encodeTiff img
            putStrLn $ "YCbCr : " ++ path
            print met
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

        Right (ImageYF img, met) -> do
            let png16  = encodePng $ fromFloatTo16 img
                tiff32 = encodeTiff $ fromFloatTo32 img
            putStrLn $ "Float : " ++ path
            print met
            putStrLn "-> PNG (16)"
            L.writeFile (path ++ "._fromYF.png") png16
            putStrLn "-> Tiff (32)"
            L.writeFile (path ++ "._fromYF.tiff") tiff32

        Right (ImageY32 img, met) -> do
            let png16     = encodePng $ from32To16 img
                tiffFloat = encodeTiff $ from32ToFloat img
            putStrLn $ "Y32 : " ++ path
            print met
            putStrLn "-> PNG (16)"
            L.writeFile (path ++ "._fromY32.png") png16
            putStrLn "-> Tiff (Float)"
            L.writeFile (path ++ "._fromY32.tiff") tiffFloat

        Right (ImageRGBF _, _) -> putStrLn "don't handle HDR image in imgToImg"
        Right (ImageCMYK16 img, met) -> do
            let rgbimg :: Image PixelRGB16
                rgbimg = convertImage img
                png = encodePng rgbimg
                tiff = encodeTiff img
            putStrLn $ "CMYK16 : " ++ path
            print met
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromCMYK16.png") png
            putStrLn "-> Tiff"
            L.writeFile (path ++ "._fromCMYK16.tiff") tiff

        Right (ImageCMYK8 img, met) -> do
            let rgbimg :: Image PixelRGB8
                rgbimg = convertImage img
                png = encodePng rgbimg
                jpg = encodeDirectJpegAtQualityWithMetadata 90 mempty img
                tiff = encodeTiff img
            putStrLn $ "CMYK8 : " ++ path
            print met
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromCMYK8.png") png
            putStrLn "-> JPG"
            L.writeFile (path ++ "._fromCMYK8.jpg") jpg
            putStrLn "-> Gif"
            eitherDo $ writeColorReducedGifImage (path ++ "._fromCMYK8.gif") rgbimg
            putStrLn "-> Tiff"
            L.writeFile (path ++ "._fromCMYK8.tiff") tiff

        Right (ImageRGB8 img, met) -> do
            let jpg = validationJpegEncode (convertImage img)
                jpgRgb = liberalJpegEncode img
                png = encodePng img
                bmp = encodeBitmap img
                tga = encodeTga img
                tiff = encodeTiff img
            putStrLn $ "RGB8 : " ++ path
            print met
            putStrLn "-> BMP"
            L.writeFile (path ++ "._fromRGB8.bmp") bmp
            putStrLn "-> JPG"
            L.writeFile (path ++ "._fromRGB8.jpg") jpg
            putStrLn "-> JPG (RGB)"
            L.writeFile (path ++ ".RGB._fromRGB8.jpg") jpgRgb
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromRGB8.png") png
            putStrLn "-> Gif"
            eitherDo $ writeColorReducedGifImage (path ++ "._fromRGB8.gif") img
            putStrLn "-> Tiff"
            L.writeFile (path ++ "._fromRGB8.tiff") tiff
            putStrLn "-> Tga"
            L.writeFile (path ++ "._fromRGB8.tga") tga

        Right (ImageY16 img, met) -> do
            let pngFile = encodePng img
                tiffFile = encodeTiff img
            putStrLn $ "Y16 : " ++ path
            print met
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromY16.png") pngFile
            putStrLn "-> Tiff"
            L.writeFile (path ++ "._fromY16.tiff") tiffFile

        Right (ImageYA16 img, met) -> do
            let pngFile = imageToPng $ ImageYA16 img
            putStrLn $ "YA16 : " ++ path
            print met
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromYA16.png") pngFile

        Right (ImageRGB16 img, met) -> do
            let pngFile = encodePng img
                tiffFile = encodeTiff img
            putStrLn $ "RGB16 : " ++ path
            print met
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromRGB16.png") pngFile
            putStrLn "-> Tiff"
            L.writeFile (path ++ "._fromRGB16.tiff") tiffFile


        Right (ImageRGBA16 img, met) -> do
            let pngFile = encodePng img
                tiffFile = encodeTiff img
            putStrLn $ "RGBA16 : " ++ path
            print met
            putStrLn "-> PNG"
            L.writeFile (path ++ "._fromRGBA16.png") pngFile
            putStrLn "-> Tiff"
            L.writeFile (path ++ "._fromRGBA16.tiff") tiffFile

        Right (ImageRGBA8 img, met) -> do
            let bmp = encodeBitmap img
                jpg = validationJpegEncode (convertImage $ dropAlphaLayer img)
                png = encodePng img
                tiff = encodeTiff img
                tga = encodeTga img
            putStrLn $ "RGBA8 : " ++ path
            print met
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

        Right (ImageY8 img, met) -> do
            let bmp = encodeBitmap img
                jpg = liberalJpegEncode img
                png = encodePng img
                tiff = encodeTiff img
                tga = encodeTiff img
                gif = encodeGifImage img
            putStrLn $ "Y8 : " ++ path
            print met
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

        Right (ImageYA8 img, met) -> do
            let bmp = encodeBitmap $ (promoteImage img :: Image PixelRGB8)
                png = encodePng $ (promoteImage img :: Image PixelRGBA8)
                gif = encodeGifImage $ (dropAlphaLayer img)
                jpg = validationJpegEncode $ convertImage
                                (promoteImage $ dropAlphaLayer img :: Image PixelRGB8)
            putStrLn $ "YA8 : " ++ path
            print met
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

fromFloatTo32 :: Image PixelF -> Image Pixel32
fromFloatTo32 = pixelMap (\f -> floor (f * (fromIntegral (maxBound :: Word32))))

fromFloatTo16 :: Image PixelF -> Image Pixel16
fromFloatTo16 = pixelMap (\f -> floor (f * (fromIntegral (maxBound :: Word16))))

from32ToFloat :: Image Pixel32 -> Image PixelF
from32ToFloat = pixelMap (\w -> fromIntegral w / 4294967296.0)

from32To16 :: Image Pixel32 -> Image Pixel16
from32To16 = pixelMap (fromIntegral . (`unsafeShiftR` 16))

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

metadataWriteTest :: IO ()
metadataWriteTest = do
  let dumbImage = generateImage (\_ _ -> PixelRGB8 255 255 255) 16 16
      mi = Met.insert
      metas = mi Met.Author "It's a me"
            $ mi Met.Software "JuicyPixels test suite"
            $ mi Met.Title "A metadata test"
            $ mi Met.Copyright "meh"
            $ mi Met.Description "let's see the results"
            $ mi Met.Comment "Test of comment"
            $ Met.mkDpiMetadata 93
  L.writeFile "tests/metadata.png" $
      encodePngWithMetadata metas dumbImage
  L.writeFile "tests/metadata.jpg" $
      encodeDirectJpegAtQualityWithMetadata 50 metas $ (convertImage dumbImage :: Image PixelYCbCr8)
  L.writeFile "tests/metadata.bmp" $
      encodeBitmapWithMetadata metas dumbImage

metadataReadTest :: IO ()
metadataReadTest = do
  image <- B.readFile "tests/jpeg/10x8-samsung-s8.jpg"
  case decodeJpegWithMetadata image of
      Left err -> putStrLn err
      Right (ImageYCbCr8 img, meta) -> do
        putStrLn "===== Storing Jpeg metadata"
        print meta
        let encoded = encodeDirectJpegAtQualityWithMetadata 90 meta img
        L.writeFile "tests/metadata_extif.jpg" encoded
        let Right (_, remeta) = decodeJpegWithMetadata image
        putStrLn "========= Reparsed metas"
        print remeta
          
      Right (_, meta) -> checkMeta meta
  where
    -- The insert order is important as there is no Eq instance (yet)
    metas = Met.insert Met.DpiY 72 $
            Met.insert Met.DpiX 72 $
            Met.insert (Met.Exif Met.TagModel) (Met.ExifString $ B.pack "SM-G955F\NUL") $
            Met.insert Met.Height 3024 $
            Met.insert (Met.Exif Met.TagOrientation) (Met.ExifShort 6) $
            Met.insert Met.Width 4032 $
            Met.insert (Met.Exif Met.TagLightSource) (Met.ExifLong 0) $
            Met.insert (Met.Exif Met.TagFlash) (Met.ExifShort 0) $
            Met.insert Met.Software "G955FXXU1AQJ1\NUL" $
            Met.insert Met.Format Met.SourceTiff $
            Met.empty

    checkMeta meta = do
      let sm = show meta
          sms = show metas
      when (sm /= sms) $
        putStrLn $ "Erroneous metadata parsed from file" ++ sm ++ " vs " ++ sms

palettedPngCreation :: IO ()
palettedPngCreation = L.writeFile "tests/paleted_alpha.png" encoded
  where
    Right encoded = encodePalettedPngWithMetadata mempty palette img
    img = generateImage (\x _y -> fromIntegral x) 256 128
    palette :: Image PixelRGBA8
    palette = generateImage (\x _y -> PixelRGBA8 255 128 128 (255 - fromIntegral x)) 256 1

-- The given `path` must be a valid JPEG; this function checks it.
-- This is to guard against not noticing that everything fails parsing.
jpgParseECS_equivalence_success :: FilePath -> IO ()
jpgParseECS_equivalence_success path = do
    bsl <- L.fromStrict <$> B.readFile path
    let ecs_res =
            runGetOrFail (JpgInternal.skipUntilFrames *> parseFramesWithParseECSFunction JpgInternal.parseECS) bsl
    let ecs_simple_res =
            runGetOrFail (JpgInternal.skipUntilFrames *> parseFramesWithParseECSFunction JpgInternal.parseECS_simple) bsl
    case (ecs_res, ecs_simple_res) of
        (Right{}, Right{})
            | ecs_res == ecs_simple_res -> return ()
            | otherwise -> error "Test failure: parseECS /= parseECS_simple"
        _ -> error $ "Test failure: parseECS / parseECS_simple failed unexpectedly with results: " ++ show (isRight ecs_res, isRight ecs_simple_res) -- only show Left/Right
  where
    parseFramesWithParseECSFunction :: Get L.ByteString -> Get [JpgInternal.JpgFrame]
    parseFramesWithParseECSFunction parseECSFunction = do
        kind <- get
        case kind of
            JpgInternal.JpgEndOfImage -> return []
            _ -> do
                mbFrame <- case kind of
                    JpgInternal.JpgStartOfScan -> do
                        scanHeader <- get
                        ecs <- parseECSFunction
                        return $! Just $! JpgInternal.JpgScanBlob scanHeader ecs
                    _ -> JpgInternal.parseFrameOfKind kind
                JpgInternal.skipFrameMarker
                remainingFrames <- JpgInternal.parseFrames
                return $ maybeToList mbFrame ++ remainingFrames

-- The given `path` must be a valid JPEG; this function checks it.
-- This is to guard against not noticing that everything fails parsing.
getJpgImage_equivalence_success :: FilePath -> IO ()
getJpgImage_equivalence_success path = do
    bsl <- L.fromStrict <$> B.readFile path
    let res = runGetOrFail (JpgInternal.getJpgImage) bsl
    let legacy_res = runGetOrFail (get :: Get JpgInternal.JpgImage) bsl
    case (res, legacy_res) of
        (Right{}, Right{})
            | res == legacy_res -> return ()
            | otherwise -> error "Test failure: getJpgImage /= (get :: Get JpgImage)"
        _ -> error $ "Test failure: getJpgImage / (get :: Get JpgImage) failed unexpectedly with results: " ++ show (isRight res, isRight legacy_res) -- only show Left/Right

testSuite :: IO ()
testSuite = do
    putStrLn ">>>> Metadata test"
    metadataWriteTest
    metadataReadTest
    putStrLn ">>>> Gif animation test"
    gifAnimationTest
    putStrLn ">>>> Gif API test"
    gifAPITest
    putStrLn ">>>> Gif palette test"
    gifPaletteTest
    putStrLn ">>>> Jpg parseECS equivalence test"
    mapM_ (jpgParseECS_equivalence_success . (("tests" </> "jpeg") </>)) ("huge.jpg" : "10x8-samsung-s8.jpg" : jpegValidTests)
    putStrLn ">>>> Jpg getJpgImage equivalence test"
    mapM_ (getJpgImage_equivalence_success . (("tests" </> "jpeg") </>)) ("huge.jpg" : "10x8-samsung-s8.jpg" : jpegValidTests)
    putStrLn ">>>> Valid instances"
    toJpg "white" $ generateImage (\_ _ -> PixelRGB8 255 255 255) 16 16
    toJpg "black" $ generateImage (\_ _ -> PixelRGB8 0 0 0) 16 16
    toJpg "test" $ generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 255)
                                        128 128
    palettedPngCreation
    planeSeparationRGB8Test 
    planeSeparationRGBA8Test 
    planeSeparationYA8Test 

    mapM_ (imgToImg . (("tests" </> "bmp") </>)) bmpValidTests
    mapM_ (imgToImg . (("tests" </> "pngsuite") </>)) ("huge.png" : validTests)
    mapM_ (imgToImg . (("tests" </> "jpeg") </>)) ("huge.jpg" : jpegValidTests)
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

