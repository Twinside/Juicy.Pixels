{-# LANGUAGE OverloadedStrings #-}

module Writer where
import GifImage
import LZW
import Data.Bits
import Control.Monad (liftM)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.ByteString.Char8 hiding (head, concat, map)
import Data.Binary.BitPut
import Data.ByteString.Lazy (toChunks)

putWord8 :: Int -> BitPut
putWord8 = putNBits 8

putDescriptor :: ScreenDescriptor -> BitPut
putDescriptor ds = do
  putWord8 (screenWidth ds)
  putWord8 (screenWidth ds `shiftR` 8)
  putWord8 (screenHeight ds)
  putWord8 (screenHeight ds `shiftR` 8)
  putBit   $ hasGlobalMap ds
  putNBits 3 (bitsOfColorResolution ds - 1)
  putBit   False
  putNBits 3 (screenBitsPerPixel ds - 1)
  putWord8 (backgroundIndex ds)
  putWord8 0

putGlobalMap :: ColorMap -> BitPut
putGlobalMap cm = do
    sequence_ $ putColor `liftM` V.toList cm
  where
    putColor (r,g,b) = do
      putWord8 $ fromIntegral r
      putWord8 $ fromIntegral g
      putWord8 $ fromIntegral b


putImage :: ScreenDescriptor -> Image -> BitPut
putImage _ (descriptor, Just map, raster) = do
  putImageDescriptor $ descriptor
  putGlobalMap $ map
  putRasterData raster $ bitsPerPixel descriptor
putImage ds (descriptor, Nothing, raster) = do
  putImageDescriptor $ descriptor
  putRasterData raster $ screenBitsPerPixel ds

putImageDescriptor :: ImageDescriptor -> BitPut
putImageDescriptor ds = do
  putWord8 0x2c
  putWord8 $ pixelsFromLeft ds
  putWord8 $ pixelsFromLeft ds `shiftR` 8
  putWord8 $ pixelsFromTop ds
  putWord8 $ pixelsFromTop ds `shiftR` 8
  putWord8 $ imageWidth ds
  putWord8 $ imageWidth ds `shiftR` 8
  putWord8 $ imageHeight ds
  putWord8 $ imageHeight ds `shiftR` 8
  putBit   $ hasLocalMap ds
  putBit   $ interlaced ds
  putNBits 3 (0 :: Int)
  putNBits 3 (bitsPerPixel ds - 1)


putRasterData :: Raster -> Int -> BitPut
putRasterData raster rootSize = do
    let intList = map fst $ concat .V.toList $ V.map V.toList $ raster :: [Int]
    let lzw = lzwEncode rootSize intList :: ByteString
    putWord8 rootSize
    putBlocks lzw
  where
    putBlocks :: ByteString -> BitPut
    putBlocks arr
      | B.null arr = putWord8 0
      | otherwise =
          let bytes  = B.take 255 arr
              actual = B.length bytes
          in do
            putWord8 actual
            putByteString bytes
            putBlocks (B.drop actual arr)

putGifTerminator :: BitPut
putGifTerminator = do
  putWord8 0x3b

gifWriter :: GifImage -> BitPut
gifWriter image = do
  putByteString "GIF87a"
  putDescriptor $ gifScreenDescriptor image
  putGlobalMap  $ gifGlobalMap image
  putImage (gifScreenDescriptor image) `mapM_` images image
  putGifTerminator

runGifWriter :: GifImage -> ByteString
runGifWriter image =
  B.concat . toChunks $ runBitPut (gifWriter image)

