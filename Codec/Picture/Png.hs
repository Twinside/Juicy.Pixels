{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
-- | Module used for loading & writing \'Portable Network Graphics\' (PNG)
-- files. The API has two layers, the high level, which load the image without
-- looking deeply about it and the low level, allowing access to data chunks contained
-- in the PNG image.
--
-- For general use, please use 'decodePng' function.
--
-- The loader has been validated against the pngsuite (http://www.libpng.org/pub/png/pngsuite.html)
module Codec.Picture.Png( -- * High level functions
                          PngSavable( .. )

                        , readPng
                        , decodePng
                        , writePng

                        ) where

import Control.Monad( foldM_, forM_ )
import Control.Monad.ST( ST )
import Control.Monad.Trans( lift )
import qualified Control.Monad.Trans.State.Strict as S
import Data.Serialize( Serialize, runGet, get)
import Data.Array.Unboxed( IArray, UArray, (!), listArray, bounds, elems )
import Data.Array.ST( STUArray, runSTUArray, MArray
                    , readArray, writeArray, newArray, getBounds )
import Data.List( find )
import Data.Word( Word8, Word16, Word32 )
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lb

import Codec.Picture.Types
import Codec.Picture.Png.Type
import Codec.Picture.Png.Export

-- | Simple structure used to hold information about Adam7 deinterlacing.
-- A structure is used to avoid pollution of the module namespace.
data Adam7MatrixInfo = Adam7MatrixInfo
    { adam7StartingRow  :: [Int]
    , adam7StartingCol  :: [Int]
    , adam7RowIncrement :: [Int]
    , adam7ColIncrement :: [Int]
    , adam7BlockHeight  :: [Int]
    , adam7BlockWidth   :: [Int]
    }

-- | The real info about the matrix.
adam7MatrixInfo :: Adam7MatrixInfo
adam7MatrixInfo = Adam7MatrixInfo
    { adam7StartingRow  = [0, 0, 4, 0, 2, 0, 1]
    , adam7StartingCol  = [0, 4, 0, 2, 0, 1, 0]
    , adam7RowIncrement = [8, 8, 8, 4, 4, 2, 2]
    , adam7ColIncrement = [8, 8, 4, 4, 2, 2, 1]
    , adam7BlockHeight  = [8, 8, 4, 4, 2, 2, 1]
    , adam7BlockWidth   = [8, 4, 4, 2, 2, 1, 1]
    }

unparsePngFilter :: Word8 -> Either String PngFilter
unparsePngFilter 0 = Right FilterNone
unparsePngFilter 1 = Right FilterSub
unparsePngFilter 2 = Right FilterUp
unparsePngFilter 3 = Right FilterAverage
unparsePngFilter 4 = Right FilterPaeth
unparsePngFilter _ = Left "Invalid scanline filter"

type PngLine s = STUArray s Int Word8
type ByteReader s a = S.StateT B.ByteString (ST s) a

{-# INLINE getNextByte #-}
getNextByte :: ByteReader s Word8
getNextByte = do str <- S.get
                 case B.uncons str of
                    Just (v, rest) -> S.put rest >> return v
                    Nothing -> return 0

{-# INLINE (!!!) #-}
(!!!) :: (IArray array e) => array Int e -> Int -> e
(!!!) = (!) -- unsafeAt

{-# INLINE (.!!!.) #-}
(.!!!.) :: (MArray array e m) => array Int e -> Int -> m e
(.!!!.) = readArray -- unsafeRead

{-# INLINE (.<-.) #-}
(.<-.) :: (MArray array e m) => array Int e -> Int -> e -> m ()
(.<-.)  = writeArray -- unsafeWrite

-- | Apply a filtering method on a reduced image. Apply the filter
-- on each line, using the previous line (the one above it) to perform
-- some prediction on the value.
pngFiltering :: LineUnpacker s -> Int -> (Int, Int)    -- ^ Image size
             -> ByteReader s ()
pngFiltering unpacker beginZeroes (imgWidth, imgHeight) = do
    thisLine <- lift $ newArray (0, beginZeroes + imgWidth - 1) 0
    otherLine <- lift $ newArray (0, beginZeroes + imgWidth - 1) 0
    foldM_ (\(previousLine, currentLine) lineIndex -> do
               byte <- getNextByte
               let lineFilter = case unparsePngFilter byte of
                       Right FilterNone    -> filterNone
                       Right FilterSub     -> filterSub
                       Right FilterAverage -> filterAverage
                       Right FilterUp      -> filterUp
                       Right FilterPaeth   -> filterPaeth
                       _ -> filterNone
               lineFilter (previousLine, currentLine)
               lift $ unpacker lineIndex (stride, currentLine)
               return (currentLine, previousLine)
        ) (thisLine, otherLine) [0 .. imgHeight - 1]

    where stride = fromIntegral beginZeroes
          filterNone, filterSub, filterUp, filterPaeth, filterAverage :: (PngLine s, PngLine s) 
                                                                      -> ByteReader s ()
          filterNone (previousLine, thisLine) =
            mapM_ (\idx -> do
                byte <- getNextByte
                lift $ (thisLine .<-. idx) byte) [beginZeroes .. beginZeroes + imgWidth - 1]

          filterSub (previousLine, thisLine) = 
            mapM_ (\idx -> do
                byte <- getNextByte
                val <- lift $ thisLine .!!!. (idx - stride)
                lift . (thisLine .<-. idx) $ byte + val)
                    [beginZeroes .. beginZeroes + imgWidth - 1]

          filterUp (previousLine, thisLine) = 
            mapM_ (\idx -> do
                byte <- getNextByte
                val <- lift $ previousLine .!!!. idx
                lift . (thisLine .<-. idx) $ val + byte)
                    [beginZeroes .. beginZeroes + imgWidth - 1]

          filterAverage (previousLine, thisLine) = 
            mapM_ (\idx -> do
                byte <- getNextByte
                valA <- lift $ thisLine .!!!. (idx - stride)
                valB <- lift $ previousLine .!!!. idx
                let a' = fromIntegral valA
                    b' = fromIntegral valB
                lift . (thisLine .<-. idx) $ byte + fromIntegral ((a' + b') `div` (2 :: Word16))
                ) [beginZeroes .. beginZeroes + imgWidth - 1]

          filterPaeth (previousLine, thisLine) =
            mapM_ (\idx -> do
                byte <- getNextByte
                valA <- lift $ thisLine .!!!. (idx - stride)
                valC <- lift $ previousLine .!!!. (idx - stride)
                valB <- lift $ previousLine .!!!. idx
                lift . (thisLine .<-. idx) $ byte + paeth valA valB valC
                ) [beginZeroes .. beginZeroes + imgWidth - 1]

-- | Directly stolen from the definition in the standard (on W3C page),
-- pixel predictor.
paeth :: Word8 -> Word8 -> Word8 -> Word8
paeth a b c
  | pa <= pb && pa <= pc = a
  | pb <= pc             = b
  | otherwise            = c
    where a' = fromIntegral a :: Int
          b' = fromIntegral b
          c' = fromIntegral c
          p = a' + b' - c'
          pa = abs $ p - a'
          pb = abs $ p - b'
          pc = abs $ p - c'

strideWrite :: STUArray s Int Word8 -> Int -> Int -> [Word8]
            -> ST s ()
strideWrite array idx stride vals = foldM_ (\ix val -> do
    (array .<-. ix) val
    return $ ix + stride) idx vals


mArrayMap :: (MArray (STUArray s) a (ST s))
          => Int -> Int -> (a -> b) -> STUArray s Int a -> ST s [b]
mArrayMap from to f arr = inner from
    where inner idx | idx == to = return []
                    | otherwise = do
                        val <- arr .!!!. from
                        rest <- inner $ idx + 1
                        return $ f val : rest

type LineUnpacker s = Int -> (Int, PngLine s) -> ST s ()

type StrideInfo = (Int, Int)

byteUnpacker :: Int -> MutableImage s Word8 -> StrideInfo -> LineUnpacker s
byteUnpacker sampleCount (MutableImage{ mutableImageWidth = imgWidth, mutableImageData = arr })
             (strideWidth, strideHeight) height (beginIdx, line) = do
    (_, maxIdx) <- getBounds line

    forM_ [0 .. maxIdx - beginIdx] $ \destSampleIndex -> do
        let destSampleBase = (height * strideHeight * imgWidth + destSampleIndex) * sampleCount
        forM_ [0 .. sampleCount - 1] $ \sample -> do
            val <- line .!!!. (beginIdx + sample)
            (arr .<-. (destSampleBase + sampleCount)) val
             

-- | Transform a scanline to a bunch of bytes. Bytes are then packed
-- into pixels at a further step.
{-unpackScanline :: Word32  -- ^ Bitdepth-}
               {--> Word32  -- ^ Sample count-}
               {--> MutableImage s Word8-}
               {--> Int     -- ^ Stride width-}
               {--> Int     -- ^ Stride height-}
               {--> Int     -- ^ Beginning index-}
               {--> PngLine s-}
               {--> ST s ()-}
{-unpackScanline 1 1 image@(MutableImage{ mutableImageWidth = imgWidth-}
                                      {-, mutableImageData = arr-}
                                      {-}) strideWidth strideHeight -}
                                         {-beginIndice line = do-}
   {-bytes <- mArrayMap beginIndice (beginIndice + lineSize) (split 8) line-}
   {-restVal <- line .!!!. (beginIndice + lineSize + 1)-}
   {-let finalList = concat bytes ++ split bitRest restVal-}
   {-strideWrite arr $ finalList-}
    {-where split :: Int -> Word8 -> [Word8] -- avoid defaulting-}
          {-split times c = map (extractBit c) [7, 6 .. 8 - times]-}
          {-lineSize = imgWidth `quot` 8-}
          {-bitRest = imgWidth `mod` 8-}
          {-realHeight = height * strideHeight-}

          {-extractBit c shiftCount =-}
              {-((c `shiftR` shiftCount) .&. 1) * 255-}

{-unpackScanline 2 1 imgWidth imgHeight = concat <$> replicateM (fromIntegral imgHeight) lineParser-}
    {-where split :: Word32 -> Word8 -> [Word8] -- avoid defaulting-}
          {-split times c = map (extractBit c) [3, 2 .. 4 - times]-}
          {-lineSize = imgWidth `quot` 4-}
          {-bitRest = imgWidth `mod` 4-}

          {-extractBit c shiftCount = (c `shiftR` (fromIntegral shiftCount * 2)) .&. 0x3-}

          {-lineParser = do-}
            {-line <- concat <$> replicateM (fromIntegral lineSize) (split 4 <$> get)-}
            {-if bitRest == 0-}
                {-then return line-}
                {-else do lastElems <- split bitRest <$> get-}
                        {-return $ line ++ lastElems-}

{-unpackScanline 4 sampleCount imgWidth imgHeight = concat <$> replicateM (fromIntegral imgHeight) lineParser-}
    {-where split :: Word8 -> [Word8]-}
          {-split c = [(c `shiftR` 4) .&. 0xF, c .&. 0xF]-}
          {-lineSize = fromIntegral $ imgWidth `quot` 2-}
          {-isFullLine = (imgWidth * sampleCount) `mod` 2 == 0-}

          {-lineParser = do-}
            {-line <- concat <$> replicateM lineSize (split <$> get)-}
            {-if isFullLine-}
                {-then return line-}
                {-else do lastElem <- (head . split) <$> get-}
                        {-return $ line ++ [lastElem]-}

unpackScanline 8 = byteUnpacker
{-unpackScanline 16 sampleCount imgWidth imgHeight =-}
    {-replicateM (fromIntegral $ imgWidth * imgHeight * sampleCount) (bitDepthReducer <$> getWord16be)-}
        {-where bitDepthReducer v = fromIntegral $ v32 * word8Max `div` word16Max-}
                  {-where v32 = fromIntegral v :: Word32 -- type signature to avoid defaulting to Integer-}
                        {-word8Max = 2 ^ (8 :: Word32) - 1 :: Word32-}
                        {-word16Max = 2 ^ (16 :: Word32) - 1 :: Word32-}

unpackScanline _ = error "Impossible bit depth"

byteSizeOfBitLength :: Word32 -> Word32 -> Word32 -> Word32
byteSizeOfBitLength pixelBitDepth sampleCount dimension = size + (if rest /= 0 then 1 else 0)
   where (size, rest) = (pixelBitDepth * dimension * sampleCount) `quotRem` 8

scanLineInterleaving :: Int -> Int -> (Int, Int) -> LineUnpacker s -> ByteReader s ()
scanLineInterleaving _depth _sampleCount (_imgWidt, _imgHeight) unpacker = return ()

-- | Given data and image size, recreate an image with deinterlaced
-- data for PNG's adam 7 method.
adam7Unpack :: Int -> Int -> (Int, Int) -> LineUnpacker s -> ByteReader s ()
adam7Unpack depth sampleCount (imgWidth, imgHeight) unpacker = sequence_
  [pngFiltering unpacker sampleCount passSize | passSize <- zip passWidths passHeights]
    where Adam7MatrixInfo { adam7StartingRow  = startRows
                          , adam7RowIncrement = rowIncrement
                          , adam7StartingCol  = startCols
                          , adam7ColIncrement = colIncrement } = adam7MatrixInfo

          sizer dimension begin increment
            | dimension <= begin = 0
            | otherwise = outDim + (if restDim /= 0 then 1 else 0)
                where (outDim, restDim) = (dimension - begin) `quotRem` increment

          passHeights =
              [sizer imgHeight begin incr | (begin, incr) <- zip startRows rowIncrement]
          passWidths  =
              [sizer imgWidth  begin incr | (begin, incr) <- zip startCols colIncrement]


-- | deinterlace picture in function of the method indicated
-- in the iHDR
deinterlacer :: PngIHdr -> ByteReader s (STUArray s Int Word8)
deinterlacer ihdr@(PngIHdr { width = w, height = h
                           , colourType = imgKind
                           , interlaceMethod = method
                           , bitDepth        = depth  }) = do
    let componentCount = sampleCountOfImageType imgKind 
        arraySize = fromIntegral $ w * h * componentCount
        deinterlaceFunction = case method of
            PngNoInterlace -> scanLineInterleaving
            PngInterlaceAdam7 -> adam7Unpack
    imgArray <- lift $ newArray (0, arraySize - 1) 0
    deinterlaceFunction (fromIntegral depth) 
                        (fromIntegral componentCount)
                        (fromIntegral w, fromIntegral h) undefined
    return imgArray

generateGreyscalePalette :: Word8 -> PngPalette
generateGreyscalePalette times = listArray (0, fromIntegral possibilities) pixels
    where possibilities = 2 ^ times - 1
          pixels = [PixelRGB8 i i i | n <- [0..possibilities]
                                    , let i = n * (255 `div` possibilities)]

sampleCountOfImageType :: PngImageType -> Word32
sampleCountOfImageType PngGreyscale = 1
sampleCountOfImageType PngTrueColour = 3
sampleCountOfImageType PngIndexedColor = 1
sampleCountOfImageType PngGreyscaleWithAlpha = 2
sampleCountOfImageType PngTrueColourWithAlpha = 4

paletteRGBA1, paletteRGBA2, paletteRGBA4 :: PngPalette
paletteRGBA1 = generateGreyscalePalette 1
paletteRGBA2 = generateGreyscalePalette 2
paletteRGBA4 = generateGreyscalePalette 4

-- | Transform a raw png image to an image, without modifying the
-- underlying pixel type. If the image is greyscale and < 8 bits,
-- a transformation to RGBA8 is performed. This should change
-- in the future.
-- The resulting image let you manage the pixel types.
decodePng :: B.ByteString -> Either String DynamicImage
decodePng byte = do
    rawImg <- runGet get byte
    let ihdr = header rawImg
        compressedImageData =
              B.concat [chunkData chunk | chunk <- chunks rawImg
                                        , chunkType chunk == iDATSignature]
        zlibHeaderSize = 1 {- compression method/flags code -}
                       + 1 {- Additional flags/check bits -}
                       + 4 {-CRC-}

        unparse _ PngGreyscale
            | bitDepth ihdr == 1 = unparse (Just paletteRGBA1) PngIndexedColor
            | bitDepth ihdr == 2 = unparse (Just paletteRGBA2) PngIndexedColor
            | bitDepth ihdr == 4 = unparse (Just paletteRGBA4) PngIndexedColor
            | otherwise = ImageY8 <$> deinterlacer ihdr
        unparse (Just plte) PngIndexedColor =
            ImageRGBA8 <$> amap (promotePixel . (plte !) . fromIntegral) <$> img
                where img = deinterlacer ihdr
        unparse Nothing PngIndexedColor  = Left "no valid palette found"
        unparse _ PngTrueColour          = ImageRGB8 <$> deinterlacer ihdr
        unparse _ PngGreyscaleWithAlpha  = ImageYA8 <$> deinterlacer ihdr
        unparse _ PngTrueColourWithAlpha = ImageRGBA8 <$> deinterlacer ihdr

    if B.length compressedImageData <= zlibHeaderSize
       then Left "Invalid data size"
       else let imgData = Z.decompress $ Lb.fromChunks [compressedImageData]
                palette = case find (\c -> pLTESignature == chunkType c) $ chunks rawImg of
                    Nothing -> Nothing
                    Just p -> case parsePalette p of
                            Left _ -> Nothing
                            Right plte -> Just plte
            in unparse palette (colourType ihdr) . B.concat $ Lb.toChunks imgData

