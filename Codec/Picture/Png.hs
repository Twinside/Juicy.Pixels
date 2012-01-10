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

import Control.Monad( foldM_, forM_
                    {-, when-}
                    )
import Control.Monad.ST( ST )
import Control.Monad.Trans( lift )
import qualified Control.Monad.Trans.State.Strict as S
import Data.Serialize( Serialize, runGet, get)
import Data.Array.Unboxed( IArray, UArray, (!), listArray, bounds, elems )
import Data.Array.ST( STUArray, runSTUArray, MArray
                    , readArray, writeArray, newArray, getBounds )
import Data.Bits( (.&.), (.|.), shiftL, shiftR )
import Data.List( find, zip4 )
import Data.Word( Word8, Word16, Word32 )
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lb

import Codec.Picture.Types
import Codec.Picture.Png.Type
import Codec.Picture.Png.Export
import Debug.Trace
{-import Text.Printf-}

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

type ByteReader s a = S.StateT B.ByteString (ST s) a

{-# INLINE getNextByte #-}
getNextByte :: ByteReader s Word8
getNextByte = do str <- S.get
                 case B.uncons str of
                    Just (v, rest) -> S.put rest >> return v
                    Nothing -> return 0

{-# INLINE (!!!) #-}
(!!!) :: (IArray array e) => array Int e -> Int -> e
(!!!) arr i = (!) arr i -- unsafeAt

{-# INLINE (.!!!.) #-}
(.!!!.) :: (MArray array e m) => array Int e -> Int -> m e
(.!!!.) arr i= readArray arr i -- unsafeRead

{-# INLINE (.<-.) #-}
(.<-.) :: (MArray array e m) => array Int e -> Int -> e -> m ()
(.<-.) arr i = writeArray arr i -- unsafeWrite

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
          filterNone, filterSub, filterUp, filterPaeth,
                filterAverage :: (PngLine s, PngLine s) -> ByteReader s ()
          filterNone (_previousLine, thisLine) =
            mapM_ (\idx -> do
                byte <- getNextByte
                lift $ (thisLine .<-. idx) byte) [beginZeroes .. beginZeroes + imgWidth - 1]

          filterSub (_previousLine, thisLine) = 
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

type PngLine s = STUArray s Int Word8
type LineUnpacker s = Int -> (Int, PngLine s) -> ST s ()

type StrideInfo  = (Int, Int)
type BeginOffset = (Int, Int)

byteUnpacker :: Int -> MutableImage s Word8 -> StrideInfo -> BeginOffset -> LineUnpacker s
byteUnpacker sampleCount (MutableImage{ mutableImageWidth = imgWidth, mutableImageData = arr })
             (strideWidth, strideHeight) (beginLeft, beginTop) h (beginIdx, line) = do
    (_, maxIdx) <- getBounds line
    let realTop = beginTop + h * strideHeight
        lineIndex = realTop * imgWidth
        pixelToRead = min (imgWidth - 1) $ (maxIdx - beginIdx) `div` sampleCount
    forM_ [0 .. pixelToRead] $ \pixelIndex -> do
        let destPixelIndex = lineIndex + pixelIndex * strideWidth + beginLeft 
            destSampleIndex = destPixelIndex * sampleCount
            srcPixelIndex = pixelIndex * sampleCount + beginIdx
        forM_ [0 .. sampleCount - 1] $ \sample -> do
            val <- line .!!!. (srcPixelIndex + sample)
            let writeIdx = destSampleIndex + sample
            (arr .<-. writeIdx) val
             
bitUnpacker :: Int -> MutableImage s Word8 -> StrideInfo -> BeginOffset -> LineUnpacker s
bitUnpacker _ (MutableImage{ mutableImageWidth = imgWidth, mutableImageData = arr })
              (strideWidth, strideHeight) (beginLeft, beginTop) h (beginIdx, line) = do
    let realTop = beginTop + h * strideHeight
        lineIndex = realTop * imgWidth
    forM_ [0 .. imgWidth `div` 8 - 1] $ \pixelIndex -> do
        val <- line .!!!. (pixelIndex  + beginIdx)
        let writeIdx n = lineIndex + (pixelIndex * 8 + n) * strideWidth + beginLeft 
        forM_ [0 .. 7] $ \bit -> (arr .<-. writeIdx (7 - bit)) ((val `shiftR` bit) .&. 1)

twoBitsUnpacker :: Int -> MutableImage s Word8 -> StrideInfo -> BeginOffset -> LineUnpacker s
twoBitsUnpacker _ (MutableImage{ mutableImageWidth = imgWidth, mutableImageData = arr })
                  (strideWidth, strideHeight) (beginLeft, beginTop) h (beginIdx, line) = do
    let realTop = beginTop + h * strideHeight
        lineIndex = realTop * imgWidth
    forM_ [0 .. imgWidth `div` 4 - 1] $ \pixelIndex -> do
        val <- line .!!!. (pixelIndex  + beginIdx)
        let writeIdx n = lineIndex + (pixelIndex * 4 + n) * strideWidth + beginLeft 
        (arr .<-. writeIdx 0) $ (val `shiftR` 6) .&. 0x3
        (arr .<-. writeIdx 1) $ (val `shiftR` 4) .&. 0x3
        (arr .<-. writeIdx 2) $ (val `shiftR` 2) .&. 0x3
        (arr .<-. writeIdx 3) $ val .&. 0x3
    {-let restBit = imgWidth `mod` 4-}
    {-when (restBit /= 0) (do-}
        {-val <- line .!!!. (imgWidth `div` 4)-}
        {-forM_ [0 .. restBit - 1] (\n-> -}
            {-(arr .<-. writeIdx n) $ (val `shiftR` 6 - 2 * n) .&. 0x3))-}

shortUnpacker :: Int -> MutableImage s Word8 -> StrideInfo -> BeginOffset -> LineUnpacker s
shortUnpacker sampleCount (MutableImage{ mutableImageWidth = imgWidth, mutableImageData = arr })
             (strideWidth, strideHeight) (beginLeft, beginTop) h (beginIdx, line) = do
    (_, maxIdx) <- getBounds line
    let realTop = beginTop + h * strideHeight
        lineIndex = realTop * imgWidth
        pixelToRead = min (imgWidth - 1) $ (maxIdx - beginIdx) `div` (sampleCount * 2)
    forM_ [0 .. pixelToRead] $ \pixelIndex -> do
        let destPixelIndex = lineIndex + pixelIndex * strideWidth + beginLeft 
            destSampleIndex = destPixelIndex * sampleCount
            srcPixelIndex = pixelIndex * sampleCount * 2 + beginIdx
        forM_ [0 .. sampleCount - 1] $ \sample -> do
            highBits <- line .!!!. (srcPixelIndex + sample * 2 + 0)
            lowBits <- line .!!!. (srcPixelIndex + sample * 2 + 1)
            let fullValue = fromIntegral lowBits .|. (fromIntegral highBits `shiftL` 8) :: Word32
                word8Max = 2 ^ (8 :: Word32) - 1 :: Word32
                word16Max = 2 ^ (16 :: Word32) - 1 :: Word32
                val = fullValue * word8Max `div` word16Max
                writeIdx = destSampleIndex + sample
            (arr .<-. writeIdx) $ fromIntegral val

-- | Transform a scanline to a bunch of bytes. Bytes are then packed
-- into pixels at a further step.
scanlineUnpacker :: Int -> Int -> MutableImage s Word8 -> StrideInfo -> BeginOffset -> LineUnpacker s
{-scanlineUnpacker 4 sampleCount imgWidth imgHeight = concat <$> replicateM (fromIntegral imgHeight) lineParser-}
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
scanlineUnpacker 1 = bitUnpacker
scanlineUnpacker 2 = twoBitsUnpacker
scanlineUnpacker 8 = byteUnpacker
scanlineUnpacker 16 = shortUnpacker
scanlineUnpacker _ = error "Impossible bit depth"

byteSizeOfBitLength :: Int -> Int -> Int -> Int
byteSizeOfBitLength pixelBitDepth sampleCount dimension = size + (if rest /= 0 then 1 else 0)
   where (size, rest) = (pixelBitDepth * dimension * sampleCount) `quotRem` 8

scanLineInterleaving :: Int -> Int -> (Int, Int) -> (StrideInfo -> BeginOffset -> LineUnpacker s)
                     -> ByteReader s ()
scanLineInterleaving depth sampleCount (imgWidth, imgHeight) unpacker = trace ("Normal interleaving") $
    pngFiltering (unpacker (1,1) (0, 0)) strideInfo (byteWidth, imgHeight)
        where byteWidth = byteSizeOfBitLength depth sampleCount imgWidth
              strideInfo | depth < 8 = 1
                         | otherwise = sampleCount * (depth `div` 8)

-- | Given data and image size, recreate an image with deinterlaced
-- data for PNG's adam 7 method.
adam7Unpack :: Int -> Int -> (Int, Int) -> (StrideInfo -> BeginOffset -> LineUnpacker s)
            -> ByteReader s ()
adam7Unpack depth sampleCount (imgWidth, imgHeight) unpacker = trace "Adam7 interleaving" $ sequence_
  [pngFiltering (unpacker (incrW, incrH) (beginW, beginH)) strideInfo (byteWidth, passHeight)
                | (beginW, incrW, beginH, incrH) <- zip4 startCols colIncrement startRows rowIncrement
                , let passWidth = sizer imgWidth beginW incrW
                      passHeight = sizer imgHeight beginH incrH
                      byteWidth = byteSizeOfBitLength depth sampleCount passWidth
                ]
    where Adam7MatrixInfo { adam7StartingRow  = startRows
                          , adam7RowIncrement = rowIncrement
                          , adam7StartingCol  = startCols
                          , adam7ColIncrement = colIncrement } = adam7MatrixInfo

          strideInfo | depth < 8 = 1
                     | otherwise = sampleCount * (depth `div` 8)
          sizer dimension begin increment
            | dimension <= begin = 0
            | otherwise = outDim + (if restDim /= 0 then 1 else 0)
                where (outDim, restDim) = (dimension - begin) `quotRem` increment

-- | deinterlace picture in function of the method indicated
-- in the iHDR
deinterlacer :: PngIHdr -> ByteReader s (STUArray s Int Word8)
deinterlacer (PngIHdr { width = w, height = h, colourType  = imgKind
                      , interlaceMethod = method, bitDepth = depth  }) = do
    let compCount = sampleCountOfImageType imgKind 
        arraySize = fromIntegral $ w * h * compCount
        deinterlaceFunction = case method of
            PngNoInterlace -> scanLineInterleaving
            PngInterlaceAdam7 -> adam7Unpack
        iBitDepth = fromIntegral depth
    imgArray <- lift $ newArray (0, arraySize - 1) 0
    let mutableImage = MutableImage (fromIntegral w) (fromIntegral h) imgArray
    deinterlaceFunction iBitDepth 
                        (fromIntegral compCount)
                        (fromIntegral w, fromIntegral h)
                        (scanlineUnpacker iBitDepth (fromIntegral compCount)
                                                    mutableImage)
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

paletteRGBA1, paletteRGBA2, paletteRGBA4, paletteRGBA8 :: PngPalette
paletteRGBA1 = generateGreyscalePalette 1
paletteRGBA2 = generateGreyscalePalette 2
paletteRGBA4 = generateGreyscalePalette 4
paletteRGBA8 = generateGreyscalePalette 8

applyPalette :: PngPalette -> UArray Int Word8 -> UArray Int Word8
applyPalette pal img = listArray (0, (initSize + 1) * 3 - 1) pixels
    where (_, initSize) = bounds img
          pixels = concat [[r, g, b] | ipx <- elems img
                                     , let PixelRGB8 r g b = pal !!! fromIntegral ipx]

readPng :: FilePath -> IO (Either String DynamicImage)
readPng path = B.readFile path >>= return . decodePng

-- | Transform a raw png image to an image, without modifying the
-- underlying pixel type. If the image is greyscale and < 8 bits,
-- a transformation to RGBA8 is performed. This should change
-- in the future.
-- The resulting image let you manage the pixel types.
decodePng :: B.ByteString -> Either String DynamicImage
decodePng byte = do
    rawImg <- runGet get byte
    let ihdr@(PngIHdr { width = w, height = h }) = header rawImg
        compressedImageData =
              B.concat [chunkData chunk | chunk <- chunks rawImg
                                        , chunkType chunk == iDATSignature]
        zlibHeaderSize = 1 {- compression method/flags code -}
                       + 1 {- Additional flags/check bits -}
                       + 4 {-CRC-}

        imager = Image (fromIntegral w) (fromIntegral h)

        unparse _ PngGreyscale bytes
            | bitDepth ihdr == 1 = unparse (Just paletteRGBA1) PngIndexedColor bytes
            | bitDepth ihdr == 2 = unparse (Just paletteRGBA2) PngIndexedColor bytes
            | bitDepth ihdr == 4 = unparse (Just paletteRGBA4) PngIndexedColor bytes
            | otherwise = unparse (Just paletteRGBA8) PngIndexedColor bytes
            {-| otherwise = Right . ImageY8 . imager $ runSTUArray stArray-}
                {-where stArray = S.evalStateT (deinterlacer ihdr) bytes-}
        unparse Nothing PngIndexedColor  _ = Left "no valid palette found"
        unparse _ PngTrueColour          bytes =
            Right . ImageRGB8 . imager $ runSTUArray stArray
                where stArray = S.evalStateT (deinterlacer ihdr) bytes
        unparse _ PngGreyscaleWithAlpha  bytes =
            Right . ImageYA8 . imager $ runSTUArray stArray
                where stArray = S.evalStateT (deinterlacer ihdr) bytes
        unparse _ PngTrueColourWithAlpha bytes =
            Right . ImageRGBA8 . imager $ runSTUArray stArray
                where stArray = S.evalStateT (deinterlacer ihdr) bytes
        unparse (Just plte) PngIndexedColor bytes =
            Right . ImageRGB8 . imager $ applyPalette plte uarray
                where stArray = S.evalStateT (deinterlacer ihdr) bytes
                      uarray = runSTUArray stArray

    if B.length compressedImageData <= zlibHeaderSize
       then Left "Invalid data size"
       else let imgData = Z.decompress $ Lb.fromChunks [compressedImageData]
       	        parseableData = B.concat $ Lb.toChunks imgData
                palette = case find (\c -> pLTESignature == chunkType c) $ chunks rawImg of
                    Nothing -> Nothing
                    Just p -> case parsePalette p of
                            Left _ -> Nothing
                            Right plte -> Just plte
            in unparse palette (colourType ihdr) parseableData

