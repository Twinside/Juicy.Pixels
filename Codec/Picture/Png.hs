{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Module used for loading & writing \'Portable Network Graphics\' (PNG)
-- files. The API has two layers, the high level, which load the image without
-- looking deeply about it and the low level, allowing access to data chunks contained
-- in the PNG image.
--
-- For general use, please use 'loadPng' function.
module Codec.Picture.Png( -- * High level functions
                          PngLoadable( .. )
                        , loadPng

                          -- * Low level types
                        , ChunkSignature 
                        , PngChunk( .. )
                        , PngLowLevel( .. )

                        ) where

import Control.Applicative
import Control.Monad( when, replicateM )
import Data.Maybe( catMaybes )
import Data.Bits
import Data.Serialize
import Data.Array.Unboxed
import Data.List( foldl', zip4, find )
import Data.Word
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lb

import Codec.Picture.ColorConversion
import Codec.Picture.Types

--------------------------------------------------
----            Types
--------------------------------------------------
type ChunkSignature = B.ByteString

-- | Generic header used in PNG images.
data PngIHdr = PngIHdr
    { width             :: Word32       -- ^ Image width in number of pixel
    , height            :: Word32       -- ^ Image height in number of pixel
    , bitDepth          :: Word8        -- ^ Number of bit per sample
    , colourType        :: PngImageType -- ^ Kind of png image (greyscale, true color, indexed...)
    , compressionMethod :: Word8        -- ^ Compression method used
    , filterMethod      :: Word8        -- ^ Must be 0
    , interlaceMethod   :: PngInterlaceMethod   -- ^ If the image is interlaced (for progressive rendering)
    }
    deriving Show

-- | What kind of information is encoded in the IDAT section
-- of the PngFile
data PngImageType =
      PngGreyscale
    | PngTrueColour
    | PngIndexedColor
    | PngGreyscaleWithAlpha
    | PngTrueColourWithAlpha
    deriving Show

sampleCountOfImageType :: PngImageType -> Word32
sampleCountOfImageType PngGreyscale = 1
sampleCountOfImageType PngTrueColour = 3
sampleCountOfImageType PngIndexedColor = 1
sampleCountOfImageType PngGreyscaleWithAlpha = 2
sampleCountOfImageType PngTrueColourWithAlpha = 4

data PngRawImage = PngRawImage
    { header       :: PngIHdr
    , chunks       :: [PngRawChunk]
    }

type PngPalette = Array Word32 PixelRGB8

parsePalette :: PngRawChunk -> Either String PngPalette
parsePalette plte 
 | chunkLength plte `mod` 3 /= 0 = Left "Invalid palette size"
 | otherwise = listArray (0, pixelCount - 1) <$> runGet pixelUnpacker (chunkData plte)
    where pixelUnpacker = replicateM (fromIntegral pixelCount) get
          pixelCount = chunkLength plte `div` 3

-- | Data structure during real png loading/parsing
data PngRawChunk = PngRawChunk
    { chunkLength :: Word32
    , chunkType   :: ChunkSignature
    , chunkCRC    :: Word32
    , chunkData   :: B.ByteString
    }

-- | PNG chunk representing some extra information found in the parsed file.
data PngChunk = PngChunk
    { pngChunkData        :: B.ByteString  -- ^ The raw data inside the chunk
    , pngChunkSignature   :: ChunkSignature -- ^ The name of the chunk.
    }

-- | Low level access to PNG information
data PngLowLevel a = PngLowLevel
    { pngImage  :: Image a      -- ^ The real uncompressed image
    , pngChunks :: [PngChunk]
    }

-- | The pixels value should be :
-- +---+---+
-- | c | b |
-- +---+---+
-- | a | x |
-- +---+---+
-- x being the current filtered pixel
data PngFilter = 
    -- | Filt(x) = Orig(x), Recon(x) = Filt(x)
      FilterNone  
    -- | Filt(x) = Orig(x) - Orig(a), 	Recon(x) = Filt(x) + Recon(a)
    | FilterSub   
    -- | Filt(x) = Orig(x) - Orig(b), 	Recon(x) = Filt(x) + Recon(b)
    | FilterUp
    -- | Filt(x) = Orig(x) - floor((Orig(a) + Orig(b)) / 2),
    -- Recon(x) = Filt(x) + floor((Recon(a) + Recon(b)) / 2)
    | FilterAverage
    -- | Filt(x) = Orig(x) - PaethPredictor(Orig(a), Orig(b), Orig(c)),
    -- Recon(x) = Filt(x) + PaethPredictor(Recon(a), Recon(b), Recon(c))
    | FilterPaeth
    deriving (Enum, Show)

-- | Different known interlace methods for PNG image
data PngInterlaceMethod =
      -- | No interlacing, basic data ordering, line by line
      -- from left to right.
      PngNoInterlace

      -- | Use the Adam7 ordering, see `adam7Reordering` 
    | PngInterlaceAdam7
    deriving (Enum, Show)

--------------------------------------------------
----            Instances
--------------------------------------------------
instance Serialize PngFilter where
    put = putWord8 . toEnum . fromEnum
    get = toEnum . fromIntegral <$> getWord8

instance Serialize PngRawImage where
    put img = do
        putByteString pngSignature
        put $ header img
        mapM_ put $ chunks img

    get = parseRawPngImage

instance Serialize PngRawChunk where
    put chunk = do
        putWord32be $ chunkLength chunk
        putByteString $ chunkType chunk
        when (chunkLength chunk /= 0)
             (putByteString $ chunkData chunk)
        putWord32be $ chunkCRC chunk

    get = do
        size <- getWord32be
        chunkSig <- getByteString (B.length iHDRSignature)
        imgData <- if size == 0
            then return B.empty
            else getByteString (fromIntegral size)
        crc <- getWord32be

        let computedCrc = pngComputeCrc [chunkSig, imgData]
        when (computedCrc `xor` crc /= 0)
             (fail $ "Invalid CRC : " ++ show computedCrc ++ ", " 
                                      ++ show crc)
        return $ PngRawChunk {
        	chunkLength = size,
        	chunkData = imgData,
        	chunkCRC = crc,
        	chunkType = chunkSig
        }

instance Serialize PngIHdr where
    put hdr = do
        putWord32be 14
        putByteString iHDRSignature
        let inner = runPut $ do
                putWord32be $ width hdr
                putWord32be $ height hdr
                put $ bitDepth hdr
                put $ colourType hdr
                put $ filterMethod hdr
                put $ interlaceMethod hdr
            crc = pngComputeCrc $ [iHDRSignature, inner]
        putByteString inner
        putWord32be crc

    get = do
        _size <- getWord32be
        ihdrSig <- getByteString (B.length iHDRSignature)
        when (ihdrSig /= iHDRSignature)
             (fail "Invalid PNG file, wrong ihdr")
        w <- getWord32be
        h <- getWord32be
        depth <- get
        colorType <- get
        compression <- get
        filtermethod <- get
        interlace <- get
        _crc <- getWord32be
        return $ PngIHdr {
        	width = w,
        	height = h,
        	bitDepth = depth,
        	colourType = colorType,
            compressionMethod = compression,
            filterMethod = filtermethod,
            interlaceMethod = interlace
        }

parseRawPngImage :: Get PngRawImage
parseRawPngImage = do
    sig <- getByteString (B.length pngSignature)
    when (sig /= pngSignature)
         (fail "Invalid PNG file, signature broken")

    ihdr <- get

    chunkList <- parseChunks
    return $ PngRawImage { header = ihdr, chunks = chunkList }

parseChunks :: Get [PngRawChunk]
parseChunks = do
    chunk <- get

    if chunkType chunk == iENDSignature
       then return [chunk]
       else (chunk:) <$> parseChunks


instance Serialize PngInterlaceMethod where
    get = toEnum . fromIntegral <$> getWord8
    put = putWord8 . fromIntegral . fromEnum

--------------------------------------------------
----            functions
--------------------------------------------------

-- | Signature signalling that the following data will be a png image
-- in the png bit stream
pngSignature :: ChunkSignature
pngSignature = signature [137, 80, 78, 71, 13, 10, 26, 10]

-- | Return the image indices used for adam7 interlacing methods.
-- Each returned list return a pass of reaordering, there is 7 of
-- them, all resulting indices go from left to right, top to bottom.
adam7Indices :: Word32 -> Word32 -> [[(Word32,Word32)]]
adam7Indices imgWidth imgHeight =
  [[ (x,y) | y <- [yBeg, yBeg + dy .. imgHeight - 1]
           , x <- [xBeg, xBeg + dx .. imgWidth - 1] ]  
          | (xBeg, dx, yBeg, dy) <- infos]
    where starting_row  = [0, 0, 4, 0, 2, 0, 1]
          starting_col  = [0, 4, 0, 2, 0, 1, 0]
          row_increment = [8, 8, 8, 4, 4, 2, 2]
          col_increment = [8, 8, 4, 4, 2, 2, 1]

          infos = zip4 starting_col col_increment starting_row row_increment

-- | Given a line size in bytes, a line count (repetition count), split
-- a byte string of lines of the given size.
breakLines :: Word32 -> Word32 -> B.ByteString -> ([B.ByteString], B.ByteString)
breakLines size times wholestr = inner times [] wholestr
    where inner 0 lst rest = (lst, rest)
          inner n lst str = inner (n - 1) (lst ++ [piece]) rest
            where (piece, rest) = B.splitAt (fromIntegral size) str

-- | Apply a filtering method on a reduced image. Apply the filter
-- on each line.
pngFiltering :: Word32 -> B.ByteString -> (Word32, Word32) 
             -> (B.ByteString, B.ByteString) -- ^ Filtered scanlines, rest
pngFiltering beginZeroes str (imgWidth, imgHeight) = (B.pack filteredBytes , wholeRest)
    where (filterLines, wholeRest) = breakLines (imgWidth + 1) imgHeight str
          nullLine = repeat 0

          filteredBytes = concat imageLines
            where imageLines = map methodRead $ zip (nullLine : imageLines) filterLines

          stride = fromIntegral beginZeroes
          zeroes = replicate stride 0

          methodRead (prev, B.uncons -> Just (rawMeth, rest)) = drop stride thisLine
              where thisLine = zeroes ++ step (toEnum $ fromIntegral rawMeth) (prevLine, thisLine) (prev, rest)
                    prevLine = zeroes ++ prev
          methodRead _ = []

          step method (prevLineByte:prevLinerest, (prevByte:restLine)) (b : restPrev, B.uncons -> Just (x, rest)) =
              thisByte : step method (prevLinerest, restLine) (restPrev, rest)
                where thisByte = inner method (prevLineByte, b, prevByte, x)
          step _ _ _ = []

          inner :: PngFilter 
                -> (Word8, Word8, Word8, Word8)  -- (c, b, a, x)
                -> Word8
          inner FilterNone    (_,_,_,x) = x
          inner FilterSub     (_,_,a,x) = x + a
          inner FilterUp      (_,b,_,x) = x + b
          -- standard indicate that Orig(a) + Orig(b) shall be computed without overflow
          inner FilterAverage (_,b,a,x) = x + fromIntegral ((a' + b') `div` (2 :: Word16))
            where a' = fromIntegral a
                  b' = fromIntegral b
          inner FilterPaeth   (c,b,a,x) = x + paeth a b c

-- | Directly stolen from the definition in the standard (on W3C page)
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

-- | Transform a scanline to real data
unpackScanline :: Word32 -> Word32 -> Word32 -> Word32 -> Get [Word8]
unpackScanline 1 1 imgWidth imgHeight = 
   concat <$> replicateM (fromIntegral imgHeight) lineParser
    where split :: Word32 -> Word8 -> [Word8] -- avoid defaulting
          split times c = map (extractBit c) [times - 1, times - 2 .. 0]
          lineSize = imgWidth `quot` 8
          bitRest = imgWidth `mod` 8

          extractBit c shiftCount = (c `shiftR` fromIntegral shiftCount) .&. 1

          lineParser = do
            line <- concat <$> replicateM (fromIntegral lineSize) (split 8 <$> get)
            if bitRest == 0
                then return line
                else do lastElems <- split bitRest <$> get
                        return $ line ++ lastElems

unpackScanline 2 1 imgWidth imgHeight = concat <$> replicateM (fromIntegral imgHeight) lineParser
    where split :: Word32 -> Word8 -> [Word8] -- avoid defaulting
          split times c = map (extractBit c) [times - 1, times - 2 .. 0]
          lineSize = imgWidth `quot` 4
          bitRest = imgWidth `mod` 4

          extractBit c shiftCount = (c `shiftR` (fromIntegral shiftCount * 2)) .&. 0x3

          lineParser = do
            line <- concat <$> replicateM (fromIntegral lineSize) (split 4 <$> get)
            if bitRest == 0
                then return line
                else do lastElems <- split bitRest <$> get
                        return $ line ++ lastElems

unpackScanline 4 sampleCount imgWidth imgHeight = concat <$> replicateM (fromIntegral imgHeight) lineParser
    where split :: Word8 -> [Word8]
          split c = [(c `shiftR` 4) .&. 0xF, (c .&. 0xF)]
          lineSize = fromIntegral $ imgWidth `quot` 2
          isFullLine = (imgWidth * sampleCount) `mod` 2 == 0

          lineParser = do
            line <- concat <$> replicateM lineSize (split <$> get)
            if isFullLine 
                then return line
                else do lastElem <- ((head . split) <$> get)
                        return $ line ++ [lastElem]

unpackScanline 8 sampleCount imgWidth imgHeight = 
    replicateM (fromIntegral $ imgWidth * imgHeight * sampleCount) getWord8
unpackScanline 16 sampleCount imgWidth imgHeight = 
    replicateM (fromIntegral $ imgWidth * imgHeight * sampleCount) (fromIntegral . (`div` 256) <$> getWord16be)
unpackScanline _ _ _ _ = fail "Impossible bit depth"

type Unpacker a = B.ByteString -> Either String (Image a)

pixelizeRawData :: (ColorConvertible a b) => [a] -> [Maybe b]
pixelizeRawData [] = []
pixelizeRawData lst = px : pixelizeRawData rest
    where (px, rest) = fromRawData lst

scanLineFilterUnpack :: (ColorConvertible Word8 a) 
                     => Word32 -> Word32 -> B.ByteString -> (Word32, Word32)
                     -> Either String ([a], B.ByteString)
scanLineFilterUnpack depth sampleCount bytes (imgWidth, imgHeight) = do
  let scanlineByteSize = byteSizeOfBitLength depth sampleCount imgWidth 
      stride = if depth >= 8 then sampleCount * (depth `div` 8) else 1
      (filtered, rest) = pngFiltering stride bytes (scanlineByteSize, imgHeight)
      unpackedBytes = runGet (unpackScanline depth sampleCount imgWidth imgHeight) filtered
  (\a -> (catMaybes $ pixelizeRawData a, rest)) <$> unpackedBytes

-- | Recreate image from normal (scanlines) png image.
scanLineUnpack :: (IArray UArray a, ColorConvertible Word8 a)
               => Word32 -> Word32 -> Word32 -> Word32
               -> Unpacker a
scanLineUnpack depth sampleCount imgWidth imgHeight bytes = case scanLineData of
        Left err -> Left err
        Right (unpacked,_) -> Right . array ((0,0), (imgWidth - 1, imgHeight - 1)) 
                                    $ zip pixelsIndices unpacked
    where pixelsIndices = [(x,y) | y <- [0 .. imgHeight - 1], x <- [0 .. imgWidth - 1]]
          scanLineData = scanLineFilterUnpack depth sampleCount bytes (imgWidth, imgHeight)

byteSizeOfBitLength :: Word32 -> Word32 -> Word32 -> Word32
byteSizeOfBitLength pixelBitDepth sampleCount dimension = size + (if rest /= 0 then 1 else 0)
   where (size, rest) = (pixelBitDepth * dimension * sampleCount) `quotRem` 8

eitherMapAccumL :: (acc -> x -> Either err (acc, y)) -- Function of elt of input list
                                    -- and accumulator, returning new
                                    -- accumulator and elt of result list
                -> acc            -- Initial accumulator 
                -> [x]            -- Input list
                -> Either err (acc, [y])     -- Final accumulator and result list
eitherMapAccumL _ s []        =  Right (s, [])
eitherMapAccumL f s (x:xs)    = case f s x of
        Left e -> Left e
        Right (s', y) -> case eitherMapAccumL f s' xs of
                            Left err -> Left err
                            Right (s'', ys) -> Right (s'',y:ys)

-- | Given data and image size, recreate an image with deinterlaced
-- data for PNG's adam 7 method.
adam7Unpack :: (IArray UArray a, ColorConvertible Word8 a) => Word32 -> Word32 -> Word32 -> Word32 -> Unpacker a
adam7Unpack depth sampleCount imgWidth imgHeight bytes = case passes of
        Left err -> Left err
        Right (_, passBytes) -> let pixels = concat [zip idxs pass | (idxs, pass) <- zip pixelIndices passBytes]
                               in Right $ array ((0,0), (imgWidth - 1, imgHeight - 1)) pixels
    where pixelIndices = adam7Indices imgWidth imgHeight

          -- given the sizes of the pass, will produce bytetring of the deseried
          -- sizes.
          passes = eitherMapAccumL (\acc -> eitherSwap . scanLineFilterUnpack depth sampleCount acc) 
                                    bytes passSizes

          eitherSwap (Left a) = Left a
          eitherSwap (Right (a,b)) = Right (b,a)

          passSizes = [ (passWidth bw, passHeight bh) | (bw, bh) <- zip block_width block_height ]

          passHeight bh = size + (if restSize /= 0 then 1 else 0)
            where (size, restSize) = (depth * imgHeight) `quotRem` bh

          passWidth bw = size + (if restSize /= 0 then 1 else 0)
            where (size, restSize) = (depth * imgWidth) `quotRem` bw

          block_height = [8, 8, 4, 4, 2, 2, 1]
          block_width = [8, 4, 4, 2, 2, 1, 1]

-- | deinterlace picture in function of the method indicated
-- in the iHDR
deinterlacer :: (ColorConvertible Word8 a, IArray UArray a)
             => PngIHdr -> Unpacker a
deinterlacer ihdr = fun (fromIntegral $ bitDepth ihdr) sampleCount
                        (width ihdr) (height ihdr)
    where fun = case interlaceMethod ihdr of 
                    PngNoInterlace -> scanLineUnpack
                    PngInterlaceAdam7 -> adam7Unpack
          sampleCount = sampleCountOfImageType $ colourType ihdr

-- | Helper function to help pack signatures.
signature :: [Word8] -> ChunkSignature 
signature = B.pack . map (toEnum . fromEnum)

-- | Signature for all the critical chunks in a PNG image.
iHDRSignature, iDATSignature, iENDSignature, pLTESignature :: ChunkSignature 
iHDRSignature = signature [73, 72, 68, 82]
pLTESignature = signature [80, 76, 84, 69]
iDATSignature = signature [73, 68, 65, 84]
iENDSignature = signature [73, 69, 78, 68]

instance Serialize PngImageType where
    put PngGreyscale = putWord8 0
    put PngTrueColour = putWord8 2
    put PngIndexedColor = putWord8 3
    put PngGreyscaleWithAlpha = putWord8 4
    put PngTrueColourWithAlpha = putWord8 6

    get = get >>= imageTypeOfCode 

imageTypeOfCode :: Word8 -> Get PngImageType
imageTypeOfCode 0 = return PngGreyscale 
imageTypeOfCode 2 = return PngTrueColour
imageTypeOfCode 3 = return PngIndexedColor
imageTypeOfCode 4 = return PngGreyscaleWithAlpha
imageTypeOfCode 6 = return PngTrueColourWithAlpha
imageTypeOfCode _ = fail "Invalid png color code"

-- | From the Annex D of the png specification.
pngCrcTable :: UArray Word32 Word32
pngCrcTable = listArray (0, 255) [ foldl' updateCrcConstant c [zero .. 7] | c <- [0 .. 255] ]
    where zero = 0 :: Int -- To avoid defaulting to Integer
          updateCrcConstant c _ | c .&. 1 /= 0 = magicConstant `xor` (c `shiftR` 1)
                                | otherwise = c `shiftR` 1
          magicConstant = 0xedb88320 :: Word32

-- | Compute the CRC of a raw buffer, as described in annex D of the PNG
-- specification.
pngComputeCrc :: [B.ByteString] -> Word32
pngComputeCrc = (0xFFFFFFFF `xor`) . B.foldl' updateCrc 0xFFFFFFFF . B.concat
    where updateCrc crc val =
              let u32Val = fromIntegral val
                  lutVal = pngCrcTable ! ((crc `xor` u32Val) .&. 0xFF)
              in lutVal `xor` (crc `shiftR` 8)

-- | Type used for png parser/converter
type PngParser a = Maybe PngPalette -> PngImageType -> PngIHdr -> B.ByteString
                 -> Either String (Image a)

-- | Parse a greyscale png
unparsePixel8 :: PngParser Pixel8
unparsePixel8 _ PngGreyscale ihdr bytes = deinterlacer ihdr bytes
unparsePixel8 _ PngIndexedColor ihdr bytes = deinterlacer ihdr bytes
unparsePixel8 _ _ _ _ = Left "Cannot reduce data kind"

type ErrImage a = Either String (Image a)

-- | Parse a greyscale with alpha channel
unparsePixelYA8 :: PngParser PixelYA8
unparsePixelYA8 _ PngGreyscale ihdr bytes = promotePixels <$> img
    where img = deinterlacer ihdr bytes :: ErrImage Pixel8
unparsePixelYA8 _ PngGreyscaleWithAlpha ihdr bytes = img
    where img = deinterlacer ihdr bytes :: ErrImage PixelYA8
unparsePixelYA8 _ _ _ _ = Left "Cannot reduce data kind"

unparsePixelRGB8 :: PngParser PixelRGB8
unparsePixelRGB8 _ PngGreyscale ihdr bytes = promotePixels <$> img
    where img = deinterlacer ihdr bytes :: ErrImage Pixel8
unparsePixelRGB8 (Just plte) PngIndexedColor ihdr bytes = amap ((plte !) . fromIntegral) <$> img
    where img = deinterlacer ihdr bytes :: ErrImage Pixel8
unparsePixelRGB8 Nothing PngIndexedColor _ _ = Left "no valid palette found"
unparsePixelRGB8 _ PngTrueColour ihdr bytes = img
    where img = deinterlacer ihdr bytes :: ErrImage PixelRGB8
unparsePixelRGB8 _ PngGreyscaleWithAlpha ihdr bytes = promotePixels <$> img
    where img = deinterlacer ihdr bytes :: ErrImage PixelYA8
unparsePixelRGB8 _ _ _ _ = Left "Cannot reduce data kind"

generateGreyscalePalette :: Word8 -> PngPalette 
generateGreyscalePalette times = listArray (0, fromIntegral possibilities) pixels
    where possibilities = 2 ^ times - 1
          pixels = [PixelRGB8 i i i | n <- [0..possibilities], let i = n * (255 `div` possibilities)]

paletteRGBA_1, paletteRGBA_2, paletteRGBA_4  :: PngPalette 
paletteRGBA_1 = generateGreyscalePalette 1
paletteRGBA_2 = generateGreyscalePalette 2
paletteRGBA_4 = generateGreyscalePalette 4

unparsePixelRGBA8 :: PngParser PixelRGBA8
unparsePixelRGBA8 _ PngGreyscale ihdr bytes 
    | bitDepth ihdr == 1 = unparsePixelRGBA8 (Just paletteRGBA_1) PngIndexedColor ihdr bytes
    | bitDepth ihdr == 2 = unparsePixelRGBA8 (Just paletteRGBA_2) PngIndexedColor ihdr bytes
    | bitDepth ihdr == 4 = unparsePixelRGBA8 (Just paletteRGBA_4) PngIndexedColor ihdr bytes
    | otherwise = promotePixels <$> img
        where img = deinterlacer ihdr bytes :: ErrImage Pixel8
unparsePixelRGBA8 (Just plte) PngIndexedColor ihdr bytes = amap (promotePixel . (plte !) . fromIntegral) <$> img
    where img = deinterlacer ihdr bytes :: ErrImage Pixel8
unparsePixelRGBA8 Nothing PngIndexedColor _ _ = Left "no valid palette found"
unparsePixelRGBA8 _ PngTrueColour ihdr bytes = promotePixels <$> img
    where img = deinterlacer ihdr bytes :: ErrImage PixelRGB8
unparsePixelRGBA8 _ PngGreyscaleWithAlpha ihdr bytes = promotePixels <$> img
    where img = deinterlacer ihdr bytes :: ErrImage PixelYA8
unparsePixelRGBA8 _ PngTrueColourWithAlpha ihdr bytes = img
    where img = deinterlacer ihdr bytes :: ErrImage PixelRGBA8

-- | Class to use in order to load a png in a given pixel type,
-- you can choose a given pixel type, the library will convert
-- any real pixel type, if possible to your pixel type
class (IArray UArray a) => PngLoadable a where
    -- | Doesn't change the type if the pixel type inside the
    -- image is the same as the requested one, otherwise try
    -- to perform a promotion.
    --
    -- Pixel promotion avoid losing any value, if a type promotion
    -- cannot happen, an error message is returned. For example,
    -- you can :
    --   - convert from greyscale to rgb
    --   - convert from rgb to rgba
    -- but you can't:
    --   - convert from rgba to rgb
    --   - convert from rgb to greyscale with alpha
    decodePng :: B.ByteString -> Either String (Image a)

instance PngLoadable Pixel8 where
    decodePng = pngUnparser unparsePixel8

instance PngLoadable PixelYA8 where
    decodePng = pngUnparser unparsePixelYA8

instance PngLoadable PixelRGB8 where
    decodePng = pngUnparser unparsePixelRGB8

instance PngLoadable PixelRGBA8 where
    decodePng = pngUnparser unparsePixelRGBA8

-- | Load a png file, perform the same casts as 'decodePng'
loadPng :: (PngLoadable a) => FilePath -> IO (Either String (Image a))
loadPng f = decodePng <$> B.readFile f

pngUnparser :: (IArray UArray a, ColorConvertible Word8 a)
            => PngParser a -> B.ByteString -> Either String (Image a)
pngUnparser unparser byte = do
    rawImg <- runGet get byte
    let ihdr = header rawImg
        compressedImageData = 
              B.concat [chunkData chunk | chunk <- chunks rawImg
                                        , chunkType chunk == iDATSignature]
        imgData = Z.decompress $ Lb.fromChunks [compressedImageData]
        palette = case find (\c -> pLTESignature == chunkType c) $ chunks rawImg of
            Nothing -> Nothing
            Just p -> case parsePalette p of
                    Left _ -> Nothing
                    Right plte -> Just plte

    unparser palette (colourType ihdr) ihdr . B.concat $ Lb.toChunks imgData


