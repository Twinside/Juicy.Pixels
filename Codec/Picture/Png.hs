{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Module used for loading & writing \'Portable Network Graphics\' (PNG)
-- files. The API has two layers, the high level, which load the image without
-- looking deeply about it and the low level, allowing access to data chunks contained
-- in the PNG image.
--
-- For general use, please use 'loadPng' function.
module Codec.Picture.Png( -- * Low level types
                          PngIHdr( .. )
                        , PngImageType( .. )
                        , PngChunk( .. )
                        , PngLowLevel( .. )

                          -- * High level functions
                        , PngLoadable( .. )
                        ) where

import Control.Applicative
import Control.Monad( when, replicateM )
import Data.Bits
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Array.Unboxed
import Data.List( foldl', zip4, mapAccumL )
import Data.Tuple( swap )
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
    { width             :: Word32
    , height            :: Word32
    , bitDepth          :: Word8
    , colourType        :: PngImageType 
    , compressionMethod :: Word8
    , filterMethod      :: PngFilter
    , interlaceMethod   :: PngInterlaceMethod
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

data PngRawImage = PngRawImage
    { header       :: PngIHdr
    , chunks       :: [PngChunk]
    }

data PngChunk = PngChunk
    { chunkLength :: Word32
    , chunkType   :: ChunkSignature
    , chunkCRC    :: Word32
    , chunkData   :: B.ByteString
    }

-- | Low level access to PNG information
data PngLowLevel a = PngLowLevel
    { pngHeader :: PngIHdr
    , pngImage  :: Image a
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
instance Binary PngFilter where
    put = putWord8 . toEnum . fromEnum
    get = toEnum . fromIntegral <$> getWord8

instance Binary PngRawImage where
    put img = do
        putByteString pngSignature
        put $ header img
        mapM_ put $ chunks img

    get = parseRawPngImage

instance Binary PngChunk where
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
        return $ PngChunk {
        	chunkLength = size,
        	chunkData = imgData,
        	chunkCRC = crc,
        	chunkType = chunkSig
        }

instance Binary PngIHdr where
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
            crc = pngComputeCrc $ iHDRSignature : Lb.toChunks inner
        putLazyByteString inner
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

parseChunks :: Get [PngChunk]
parseChunks = do
    chunk <- get

    if chunkType chunk == iENDSignature
       then return [chunk]
       else (chunk:) <$> parseChunks


instance Binary PngInterlaceMethod where
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
breakLines :: Word32 -> Word32 -> Lb.ByteString -> ([Lb.ByteString], Lb.ByteString)
breakLines size times wholestr = inner times [] wholestr
    where inner 0 lst rest = (lst, rest)
          inner n lst str = inner (n - 1) (lst ++ [piece]) rest
            where (piece, rest) = Lb.splitAt (fromIntegral size) str

-- | Apply a filtering method on a reduced image. Apply the filter
-- on each line.
pngFiltering :: PngFilter -> Lb.ByteString -> (Word32, Word32) 
             -> (Lb.ByteString, Lb.ByteString) -- ^ Filtered scanlines, rest
pngFiltering method str (imgWidth, imgHeight) = (Lb.pack filteredBytes , wholeRest)
    where (filterLines, wholeRest) = breakLines imgWidth imgHeight str
          nullLine = Lb.replicate (fromIntegral imgWidth) 0

          filteredBytes = concatMap (step (0, 0)) $ zip (nullLine : filterLines) filterLines

          step (prevLineByte, prevByte) (Lb.uncons -> Just (b, restPrev), Lb.uncons -> Just (x, rest)) =
              innerMethod (prevLineByte, b, prevByte, x) : step (b, x) (restPrev, rest)
          step _ _ = []

          innerMethod = inner method

          inner :: PngFilter 
                -> (Word8, Word8, Word8, Word8)  -- (c, b, a, x)
                -> Word8
          inner FilterNone    (_,_,_,x) = x
          inner FilterSub     (_,_,a,x) = x + a
          inner FilterUp      (_,b,_,x) = x + b
          inner FilterAverage (_,b,a,x) = x + (a + b) `div` 2
          inner FilterPaeth   (c,b,a,x) = x + paeth a b c

-- | Directly stolen from the definition in the standard (on W3C page)
paeth :: Word8 -> Word8 -> Word8 -> Word8
paeth a b c
  | pa <= pb && pa <= pc = a
  | pb <= pc              = b
  | otherwise             = c
    where p = a + b - c
          pa = abs $ p - a
          pb = abs $ p - b
          pc = abs $ p - c


-- | Transform a scanline to real data
unpackScanline :: (Binary a, IArray UArray a) => Word32 -> Word32 -> Word32 -> Get [a]
{-unpackScanline 1 imgWidth imgHeight = replicateM (fromIntegral $ imgWidth * imgHeight) (fromIntegral <$> (get :: Get Word8))-}
{-unpackScanline 2 imgWidth imgHeight = replicateM (fromIntegral $ imgWidth * imgHeight) (fromIntegral <$> (get :: Get Word8))-}
{-unpackScanline 4 imgWidth imgHeight = replicateM (fromIntegral $ imgWidth * imgHeight) (fromIntegral <$> (get :: Get Word8))-}
{-unpackScanline 8 imgWidth imgHeight = replicateM (fromIntegral $ imgWidth * imgHeight) (fromIntegral <$> (get :: Get Word8))-}
{-unpackScanline 16 imgWidth imgHeight = replicateM (fromIntegral $ imgWidth * imgHeight) (fromIntegral <$> (get :: Get Word16))-}
unpackScanline _ _ _ = fail "Impossible bit depth"

type Unpacker a =  Lb.ByteString -> Image a

scanLineFilterUnpack :: (Binary a, IArray UArray a) => PngFilter -> Word32 -> Lb.ByteString -> (Word32, Word32)
                     -> ([a], Lb.ByteString)
scanLineFilterUnpack pngfilter depth bytes (imgWidth, imgHeight) =
  (runGet (unpackScanline depth imgWidth imgHeight) filtered, rest)
    where scanlineByteSize = byteSizeOfBitLength depth imgWidth 
          (filtered, rest) = pngFiltering pngfilter bytes (scanlineByteSize, imgHeight)

-- | Recreate image from normal (scanlines) png image.
scanLineUnpack :: (Binary a, IArray UArray a) => PngFilter -> Word32 -> Word32 -> Word32
               -> Unpacker a
scanLineUnpack pngfilter depth imgWidth imgHeight bytes = 
  array ((0,0), (imgWidth - 1, imgHeight - 1)) $ zip pixelsIndices unpacked
    where pixelsIndices = [(x,y) | y <- [0 .. imgHeight - 1], x <- [0 .. imgWidth - 1]]
          (unpacked, _) = scanLineFilterUnpack pngfilter depth bytes (imgWidth, imgHeight)

byteSizeOfBitLength :: Word32 -> Word32 -> Word32
byteSizeOfBitLength pixelBitDepth dimension = size + (if rest /= 0 then 1 else 0)
   where (size, rest) = (pixelBitDepth * dimension) `quotRem` 8

-- | Given data and image size, recreate an image with deinterlaced
-- data for PNG's adam 7 method.
adam7Unpack :: (Binary a, IArray UArray a) => PngFilter -> Word32 -> Word32 -> Word32 -> Unpacker a
adam7Unpack pngfilter depth imgWidth imgHeight bytes = array ((0,0), (imgWidth - 1, imgHeight - 1)) pixels
    where pixels = concat [zip idxs pass | (idxs, pass) <- zip pixelIndices passBytes]
          pixelIndices = adam7Indices imgWidth imgHeight

          -- given the sizes of the pass, will produce bytetring of the deseried
          -- sizes.
          (_, passBytes) = mapAccumL (\acc -> swap . scanLineFilterUnpack pngfilter depth acc) bytes passSizes

          passSizes = [ (passWidth bw, passHeight bh) | (bw, bh) <- zip block_width block_height ]

          passHeight bh = size + (if restSize /= 0 then 1 else 0)
            where (size, restSize) = (depth * imgHeight) `quotRem` bh

          passWidth bw = size + (if restSize /= 0 then 1 else 0)
            where (size, restSize) = (depth * imgWidth) `quotRem` bw

          block_height = [8, 8, 4, 4, 2, 2, 1]
          block_width = [8, 4, 4, 2, 2, 1, 1]

-- | deinterlace picture in function of the method indicated
-- in the iHDR
deinterlacer :: (Binary a, IArray UArray a)
             => PngInterlaceMethod -> PngFilter -> Word32 -> Word32 -> Word32 -> Unpacker a
deinterlacer PngNoInterlace = scanLineUnpack
deinterlacer PngInterlaceAdam7 = adam7Unpack

-- | Helper function to help pack signatures.
signature :: [Word8] -> ChunkSignature 
signature = B.pack . map (toEnum . fromEnum)

-- | Signature for all the critical chunks in a PNG image.
iHDRSignature, iDATSignature, iENDSignature :: ChunkSignature 
iHDRSignature = signature [73, 72, 68, 82]
-- pLTESignature = signature [80, 76, 84, 69]
iDATSignature = signature [73, 68, 65, 84]
iENDSignature = signature [73, 69, 78, 68]

instance Binary PngImageType where
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

type PngParser a = PngImageType 
                 -> (forall b. Lb.ByteString -> Image b)
                 -> Lb.ByteString
                 -> Either String (UArray (Word32, Word32) a)

-- | Parse a greyscale png
unparsePixel8 :: PngParser Pixel8
unparsePixel8 PngGreyscale f bytes = Right $ f bytes
unparsePixel8 PngIndexedColor f bytes = Right $ f bytes
unparsePixel8 _ _ _ = Left "Cannot reduce bit depth of image"

-- | Parse a greyscale with alpha channel
unparsePixelYA8 :: PngParser PixelYA8
unparsePixelYA8 PngGreyscale f bytes = Right $ promotePixels (f bytes :: Image Pixel8)
unparsePixelYA8 PngIndexedColor f bytes = Right $ promotePixels (f bytes :: Image Pixel8)
unparsePixelYA8 PngGreyscaleWithAlpha f bytes = Right $ (f bytes :: Image PixelYA8)
unparsePixelYA8 _ _ _ = Left "Cannot reduce bit depth of image"

{-unparsePixelRGB8 :: PngImageType-}
                 {--> (Lb.ByteString -> UArray (Word32, Word32) b)-}
                 {--> Lb.ByteString-}
                 {--> Either String (UArray (Word32, Word32) PixelRGB8)-}
{-unparsePixelRGB8 PngGreyscale f bytes = Right . promotePixels $ f bytes-}
{-unparsePixelRGB8 PngTrueColour =-}
{-unparsePixelRGB8 PngIndexedColor =-}
{-unparsePixelRGB8 PngGreyscaleWithAlpha =-}
{-unparsePixelRGB8 PngTrueColourWithAlpha =-}

class (IArray UArray a) => PngLoadable a where
    loadPng :: Lb.ByteString -> Either String (Image a)

instance PngLoadable Pixel8 where
    loadPng = pngUnparser unparsePixel8

instance PngLoadable PixelYA8 where
    loadPng = pngUnparser unparsePixelYA8

pngUnparser :: forall a. (IArray UArray a, Binary a)
            => PngParser a -> Lb.ByteString -> Either String (Image a)
pngUnparser unparser byte = unparser (colourType ihdr) 
                                    unpacker imgData
    where rawImg = runGet get byte
          ihdr = header rawImg
          
          unpacker :: (Binary b, IArray UArray b) => Unpacker b
          unpacker = ((deinterlacer $ interlaceMethod ihdr) 
                            (filterMethod ihdr)
                            (fromIntegral $ bitDepth ihdr)
                            (width ihdr)
                            (height ihdr)

          imgData :: Lb.ByteString
          imgData = Z.decompress compressedImageData

          compressedImageData :: Lb.ByteString
          compressedImageData = 
                Lb.fromChunks [chunkData chunk | chunk <- chunks rawImg
                                               , chunkType chunk == iDATSignature]

