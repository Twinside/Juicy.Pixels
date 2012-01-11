module Codec.Picture.Png.Type( PngIHdr( .. )
                             , PngFilter( .. )
                             , PngInterlaceMethod( .. )
                             , PngPalette
                             , PngImageType( .. )
                             , parsePalette 
                             , pngComputeCrc
                             , pLTESignature
                             , iDATSignature
                             , iENDSignature
                             -- * Low level types
                             , ChunkSignature
                             , PngRawImage( .. )
                             , PngChunk( .. )
                             , PngRawChunk( .. )
                             , PngLowLevel( .. )
                             ) where

import Control.Applicative( (<$>) )
import Control.Monad( when, replicateM )
import Data.Bits( xor, (.&.), shiftR )
import Data.Serialize( Serialize(..), Get, get, runGet, runPut
                     , putWord8, getWord8
                     , putWord32be, getWord32be
                     , getByteString, putByteString )
import Data.Array.Unboxed( Array, UArray, listArray, (!) )
import Data.List( foldl' )
import Data.Word( Word32, Word8 )
import qualified Data.ByteString as B

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

data PngRawImage = PngRawImage
    { header       :: PngIHdr
    , chunks       :: [PngRawChunk]
    }

type PngPalette = Array Int PixelRGB8

parsePalette :: PngRawChunk -> Either String PngPalette
parsePalette plte
 | chunkLength plte `mod` 3 /= 0 = Left "Invalid palette size"
 | otherwise = listArray (0, pixelCount - 1) <$> runGet pixelUnpacker (chunkData plte)
    where pixelUnpacker = replicateM (fromIntegral pixelCount) get
          pixelCount = fromIntegral $ chunkLength plte `div` 3

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
    get = getWord8 >>= \w -> case w of
        0 -> return FilterNone
        1 -> return FilterSub
        2 -> return FilterUp
        3 -> return FilterAverage
        4 -> return FilterPaeth
        _ -> fail "Invalid scanline filter"

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
        return PngRawChunk {
        	chunkLength = size,
        	chunkData = imgData,
        	chunkCRC = crc,
        	chunkType = chunkSig
        }

instance Serialize PngIHdr where
    put hdr = do
        putWord32be 13
        let inner = runPut $ do
                putByteString iHDRSignature
                putWord32be $ width hdr
                putWord32be $ height hdr
                putWord8    $ bitDepth hdr
                put $ colourType hdr
                put $ compressionMethod hdr
                put $ filterMethod hdr
                put $ interlaceMethod hdr
            crc = pngComputeCrc [inner]
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
        return PngIHdr {
        	width = w,
        	height = h,
        	bitDepth = depth,
        	colourType = colorType,
            compressionMethod = compression,
            filterMethod = filtermethod,
            interlaceMethod = interlace
        }

-- | Parse method for a png chunk, without decompression.
parseChunks :: Get [PngRawChunk]
parseChunks = do
    chunk <- get

    if chunkType chunk == iENDSignature
       then return [chunk]
       else (chunk:) <$> parseChunks


instance Serialize PngInterlaceMethod where
    get = getWord8 >>= \w -> case w of
        0 -> return PngNoInterlace
        1 -> return PngInterlaceAdam7
        _ -> fail "Invalid interlace method"

    put PngNoInterlace    = putWord8 0
    put PngInterlaceAdam7 = putWord8 1

-- | Implementation of the get method for the PngRawImage,
-- unpack raw data, without decompressing it.
parseRawPngImage :: Get PngRawImage
parseRawPngImage = do
    sig <- getByteString (B.length pngSignature)
    when (sig /= pngSignature)
         (fail "Invalid PNG file, signature broken")

    ihdr <- get

    chunkList <- parseChunks
    return PngRawImage { header = ihdr, chunks = chunkList }

--------------------------------------------------
----            functions
--------------------------------------------------

-- | Signature signalling that the following data will be a png image
-- in the png bit stream
pngSignature :: ChunkSignature
pngSignature = signature [137, 80, 78, 71, 13, 10, 26, 10]

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

