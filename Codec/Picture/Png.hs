module Codec.Picture.Png where

import Control.Applicative
import Control.Monad( when )
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Array.Unboxed
import Data.List( foldl' )
import Data.Bits
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lb

-- | Signature signalling that the following data will be a png image
-- in the png bit stream
pngSignature :: ChunkSignature
pngSignature = signature [137, 80, 78, 71, 13, 10, 26, 10]

starting_row, starting_col, row_increment, col_increment, 
    block_height, block_width :: UArray Word32 Word32
starting_row  = listArray (0, 7) [0, 0, 4, 0, 2, 0, 1]
starting_col  = listArray (0, 7) [0, 4, 0, 2, 0, 1, 0]
row_increment = listArray (0, 7) [8, 8, 8, 4, 4, 2, 2]
col_increment = listArray (0, 7) [8, 8, 4, 4, 2, 2, 1]
block_height  = listArray (0, 7) [8, 8, 4, 4, 2, 2, 1]
block_width   = listArray (0, 7) [8, 4, 4, 2, 2, 1, 1]

adam7Matrix :: UArray (Word32, Word32) Word32
adam7Matrix   = array ((0,0), (7,7)) [((j,i), val) | (j, row) <- zip [0..] coefsList
                                                   , (i, val) <- zip [0..] row ]
    where coefsList =
            [ [0, 5, 3, 5, 1, 5, 3, 5]
            , [6, 6, 6, 6, 6, 6, 6, 6]
            , [4, 5, 4, 5, 4, 5, 4, 5]
            , [6, 6, 6, 6, 6, 6, 6, 6]
            , [2, 5, 3, 5, 2, 5, 3, 5]
            , [6, 6, 6, 6, 6, 6, 6, 6]
            , [4, 5, 4, 5, 4, 5, 4, 5]
            , [6, 6, 6, 6, 6, 6, 6, 6] ]


-- | This reordering is used for progressive PNG, creating
-- 7 "buckets" to store pixel to.
adam7Reordering :: Word32 -> Word32 -> Word32
adam7Reordering x y = adam7Matrix ! (x `mod` 8, y `mod` 8)

type ChunkSignature = B.ByteString
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

instance Binary PngFilter where
    put = putWord8 . toEnum . fromEnum
    get = toEnum . fromIntegral <$> getWord8

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

data PngRawImage = PngRawImage
    { header       :: PngIHdr
    , chunks       :: [PngChunk]
    }

instance Binary PngRawImage where
    put img = do
        putByteString pngSignature
        put $ header img
        mapM_ put $ chunks img

    get = parseRawPngImage

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


data PngChunk = PngChunk
    { chunkLength :: Word32
    , chunkType   :: ChunkSignature
    , chunkCRC    :: Word32
    , chunkData   :: B.ByteString
    }

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

-- | Different known interlace methods for PNG image
data PngInterlaceMethod =
      -- | No interlacing, basic data ordering, line by line
      -- from left to right.
      PngNoInterlace

      -- | Use the Adam7 ordering, see `adam7Reordering` 
    | PngInterlaceAdam7
    deriving (Enum, Show)

instance Binary PngInterlaceMethod where
    get = toEnum . fromIntegral <$> getWord8
    put = putWord8 . fromIntegral . fromEnum

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

data PngImageType =
      PngGreyscale
    | PngTrueColour
    | PngIndexedColor
    | PngGreyscaleWithAlpha
    | PngTrueColourWithAlpha
    deriving Show

-- | Helper function to help pack signatures.
signature :: [Word8] -> ChunkSignature 
signature = B.pack . map (toEnum . fromEnum)

-- | Signature for all the critical chunks in a PNG image.
iHDRSignature, pLTESignature, iDATSignature, iENDSignature :: ChunkSignature 
iHDRSignature = signature [73, 72, 68, 82]
pLTESignature = signature [80, 76, 84, 69]
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

loadRawPng :: Lb.ByteString -> PngRawImage 
loadRawPng = runGet get

{-putGreyScalePng :: PngFilter -> PngInterlaceMethod -> UArray (Word32, Word32) Word32-}

class PngRepresentable a where
    unpack :: PngIHdr -> Lb.ByteString -> UArray (Word32, Word32) a


loadPng :: Lb.ByteString -> ()
loadPng byte = ()
    where rawImg = runGet get byte

          imgData :: Lb.ByteString
          imgData = Z.decompress compressedImageData

          compressedImageData :: Lb.ByteString
          compressedImageData = 
                Lb.fromChunks [chunkData chunk | chunk <- chunks rawImg
                                               , chunkType chunk == iDATSignature ]

