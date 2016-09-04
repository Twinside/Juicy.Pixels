{-# LANGUAGE CPP #-}
-- | Low level png module, you should import 'Codec.Picture.Png' instead.
module Codec.Picture.Png.Type( PngIHdr( .. )
                             , PngFilter( .. )
                             , PngInterlaceMethod( .. )
                             , PngPalette
                             , PngImageType( .. )
                             , PngPhysicalDimension( .. )
                             , PngGamma( .. )
                             , PngUnit( .. )
                             , APngAnimationControl( .. )
                             , APngFrameDisposal( .. )
                             , APngBlendOp( .. )
                             , APngFrameControl( .. )
                             , parsePalette 
                             , pngComputeCrc
                             , pLTESignature
                             , iDATSignature
                             , iENDSignature
                             , tRNSSignature
                             , tEXtSignature
                             , zTXtSignature
                             , gammaSignature
                             , pHYsSignature
                             , animationControlSignature
                             -- * Low level types
                             , ChunkSignature
                             , PngRawImage( .. )
                             , PngChunk( .. )
                             , PngRawChunk( .. )
                             , PngLowLevel( .. )
                             , chunksWithSig
                             , mkRawChunk
                             ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>), (<*>), pure )
#endif

import Control.Monad( when, replicateM )
import Data.Bits( xor, (.&.), unsafeShiftR )
import Data.Binary( Binary(..), Get, get )
import Data.Binary.Get( getWord8
                      , getWord32be
                      , getLazyByteString
                      )
import Data.Binary.Put( runPut
                      , putWord8
                      , putWord32be
                      , putLazyByteString
                      )
import Data.Vector.Unboxed( Vector, fromListN, (!) )
import qualified Data.Vector.Storable as V
import Data.List( foldl' )
import Data.Word( Word32, Word16, Word8 )
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LS

import Codec.Picture.Types
import Codec.Picture.InternalHelper

--------------------------------------------------
----            Types
--------------------------------------------------

-- | Value used to identify a png chunk, must be 4 bytes long.
type ChunkSignature = L.ByteString

-- | Generic header used in PNG images.
data PngIHdr = PngIHdr
    { width             :: !Word32       -- ^ Image width in number of pixel
    , height            :: !Word32       -- ^ Image height in number of pixel
    , bitDepth          :: !Word8        -- ^ Number of bit per sample
    , colourType        :: !PngImageType -- ^ Kind of png image (greyscale, true color, indexed...)
    , compressionMethod :: !Word8        -- ^ Compression method used
    , filterMethod      :: !Word8        -- ^ Must be 0
    , interlaceMethod   :: !PngInterlaceMethod   -- ^ If the image is interlaced (for progressive rendering)
    }
    deriving Show

data PngUnit
    = PngUnitUnknown -- ^ 0 value
    | PngUnitMeter   -- ^ 1 value

instance Binary PngUnit where
  get = do
    v <- getWord8
    pure $ case v of
      0 -> PngUnitUnknown
      1 -> PngUnitMeter
      _ -> PngUnitUnknown
  
  put v = case v of
    PngUnitUnknown -> putWord8 0
    PngUnitMeter -> putWord8 1

data PngPhysicalDimension = PngPhysicalDimension
    { pngDpiX     :: !Word32
    , pngDpiY     :: !Word32
    , pngUnit     :: !PngUnit
    }

instance Binary PngPhysicalDimension where
  get = PngPhysicalDimension <$> getWord32be <*> getWord32be <*> get
  put (PngPhysicalDimension dpx dpy unit) =
    putWord32be dpx >> putWord32be dpy >> put unit

newtype PngGamma = PngGamma { getPngGamma :: Double }

instance Binary PngGamma where
  get = PngGamma . (/ 100000) . fromIntegral <$> getWord32be
  put = putWord32be . ceiling . (100000 *) . getPngGamma 

data APngAnimationControl = APngAnimationControl
    { animationFrameCount :: !Word32
    , animationPlayCount  :: !Word32
    }
    deriving Show

-- | Encoded in a Word8
data APngFrameDisposal
      -- | No disposal is done on this frame before rendering the
      -- next; the contents of the output buffer are left as is. 
      -- Has Value 0
    = APngDisposeNone
      -- | The frame's region of the output buffer is to be cleared
      -- to fully transparent black before rendering the next frame. 
      -- Has Value 1
    | APngDisposeBackground
      -- | the frame's region of the output buffer is to be reverted
      -- to the previous contents before rendering the next frame.
      -- Has Value 2
    | APngDisposePrevious 
    deriving Show

-- | Encoded in a Word8
data APngBlendOp
      -- | Overwrite output buffer. has value '0'
    = APngBlendSource
      -- | Alpha blend to the output buffer. Has value '1'
    | APngBlendOver
    deriving Show

data APngFrameControl = APngFrameControl
    { frameSequenceNum      :: !Word32 -- ^ Starting from 0
    , frameWidth            :: !Word32 -- ^ Width of the following frame
    , frameHeight           :: !Word32 -- ^ Height of the following frame
    , frameLeft             :: !Word32 -- X position where to render the frame.
    , frameTop              :: !Word32 -- Y position where to render the frame.
    , frameDelayNumerator   :: !Word16
    , frameDelayDenuminator :: !Word16
    , frameDisposal         :: !APngFrameDisposal
    , frameBlending         :: !APngBlendOp
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

-- | Raw parsed image which need to be decoded.
data PngRawImage = PngRawImage
    { header       :: PngIHdr
    , chunks       :: [PngRawChunk]
    }

-- | Palette with indices beginning at 0 to elemcount - 1
type PngPalette = Palette' PixelRGB8

-- | Parse a palette from a png chunk.
parsePalette :: PngRawChunk -> Either String PngPalette
parsePalette plte
 | chunkLength plte `mod` 3 /= 0 = Left "Invalid palette size"
 | otherwise = Palette' pixelCount . V.fromListN (3 * pixelCount) <$> pixels
    where pixelUnpacker = replicateM (fromIntegral pixelCount * 3) get
          pixelCount = fromIntegral $ chunkLength plte `div` 3
          pixels = runGet pixelUnpacker (chunkData plte)

-- | Data structure during real png loading/parsing
data PngRawChunk = PngRawChunk
    { chunkLength :: Word32
    , chunkType   :: ChunkSignature
    , chunkCRC    :: Word32
    , chunkData   :: L.ByteString
    }

mkRawChunk :: ChunkSignature -> L.ByteString -> PngRawChunk
mkRawChunk sig binaryData = PngRawChunk
  { chunkLength = fromIntegral $ L.length binaryData
  , chunkType   = sig
  , chunkCRC    = pngComputeCrc [sig, binaryData]
  , chunkData   = binaryData
  }

-- | PNG chunk representing some extra information found in the parsed file.
data PngChunk = PngChunk
    { pngChunkData        :: L.ByteString  -- ^ The raw data inside the chunk
    , pngChunkSignature   :: ChunkSignature -- ^ The name of the chunk.
    }

-- | Low level access to PNG information
data PngLowLevel a = PngLowLevel
    { pngImage  :: Image a      -- ^ The real uncompressed image
    , pngChunks :: [PngChunk]   -- ^ List of raw chunk where some user data might be present.
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
    -- | Filt(x) = Orig(x) - Orig(a),     Recon(x) = Filt(x) + Recon(a)
    | FilterSub
    -- | Filt(x) = Orig(x) - Orig(b),     Recon(x) = Filt(x) + Recon(b)
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
    get = getWord8 >>= \w -> case w of
        0 -> return FilterNone
        1 -> return FilterSub
        2 -> return FilterUp
        3 -> return FilterAverage
        4 -> return FilterPaeth
        _ -> fail "Invalid scanline filter"

instance Binary PngRawImage where
    put img = do
        putLazyByteString pngSignature
        put $ header img
        mapM_ put $ chunks img

    get = parseRawPngImage

instance Binary PngRawChunk where
    put chunk = do
        putWord32be $ chunkLength chunk
        putLazyByteString $ chunkType chunk
        when (chunkLength chunk /= 0)
             (putLazyByteString $ chunkData chunk)
        putWord32be $ chunkCRC chunk

    get = do
        size <- getWord32be
        chunkSig <- getLazyByteString (fromIntegral $ L.length iHDRSignature)
        imgData <- if size == 0
            then return L.empty
            else getLazyByteString (fromIntegral size)
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

instance Binary PngIHdr where
    put hdr = do
        putWord32be 13
        let inner = runPut $ do
                putLazyByteString iHDRSignature
                putWord32be $ width hdr
                putWord32be $ height hdr
                putWord8    $ bitDepth hdr
                put $ colourType hdr
                put $ compressionMethod hdr
                put $ filterMethod hdr
                put $ interlaceMethod hdr
            crc = pngComputeCrc [inner]
        putLazyByteString inner
        putWord32be crc

    get = do
        _size <- getWord32be
        ihdrSig <- getLazyByteString (L.length iHDRSignature)
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


instance Binary PngInterlaceMethod where
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
    sig <- getLazyByteString (L.length pngSignature)
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
pngSignature = L.pack [137, 80, 78, 71, 13, 10, 26, 10]

-- | Helper function to help pack signatures.
signature :: String -> ChunkSignature
signature = LS.pack 

-- | Signature for the header chunk of png (must be the first)
iHDRSignature :: ChunkSignature 
iHDRSignature = signature "IHDR"

-- | Signature for a palette chunk in the pgn file. Must
-- occure before iDAT.
pLTESignature :: ChunkSignature
pLTESignature = signature "PLTE"

-- | Signature for a data chuck (with image parts in it)
iDATSignature :: ChunkSignature
iDATSignature = signature "IDAT"

-- | Signature for the last chunk of a png image, telling
-- the end.
iENDSignature :: ChunkSignature
iENDSignature = signature "IEND"

tRNSSignature :: ChunkSignature
tRNSSignature = signature "tRNS"

gammaSignature :: ChunkSignature
gammaSignature = signature "gAMA"

pHYsSignature :: ChunkSignature
pHYsSignature = signature "pHYs"

tEXtSignature :: ChunkSignature
tEXtSignature = signature "tEXt"

zTXtSignature :: ChunkSignature
zTXtSignature = signature "zTXt"

animationControlSignature :: ChunkSignature
animationControlSignature = signature "acTL"

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
pngCrcTable :: Vector Word32
pngCrcTable = fromListN 256 [ foldl' updateCrcConstant c [zero .. 7] | c <- [0 .. 255] ]
    where zero = 0 :: Int -- To avoid defaulting to Integer
          updateCrcConstant c _ | c .&. 1 /= 0 = magicConstant `xor` (c `unsafeShiftR` 1)
                                | otherwise = c `unsafeShiftR` 1
          magicConstant = 0xedb88320 :: Word32

-- | Compute the CRC of a raw buffer, as described in annex D of the PNG
-- specification.
pngComputeCrc :: [L.ByteString] -> Word32
pngComputeCrc = (0xFFFFFFFF `xor`) . L.foldl' updateCrc 0xFFFFFFFF . L.concat
    where updateCrc crc val =
              let u32Val = fromIntegral val
                  lutVal = pngCrcTable ! (fromIntegral ((crc `xor` u32Val) .&. 0xFF))
              in lutVal `xor` (crc `unsafeShiftR` 8)

chunksWithSig :: PngRawImage -> ChunkSignature -> [LS.ByteString]
chunksWithSig rawImg sig =
  [chunkData chunk | chunk <- chunks rawImg, chunkType chunk == sig]

