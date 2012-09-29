-- | Module implementing GIF decoding.
module Codec.Picture.Gif ( decodeGif
                         , decodeGifImages
                         ) where

import Control.Applicative( pure, (<$>), (<*>) )
import Control.Monad( replicateM )
import Control.Monad.ST( runST )
import Control.Monad.Trans.Class( lift )

import Data.Bits( (.&.), shiftR, testBit )
import Data.Word( Word8, Word16 )

import qualified Data.ByteString as B
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

import Data.Serialize( Serialize(..)
                     , Get
                     , decode
                     , getWord8
                     , getWord16le
                     , getBytes
                     , lookAhead
                     {-, decode-}
                     {-, remaining-}

                     {-, Put-}
                     {-, putWord8-}
                     {-, putWord16be-}
                     {-, encode-}
                     {-, putByteString -}
                     )

import Codec.Picture.Types
import Codec.Picture.Gif.LZW
import Codec.Picture.BitWriter

{-
   <GIF Data Stream> ::=     Header <Logical Screen> <Data>* Trailer

   <Logical Screen> ::=      Logical Screen Descriptor [Global Color Table]

   <Data> ::=                <Graphic Block>  |
                             <Special-Purpose Block>

   <Graphic Block> ::=       [Graphic Control Extension] <Graphic-Rendering Block>

   <Graphic-Rendering Block> ::=  <Table-Based Image>  |
                                  Plain Text Extension

   <Table-Based Image> ::=   Image Descriptor [Local Color Table] Image Data

   <Special-Purpose Block> ::=    Application Extension  |
                                  Comment Extension
 -}

--------------------------------------------------
----            GifVersion
--------------------------------------------------
data GifVersion = GIF87a | GIF89a

gif87aSignature, gif89aSignature :: B.ByteString
gif87aSignature = B.pack $ map (fromIntegral . fromEnum) "GIF87a"
gif89aSignature = B.pack $ map (fromIntegral . fromEnum) "GIF89a"

instance Serialize GifVersion where
    put GIF87a = put gif87aSignature
    put GIF89a = put gif89aSignature

    get = do
        sig <- getBytes (B.length gif87aSignature)
        case (sig == gif87aSignature, sig == gif89aSignature) of
            (True, _)  -> pure GIF87a
            (_ , True) -> pure GIF89a
            _          -> fail "Invalid Gif signature"


--------------------------------------------------
----         LogicalScreenDescriptor
--------------------------------------------------
-- | Section 18 of spec-gif89a
data LogicalScreenDescriptor = LogicalScreenDescriptor
  { -- | Stored on 16 bits
    screenWidth           :: !Word16
    -- | Stored on 16 bits
  , screenHeight          :: !Word16
    -- | Stored on 8 bits
  , backgroundIndex       :: !Word8

  -- | Stored on 1 bit
  , hasGlobalMap          :: !Bool
  -- | Stored on 3 bits
  , colorResolution       :: !Word8
  -- | Stored on 1 bit
  , isColorTableSorted    :: !Bool
  -- | Stored on 3 bits
  , colorTableSize        :: !Word8
  }

instance Serialize LogicalScreenDescriptor where
    put _ = undefined
    get = do
        w <- getWord16le
        h <- getWord16le
        packedField  <- getWord8
        backgroundColorIndex  <- getWord8
        _aspectRatio  <- getWord8
        return LogicalScreenDescriptor
            { screenWidth           = w
            , screenHeight          = h
            , hasGlobalMap          = packedField `testBit` 7
            , colorResolution       = (packedField `shiftR` 5) .&. 0x7 + 1
            , isColorTableSorted    = packedField `testBit` 3
            , colorTableSize        = (packedField .&. 0x7) + 1
            , backgroundIndex       = backgroundColorIndex
            }


--------------------------------------------------
----            ImageDescriptor
--------------------------------------------------
-- | Section 20 of spec-gif89a
data ImageDescriptor = ImageDescriptor
  { gDescPixelsFromLeft         :: !Word16
  , gDescPixelsFromTop          :: !Word16
  , gDescImageWidth             :: !Word16
  , gDescImageHeight            :: !Word16
  , gDescHasLocalMap            :: !Bool
  , gDescIsInterlaced           :: !Bool
  , gDescIsImgDescriptorSorted  :: !Bool
  , gDescLocalColorTableSize    :: !Word8
  }

imageSeparator, extensionIntroducer, gifTrailer :: Word8
imageSeparator      = 0x2C
extensionIntroducer = 0x21
gifTrailer          = 0x3B

graphicControlLabel :: Word8
graphicControlLabel = 0xF9

--commentLabel, graphicControlLabel, applicationLabel
--
{-commentLabel        = 0xFE-}
{-plainTextLabel      = 0x01-}
{-applicationLabel    = 0xFF-}

parseDataBlocks :: Get B.ByteString
parseDataBlocks = B.concat <$> (getWord8 >>= aux)
 where aux    0 = pure []
       aux size = (:) <$> getBytes (fromIntegral size) <*> (getWord8 >>= aux)

data GraphicControlExtension = GraphicControlExtension
    { gceDisposalMethod        :: !Word8 -- ^ Stored on 3 bits
    , gceUserInputFlag         :: !Bool
    , gceTransparentFlag       :: !Bool
    , gceDelay                 :: !Word16
    , gceTransparentColorIndex :: !Word8
    }

instance Serialize GraphicControlExtension where
    put _ = undefined
    get = do
        _extensionLabel  <- getWord8
        _size            <- getWord8
        packedFields     <- getWord8
        delay            <- getWord16le
        idx              <- getWord8
        _blockTerminator <- getWord8
        return GraphicControlExtension
            { gceDisposalMethod        = (packedFields `shiftR` 2) .&. 0x07
            , gceUserInputFlag         = packedFields `testBit` 1
            , gceTransparentFlag       = packedFields `testBit` 0
            , gceDelay                 = delay
            , gceTransparentColorIndex = idx
            }

data GifImage = GifImage
    { imgDescriptor   :: !ImageDescriptor
    , imgLocalPalette :: !(Maybe Palette)
    , imgLzwRootSize  :: !Word8
    , imgData         :: B.ByteString
    }

instance Serialize GifImage where
    put _ = undefined
    get = do
        desc <- get
        let paletteSize = gDescLocalColorTableSize desc
        palette <- if paletteSize > 0
           then Just <$> getPalette paletteSize
           else pure Nothing

        GifImage desc palette <$> getWord8 <*> parseDataBlocks

data Block = BlockImage GifImage
           | BlockGraphicControl GraphicControlExtension

parseGifBlocks :: Get [Block]
parseGifBlocks = lookAhead getWord8 >>= blockParse
  where blockParse v
          | v == gifTrailer = getWord8 >> pure []
          | v == imageSeparator = (:) <$> (BlockImage <$> get) <*> parseGifBlocks
          | v == extensionIntroducer = do
                _ <- getWord8
                extensionCode <- lookAhead getWord8
                if extensionCode /= graphicControlLabel
                   then getWord8 >> parseDataBlocks >> parseGifBlocks
                   else (:) <$> (BlockGraphicControl <$> get) <*> parseGifBlocks

        blockParse v = fail ("Unrecognize gif block " ++ show v)

instance Serialize ImageDescriptor where
    put _ = undefined
    get = do
        _imageSeparator <- getWord8
        imgLeftPos <- getWord16le
        imgTopPos  <- getWord16le
        imgWidth   <- getWord16le
        imgHeight  <- getWord16le
        packedFields <- getWord8
        let tableSize = packedFields .&. 0x7
        return ImageDescriptor
            { gDescPixelsFromLeft = imgLeftPos
            , gDescPixelsFromTop  = imgTopPos
            , gDescImageWidth     = imgWidth
            , gDescImageHeight    = imgHeight
            , gDescHasLocalMap    = packedFields `testBit` 7
            , gDescIsInterlaced     = packedFields `testBit` 6
            , gDescIsImgDescriptorSorted = packedFields `testBit` 5
            , gDescLocalColorTableSize = if tableSize > 0 then tableSize + 1 else 0
            }


--------------------------------------------------
----            Palette
--------------------------------------------------
type Palette = V.Vector PixelRGB8

getPalette :: Word8 -> Get Palette
getPalette bitDepth = replicateM size get >>= return . V.fromList
  where size = 2 ^ (fromIntegral bitDepth :: Int)

--------------------------------------------------
----            GifImage
--------------------------------------------------
data GifHeader = GifHeader
  { gifVersion          :: GifVersion
  , gifScreenDescriptor :: LogicalScreenDescriptor
  , gifGlobalMap        :: !Palette
  }

instance Serialize GifHeader where
    put _ = undefined
    get = do
        version    <- get
        screenDesc <- get
        palette    <- getPalette $ colorTableSize screenDesc
        return GifHeader
            { gifVersion = version
            , gifScreenDescriptor = screenDesc
            , gifGlobalMap = palette
            }

data GifFile = GifFile
    { gifHeader  :: !GifHeader
    , gifImages  :: [(Maybe GraphicControlExtension, GifImage)]
    }

associateDescr :: [Block] -> [(Maybe GraphicControlExtension, GifImage)]
associateDescr [] = []
associateDescr [BlockGraphicControl _] = []
associateDescr (BlockGraphicControl _ : rest@(BlockGraphicControl _ : _)) = associateDescr rest
associateDescr (BlockImage img:xs) = (Nothing, img) : associateDescr xs
associateDescr (BlockGraphicControl ctrl : BlockImage img : xs) =
    (Just ctrl, img) : associateDescr xs

instance Serialize GifFile where
    put _ = undefined
    get = do
        hdr <- get
        blocks <- parseGifBlocks
        return GifFile { gifHeader = hdr
                       , gifImages = associateDescr blocks }

substituteColors :: Palette -> Image Pixel8 -> Image PixelRGB8
substituteColors palette = pixelMap swaper
  where swaper n = palette V.! (fromIntegral n)

decodeImage :: GifImage -> Image Pixel8
decodeImage img = runST $ runBoolReader $ do
    outputVector <- lift . M.new $ width * height
    decodeLzw (imgData img) 12 lzwRoot outputVector
    frozenData <- lift $ V.unsafeFreeze outputVector
    return $ Image
      { imageWidth = width
      , imageHeight = height
      , imageData = frozenData
      }
  where lzwRoot = fromIntegral $ imgLzwRootSize img
        width = fromIntegral $ gDescImageWidth descriptor
        height = fromIntegral $ gDescImageHeight descriptor
        descriptor = imgDescriptor img

paletteOf :: Palette -> GifImage -> Palette
paletteOf global GifImage { imgLocalPalette = Nothing } = global
paletteOf      _ GifImage { imgLocalPalette = Just p  } = p

decodeAllGifImages :: GifFile -> [Image PixelRGB8]
decodeAllGifImages GifFile { gifImages = [] } = []
decodeAllGifImages GifFile { gifHeader = GifHeader { gifGlobalMap = palette
                                                   , gifScreenDescriptor = wholeDescriptor
                                                   }
                           , gifImages = (_, firstImage) : rest } = map paletteApplyer $
 scanl generator (paletteOf palette firstImage, decodeImage firstImage) rest
    where globalWidth = fromIntegral $ screenWidth wholeDescriptor
          globalHeight = fromIntegral $ screenHeight wholeDescriptor

          {-background = backgroundIndex wholeDescriptor-}

          paletteApplyer (pal, img) = substituteColors pal img

          generator (_, img1) (controlExt, img2@(GifImage { imgDescriptor = descriptor })) =
                        (paletteOf palette img2, generateImage pixeler globalWidth globalHeight)
               where localWidth = fromIntegral $ gDescImageWidth descriptor
                     localHeight = fromIntegral $ gDescImageHeight descriptor

                     left = fromIntegral $ gDescPixelsFromLeft descriptor
                     top = fromIntegral $ gDescPixelsFromTop descriptor

                     isPixelInLocalImage x y =
                         x >= left && x < left + localWidth && y >= top && y < top + localHeight

                     decoded = decodeImage img2

                     transparent :: Int
                     transparent = case controlExt of
                        Nothing  -> 300
                        Just ext -> if gceTransparentFlag ext
                            then fromIntegral $ gceTransparentColorIndex ext
                            else 300

                     pixeler x y
                        | isPixelInLocalImage x y && fromIntegral val /= transparent = val
                            where val = pixelAt decoded (x - left) (y - top)
                     pixeler x y = pixelAt img1 x y

decodeFirstGifImage :: GifFile -> Either String (Image PixelRGB8)
decodeFirstGifImage
        GifFile { gifHeader = GifHeader { gifGlobalMap = palette}
                , gifImages = ((_, gif):_) } = Right . substituteColors palette $ decodeImage gif
decodeFirstGifImage _ = Left "No image in gif file"

-- | Transform a raw gif image to an image, witout
-- modifying the pixels.
-- This function can output the following pixel types :
--
--  * PixelRGB8
--
decodeGif :: B.ByteString -> Either String DynamicImage
decodeGif img = ImageRGB8 <$> (decode img >>= decodeFirstGifImage)

-- | Transform a raw gif to a list of images, representing
-- all the images of an animation.
decodeGifImages :: B.ByteString -> Either String [Image PixelRGB8]
decodeGifImages img = decodeAllGifImages <$> decode img

