{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
-- | Module implementing function to read and write
-- Targa (*.tga) files.
module Codec.Picture.Tga( decodeTga
                        , decodeTgaWithMetadata
                        , decodeTgaWithPaletteAndMetadata
                        , TgaSaveable
                        , encodeTga
                        , writeTga
                        )  where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mempty )
import Control.Applicative( (<*>), pure, (<$>) )
#endif

import Control.Arrow( first )
import Control.Monad.ST( ST, runST )
import Data.Bits( (.&.)
                , (.|.)
                , bit
                , testBit
                , setBit
                , unsafeShiftL
                , unsafeShiftR )
import Data.Word( Word8, Word16 )
import Data.Binary( Binary( .. ), encode )
import Data.Binary.Get( Get
                      , getByteString 
                      , getWord8
                      , getWord16le
                      )
import Data.Binary.Put( putWord8
                      , putWord16le
                      , putByteString
                      )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lb
import qualified Data.ByteString.Unsafe as U
import qualified Data.Vector.Storable.Mutable as M

import Codec.Picture.Types
import Codec.Picture.InternalHelper
import Codec.Picture.Metadata( Metadatas
                             , SourceFormat( SourceTGA )
                             , basicMetadata )
import Codec.Picture.VectorByteConversion

data TgaColorMapType
  = ColorMapWithoutTable
  | ColorMapWithTable
  | ColorMapUnknown Word8

instance Binary TgaColorMapType where
  get = do
    v <- getWord8
    return $ case v of
      0 -> ColorMapWithoutTable
      1 -> ColorMapWithTable
      n -> ColorMapUnknown n

  put v = case v of
    ColorMapWithoutTable -> putWord8 0
    ColorMapWithTable -> putWord8 1
    (ColorMapUnknown vv) -> putWord8 vv

data TgaImageType
  = ImageTypeNoData Bool
  | ImageTypeColorMapped Bool
  | ImageTypeTrueColor Bool
  | ImageTypeMonochrome Bool

isRleEncoded :: TgaImageType -> Bool
isRleEncoded v = case v of
  ImageTypeNoData      yn -> yn
  ImageTypeColorMapped yn -> yn
  ImageTypeTrueColor   yn -> yn
  ImageTypeMonochrome  yn -> yn

imageTypeOfCode :: Monad m => Word8 -> m TgaImageType
imageTypeOfCode v = case v .&. 3 of
    0 -> return $ ImageTypeNoData isEncoded
    1 -> return $ ImageTypeColorMapped isEncoded
    2 -> return $ ImageTypeTrueColor isEncoded
    3 -> return $ ImageTypeMonochrome isEncoded
    _ -> fail $ "Unknown TGA image type " ++ show v
  where
    isEncoded = testBit v 3

codeOfImageType :: TgaImageType -> Word8
codeOfImageType v = case v of
    ImageTypeNoData encoded -> setVal 0 encoded
    ImageTypeColorMapped encoded -> setVal 1 encoded
    ImageTypeTrueColor encoded -> setVal 2 encoded
    ImageTypeMonochrome encoded -> setVal 3 encoded
    where
      setVal vv True = setBit vv 3
      setVal vv False = vv

instance Binary TgaImageType where
  get = getWord8 >>= imageTypeOfCode
  put = putWord8 . codeOfImageType
    
data TgaImageDescription = TgaImageDescription
  { _tgaIdXOrigin       :: Bool
  , _tgaIdYOrigin       :: Bool
  , _tgaIdAttributeBits :: Word8
  }

instance Binary TgaImageDescription where
  put desc = putWord8 $ xOrig .|. yOrig .|. attr
    where
      xOrig | _tgaIdXOrigin desc = bit 4
            | otherwise = 0

      yOrig | not $ _tgaIdYOrigin desc = bit 5
            | otherwise = 0
      
      attr = _tgaIdAttributeBits desc .&. 0xF

  get = toDescr <$> getWord8 where
    toDescr v = TgaImageDescription
      { _tgaIdXOrigin       = testBit v 4
      , _tgaIdYOrigin       = not $ testBit v 5
      , _tgaIdAttributeBits = v .&. 0xF
      }

data TgaHeader = TgaHeader
  { _tgaHdrIdLength         :: {-# UNPACK #-} !Word8
  , _tgaHdrColorMapType     :: !TgaColorMapType
  , _tgaHdrImageType        :: !TgaImageType
  , _tgaHdrMapStart         :: {-# UNPACK #-} !Word16
  , _tgaHdrMapLength        :: {-# UNPACK #-} !Word16
  , _tgaHdrMapDepth         :: {-# UNPACK #-} !Word8
  , _tgaHdrXOffset          :: {-# UNPACK #-} !Word16
  , _tgaHdrYOffset          :: {-# UNPACK #-} !Word16
  , _tgaHdrWidth            :: {-# UNPACK #-} !Word16
  , _tgaHdrHeight           :: {-# UNPACK #-} !Word16
  , _tgaHdrPixelDepth       :: {-# UNPACK #-} !Word8
  , _tgaHdrImageDescription :: {-# UNPACK #-} !TgaImageDescription
  }

instance Binary TgaHeader where
  get = TgaHeader
     <$> g8 <*> get <*> get <*> g16 <*> g16 <*> g8
     <*> g16 <*> g16 <*> g16 <*> g16 <*> g8 <*> get
   where g16 = getWord16le
         g8 = getWord8

  put v = do
    let p8 = putWord8
        p16 = putWord16le
    p8  $ _tgaHdrIdLength v
    put $ _tgaHdrColorMapType v
    put $ _tgaHdrImageType v

    p16 $ _tgaHdrMapStart v
    p16 $ _tgaHdrMapLength v
    p8  $ _tgaHdrMapDepth v
    p16 $ _tgaHdrXOffset v
    p16 $ _tgaHdrYOffset v
    p16 $ _tgaHdrWidth v
    p16 $ _tgaHdrHeight v
    p8  $ _tgaHdrPixelDepth v
    put $ _tgaHdrImageDescription v


data TgaFile = TgaFile
  { _tgaFileHeader :: !TgaHeader
  , _tgaFileId     :: !B.ByteString
  , _tgaPalette    :: !B.ByteString
  , _tgaFileRest   :: !B.ByteString
  }

getPalette :: TgaHeader -> Get B.ByteString
getPalette hdr | _tgaHdrMapLength hdr <= 0 = return mempty
getPalette hdr = getByteString $ bytePerPixel * pixelCount
  where
    bytePerPixel = fromIntegral $ _tgaHdrMapDepth hdr `div` 8
    pixelCount = fromIntegral $ _tgaHdrMapLength hdr

instance Binary TgaFile where
  get = do
    hdr <- get
    validateTga hdr
    fileId <- getByteString . fromIntegral $ _tgaHdrIdLength hdr
    palette <- getPalette hdr
    rest <- getRemainingBytes

    return TgaFile {
        _tgaFileHeader = hdr
      , _tgaFileId = fileId
      , _tgaPalette = palette
      , _tgaFileRest = rest
      }

  put file = do
    put $ _tgaFileHeader file
    putByteString $ _tgaFileId file
    putByteString $ _tgaPalette file
    putByteString $ _tgaFileRest file

data Depth8 = Depth8
data Depth15 = Depth15
data Depth24 = Depth24
data Depth32 = Depth32

class (Pixel (Unpacked a)) => TGAPixel a where
   type Unpacked a
   packedByteSize :: a -> Int
   tgaUnpack      :: a -> B.ByteString -> Int -> Unpacked a

instance TGAPixel Depth8 where
   type Unpacked Depth8 = Pixel8
   packedByteSize _ = 1
   tgaUnpack _ = U.unsafeIndex

instance TGAPixel Depth15 where
   type Unpacked Depth15 = PixelRGBA8
   packedByteSize _ = 2
   tgaUnpack _ str ix = PixelRGBA8 r g b a
      where
        v0 = U.unsafeIndex str ix
        v1 = U.unsafeIndex str $ ix + 1
        r = (v1 .&. 0x7c) `unsafeShiftL` 1;
        g = ((v1 .&. 0x03) `unsafeShiftL` 6) .|. ((v0 .&. 0xe0) `unsafeShiftR` 2);
        b = (v0 .&. 0x1f) `unsafeShiftL` 3
        a = 255 -- v1 .&. 0x80

instance TGAPixel Depth24 where
   type Unpacked Depth24 = PixelRGB8
   packedByteSize _ = 3
   tgaUnpack _ str ix = PixelRGB8 r g b
     where
       b = U.unsafeIndex str ix
       g = U.unsafeIndex str (ix + 1)
       r = U.unsafeIndex str (ix + 2)

instance TGAPixel Depth32 where
   type Unpacked Depth32 = PixelRGBA8
   packedByteSize _ = 4
   tgaUnpack _ str ix = PixelRGBA8 r g b a
     where
       b = U.unsafeIndex str ix
       g = U.unsafeIndex str (ix + 1)
       r = U.unsafeIndex str (ix + 2)
       a = U.unsafeIndex str (ix + 3)

prepareUnpacker :: TgaFile
                -> (forall tgapx. (TGAPixel tgapx) => tgapx -> TgaFile -> Image (Unpacked tgapx))
                -> Either String DynamicImage
prepareUnpacker file f =
  let hdr = _tgaFileHeader file
      flipper :: (Pixel px) => Image px -> Image px
      flipper = flipImage $ _tgaHdrImageDescription hdr
  in
  case _tgaHdrPixelDepth hdr of
    8  -> pure . ImageY8 . flipper $ f Depth8 file
    16 -> pure . ImageRGBA8 . flipper $ f Depth15 file
    24 -> pure . ImageRGB8 . flipper $ f Depth24 file
    32 -> pure . ImageRGBA8 . flipper $ f Depth32 file
    n  -> fail $ "Invalid bit depth (" ++ show n ++ ")"

toPaletted :: (Pixel px)
           => (Image Pixel8 -> Palette' px -> PalettedImage) -> Image px
           -> DynamicImage
           -> Either String PalettedImage
toPaletted f palette (ImageY8 img) = pure $ f img pal where
  pal = Palette' 
    { _paletteSize = imageWidth palette
    , _paletteData = imageData palette
    }
toPaletted _ _ _ = fail "Bad colorspace for image"

unparse :: TgaFile -> Either String (PalettedImage, Metadatas)
unparse file =
  let hdr = _tgaFileHeader file
      imageType = _tgaHdrImageType hdr

      unpacker :: forall tgapx. (TGAPixel tgapx)
               => tgapx -> TgaFile -> Image (Unpacked tgapx)
      unpacker | isRleEncoded imageType = unpackRLETga
               | otherwise = unpackUncompressedTga

      metas = basicMetadata SourceTGA (_tgaHdrWidth hdr) (_tgaHdrHeight hdr)
      decodedPalette = unparse file
        { _tgaFileHeader = hdr
            { _tgaHdrHeight = 1
            , _tgaHdrWidth = _tgaHdrMapLength hdr
            , _tgaHdrPixelDepth = _tgaHdrMapDepth hdr
            , _tgaHdrImageType = ImageTypeTrueColor False
            }
        , _tgaFileRest = _tgaPalette file
        }
  in
  case imageType of
    ImageTypeNoData _ -> fail "No data detected in TGA file"
    ImageTypeTrueColor _ ->
      fmap ((, metas) . TrueColorImage) $ prepareUnpacker file unpacker
    ImageTypeMonochrome _ ->
      fmap ((, metas) . TrueColorImage) $ prepareUnpacker file unpacker
    ImageTypeColorMapped _ ->
      case decodedPalette of
        Left str -> Left str
        Right (TrueColorImage (ImageY8 img), _) ->
          fmap (, metas) $ prepareUnpacker file unpacker >>= toPaletted PalettedY8 img
        Right (TrueColorImage (ImageRGB8 img), _) ->
          fmap (, metas) $ prepareUnpacker file unpacker >>= toPaletted PalettedRGB8 img
        Right (TrueColorImage (ImageRGBA8 img), _) ->
          fmap (, metas) $ prepareUnpacker file unpacker >>= toPaletted PalettedRGBA8 img
        Right _ -> fail "Unknown pixel type"

writeRun :: (Pixel px)
         => M.STVector s (PixelBaseComponent px) -> Int -> px -> Int
         -> ST s Int
writeRun imgData localMaxi px = run
  where
    writeDelta = componentCount px
    run writeIndex 
      | writeIndex >= localMaxi = return writeIndex
    run writeIndex = do
      unsafeWritePixel imgData writeIndex px
      run $ writeIndex + writeDelta

copyData :: forall tgapx s
          . (TGAPixel tgapx)
         => tgapx
         -> M.STVector s (PixelBaseComponent (Unpacked tgapx))
         -> B.ByteString
         -> Int -> Int
         -> Int -> Int
         -> ST s (Int, Int)
copyData tgapx imgData readData maxi maxRead = go
  where
    readDelta = packedByteSize tgapx
    writeDelta = componentCount (undefined :: Unpacked tgapx)

    go writeIndex readIndex
      | writeIndex >= maxi ||
        readIndex >= maxRead = return (writeIndex, readIndex)
    go writeIndex readIndex = do
      let px = tgaUnpack tgapx readData readIndex :: Unpacked tgapx
      unsafeWritePixel imgData writeIndex px
      go (writeIndex + writeDelta) (readIndex + readDelta)

unpackUncompressedTga :: forall tgapx
                       . (TGAPixel tgapx)
                      => tgapx -- ^ Type witness
                      -> TgaFile
                      -> Image (Unpacked tgapx)
unpackUncompressedTga tga file = runST $ do
    img <- MutableImage width height <$> M.new maxi
    let imgData = mutableImageData img
    _ <- copyData tga imgData readData maxi maxRead 0 0
    unsafeFreezeImage img

  where
    hdr = _tgaFileHeader file
    width = fromIntegral $ _tgaHdrWidth hdr
    height = fromIntegral $ _tgaHdrHeight hdr
    readData = _tgaFileRest file
    compCount = componentCount (undefined :: Unpacked tgapx)
    maxi = width * height * compCount
    maxRead = B.length readData

isRleChunk :: Word8 -> Bool
isRleChunk v = testBit v 7

runLength :: Word8 -> Int
runLength v = fromIntegral (v .&. 0x7F) + 1

unpackRLETga :: forall tgapx
              . (TGAPixel tgapx)
             => tgapx -- ^ Type witness
             -> TgaFile
             -> Image (Unpacked tgapx)
unpackRLETga tga file = runST $ do
    img <- MutableImage width height <$> M.new maxi
    let imgData = mutableImageData img

        go writeIndex readIndex
            | writeIndex >= maxi = return () 
            | readIndex >= maxRead = return ()
        go writeIndex readIndex = do
          let code = U.unsafeIndex readData readIndex
              copyMax = min maxi $ writeIndex + runLength code * compCount
          
          if isRleChunk code then do
            let px = tgaUnpack tga readData (readIndex + 1) :: Unpacked tgapx
            lastWriteIndex <- writeRun imgData copyMax px writeIndex
            go lastWriteIndex $ readIndex + 1 + readDelta

          else do
            (newWrite, newRead) <-
                copyData tga imgData readData copyMax maxRead
                    writeIndex (readIndex + 1)
            go newWrite newRead

    go 0 0
    unsafeFreezeImage img

  where
    hdr = _tgaFileHeader file
    width = fromIntegral $ _tgaHdrWidth hdr
    height = fromIntegral $ _tgaHdrHeight hdr
    readData = _tgaFileRest file
    compCount = componentCount (undefined :: Unpacked tgapx)
    maxi = width * height * compCount
    maxRead = B.length readData
    readDelta = packedByteSize tga

flipImage :: (Pixel px)
          => TgaImageDescription -> Image px -> Image px
flipImage desc img
    | xFlip && yFlip =
        generateImage (\x y -> pixelAt img (wMax - x) (hMax - y)) w h
    | xFlip =
        generateImage (\x y -> pixelAt img (wMax - x) y) w h
    | yFlip =
        generateImage (\x y -> pixelAt img x (hMax - y)) w h
    | otherwise = img
  where
    xFlip = _tgaIdXOrigin desc
    yFlip = _tgaIdYOrigin desc
    w = imageWidth img
    h = imageHeight img

    !wMax = w - 1
    !hMax = h - 1

validateTga :: (Monad m) => TgaHeader -> m ()
validateTga hdr
    | _tgaHdrWidth hdr <= 0 = fail "Width is null or negative"
    | _tgaHdrHeight hdr <= 0 = fail "Height is null or negative"
validateTga _ = return ()

-- | Transform a raw tga image to an image, without modifying
-- the underlying pixel type.
--
-- This function can output the following images:
--
--  * 'ImageY8'
--
--  * 'ImageRGB8'
--
--  * 'ImageRGBA8'
--
decodeTga :: B.ByteString -> Either String DynamicImage
decodeTga byte = fst <$> decodeTgaWithMetadata byte

-- | Equivalent to decodeTga but also provide metadata
decodeTgaWithMetadata :: B.ByteString -> Either String (DynamicImage, Metadatas)
decodeTgaWithMetadata byte = first palettedToTrueColor <$> decodeTgaWithPaletteAndMetadata byte

-- | Equivalent to decodeTga but with metdata and palette if any
decodeTgaWithPaletteAndMetadata :: B.ByteString -> Either String (PalettedImage, Metadatas)
decodeTgaWithPaletteAndMetadata byte = runGetStrict get byte >>= unparse

-- | This typeclass determine if a pixel can be saved in the
-- TGA format.
class TgaSaveable a where
    tgaDataOfImage :: Image a -> B.ByteString
    tgaPixelDepthOfImage :: Image a -> Word8
    tgaTypeOfImage :: Image a -> TgaImageType

instance TgaSaveable Pixel8 where
    tgaDataOfImage = toByteString . imageData
    tgaPixelDepthOfImage _ = 8
    tgaTypeOfImage _ = ImageTypeMonochrome False

instance TgaSaveable PixelRGB8 where
    tgaPixelDepthOfImage _ = 24
    tgaTypeOfImage _ = ImageTypeTrueColor False
    tgaDataOfImage = toByteString . imageData . pixelMap flipRgb
      where
        flipRgb (PixelRGB8 r g b) = PixelRGB8 b g r

instance TgaSaveable PixelRGBA8 where
    tgaPixelDepthOfImage _ = 32
    tgaTypeOfImage _ = ImageTypeTrueColor False
    tgaDataOfImage = toByteString . imageData . pixelMap flipRgba
      where
        flipRgba (PixelRGBA8 r g b a) = PixelRGBA8 b g r a

-- | Helper function to directly write an image a tga on disk.
writeTga :: (TgaSaveable pixel) => FilePath -> Image pixel -> IO ()
writeTga path img = Lb.writeFile path $ encodeTga img

-- | Transform a compatible image to a raw bytestring
-- representing a Targa file.
encodeTga :: (TgaSaveable px) => Image px -> Lb.ByteString
encodeTga img = encode file
  where
    file = TgaFile
      { _tgaFileHeader = TgaHeader
            { _tgaHdrIdLength         = 0
            , _tgaHdrColorMapType     = ColorMapWithoutTable
            , _tgaHdrImageType        = tgaTypeOfImage img
            , _tgaHdrMapStart         = 0
            , _tgaHdrMapLength        = 0
            , _tgaHdrMapDepth         = 0
            , _tgaHdrXOffset          = 0
            , _tgaHdrYOffset          = 0
            , _tgaHdrWidth            = fromIntegral $ imageWidth img
            , _tgaHdrHeight           = fromIntegral $ imageHeight img
            , _tgaHdrPixelDepth       = tgaPixelDepthOfImage img
            , _tgaHdrImageDescription = TgaImageDescription
                    { _tgaIdXOrigin       = False
                    , _tgaIdYOrigin       = False
                    , _tgaIdAttributeBits = 0
                    }
            }
      , _tgaFileId     = mempty
      , _tgaPalette    = mempty
      , _tgaFileRest   = tgaDataOfImage img
      }

{-# ANN module "HLint: ignore Reduce duplication" #-}

