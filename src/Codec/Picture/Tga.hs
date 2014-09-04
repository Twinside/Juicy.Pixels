{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Codec.Picture.Tga where

import Control.Monad.ST( runST )
import Control.Applicative( (<$>), (<*>) )
import Data.Bits( (.&.), testBit, setBit )
import Data.Monoid( mempty )
import Data.Word( Word8, Word16 )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( Get
                      , getByteString 
                      , getWord8
                      , getWord16le
                      , getRemainingLazyByteString
                      )
import Data.Binary.Put( putWord8
                      , putWord16le
                      , putByteString
                      )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as U
import qualified Data.Vector.Storable.Mutable as M

import Codec.Picture.Types

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

imageTypeOfCode :: Monad m => Word8 -> m TgaImageType
imageTypeOfCode v = case v .&. 3 of
    0 -> return $ ImageTypeNoData isEncoded
    1 -> return $ ImageTypeColorMapped isEncoded
    2 -> return $ ImageTypeTrueColor isEncoded
    3 -> return $ ImageTypeMonochrome isEncoded
    _ -> fail $ "Unknown TGA image type " ++ show v
  where
    isEncoded = testBit v 2

codeOfImageType :: TgaImageType -> Word8
codeOfImageType v = case v of
    ImageTypeNoData encoded -> setVal 0 encoded
    ImageTypeColorMapped encoded -> setVal 1 encoded
    ImageTypeTrueColor encoded -> setVal 2 encoded
    ImageTypeMonochrome encoded -> setVal 3 encoded
    where
      setVal vv True = setBit vv 2
      setVal vv False = vv

instance Binary TgaImageType where
  get = getWord8 >>= imageTypeOfCode
  put = putWord8 . codeOfImageType
    
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
  , _tgaHdrImageDescription :: {-# UNPACK #-} !Word8
  }

instance Binary TgaHeader where
  get = TgaHeader
     <$> g8 <*> get <*> get <*> g16 <*> g16 <*> g8
     <*> g16 <*> g16 <*> g16 <*> g16 <*> g8 <*> g8
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
    p8  $ _tgaHdrImageDescription v


data TgaFile = TgaFile
  { _tgaFileHeader :: !TgaHeader
  , _tgaFileId     :: !B.ByteString
  , _tgaPalette    :: !B.ByteString
  , _tgaFileRest   :: !B.ByteString
  }

getPalette :: TgaHeader -> Get B.ByteString
getPalette _ = return mempty

toStrict :: L.ByteString -> B.ByteString
toStrict = B.concat . L.toChunks

instance Binary TgaFile where
  get = do
    hdr <- get
    fileId <- getByteString . fromIntegral $ _tgaHdrIdLength hdr
    palette <- getPalette hdr
    rest <- getRemainingLazyByteString

    return TgaFile {
        _tgaFileHeader = hdr
      , _tgaFileId = fileId
      , _tgaPalette = palette
      , _tgaFileRest = toStrict rest
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
   tgaUnpack      :: a -> B.ByteString -> Int -> (Unpacked a)

instance TGAPixel Depth8 where
   type Unpacked Depth8 = Pixel8
   packedByteSize _ = 1
   tgaUnpack _ = U.unsafeIndex

unpackUncompressedTga :: forall tgapx
                       . (TGAPixel tgapx)
                      => tgapx -- ^ Type witness
                      -> TgaFile
                      -> Image (Unpacked tgapx)
unpackUncompressedTga tga file = runST $ do
    img <- MutableImage width height <$> M.new maxi
    let imgData = mutableImageData img

        go writeIndex readIndex
            | writeIndex >= maxi = return () 
            | readIndex >= maxRead = return ()
        go writeIndex readIndex = do
          let px = tgaUnpack tga readData readIndex :: Unpacked tgapx
          unsafeWritePixel imgData writeIndex px
          go (writeIndex + writeDelta) (readIndex + readDelta)

    go 0 0
    unsafeFreezeImage img

  where
    hdr = _tgaFileHeader file
    width = fromIntegral $ _tgaHdrWidth hdr
    height = fromIntegral $ _tgaHdrHeight hdr

    readData = _tgaFileRest file

    maxi = width * height * writeDelta
    maxRead = B.length readData

    readDelta = packedByteSize tga
    writeDelta = componentCount (undefined :: Unpacked tgapx)

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
          let code = B.unsafeIndex readIndex
          let px = tgaUnpack tga readData readIndex :: Unpacked tgapx
          unsafeWritePixel imgData writeIndex px
          go (writeIndex + writeDelta) (readIndex + readDelta)

    go 0 0
    unsafeFreezeImage img

  where
    hdr = _tgaFileHeader file
    width = fromIntegral $ _tgaHdrWidth hdr
    height = fromIntegral $ _tgaHdrHeight hdr

    readData = _tgaFileRest file

    maxi = width * height * writeDelta
    maxRead = B.length readData

    readDelta = packedByteSize tga
    writeDelta = componentCount (undefined :: Unpacked tgapx)

