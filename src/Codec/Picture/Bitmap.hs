{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | Modules used for Bitmap file (.bmp) file loading and writing
module Codec.Picture.Bitmap( -- * Functions
                             writeBitmap
                           , encodeBitmap
                           , encodeBitmapWithMetadata
                           , decodeBitmap
                           , decodeBitmapWithMetadata
                           , decodeBitmapWithPaletteAndMetadata
                           , encodeDynamicBitmap 
                           , encodeBitmapWithPaletteAndMetadata
                           , writeDynamicBitmap 
                             -- * Accepted format in output
                           , BmpEncodable( )
                           ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mempty )
import Control.Applicative( (<$>) )
#endif

import Control.Arrow( first )
import Control.Monad( replicateM, when, foldM_, forM_, void )
import Control.Monad.ST ( ST, runST )
import Data.Maybe( fromMaybe )
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as M
import Data.Binary( Binary( .. ) )
import Data.Binary.Put( Put
                      , runPut
                      , putInt32le
                      , putWord16le
                      , putWord32le
                      , putByteString 
                      )

import Data.Binary.Get( Get
                      , getWord8
                      , getWord16le 
                      , getWord32le
                      , getInt32le
                      , getByteString
                      , bytesRead
                      , skip
                      , label
                      )

import Data.Bits
import Data.Int( Int32 )
import Data.Word( Word32, Word16, Word8 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Codec.Picture.InternalHelper
import Codec.Picture.Types
import Codec.Picture.VectorByteConversion
import qualified Codec.Picture.Metadata as Met
import Codec.Picture.Metadata ( Metadatas )

data BmpHeader = BmpHeader
    { magicIdentifier :: !Word16
    , fileSize        :: !Word32 -- ^ in bytes
    , reserved1       :: !Word16
    , reserved2       :: !Word16
    , dataOffset      :: !Word32
    }

bitmapMagicIdentifier :: Word16
bitmapMagicIdentifier = 0x4D42

instance Binary BmpHeader where
    put hdr = do
        putWord16le $ magicIdentifier hdr
        putWord32le $ fileSize hdr
        putWord16le $ reserved1 hdr
        putWord16le $ reserved2 hdr
        putWord32le $ dataOffset hdr

    get = do
        ident <- getWord16le
        when (ident /= bitmapMagicIdentifier)
             (fail "Invalid Bitmap magic identifier")
        fsize <- getWord32le
        r1 <- getWord16le
        r2 <- getWord16le
        offset <- getWord32le
        return BmpHeader
            { magicIdentifier = ident
            , fileSize = fsize
            , reserved1 = r1
            , reserved2 = r2
            , dataOffset = offset
            }

-- | The type of color space declared in a Windows BMP file.
data ColorSpaceType = CalibratedRGB
                    | DeviceDependentRGB
                    | DeviceDependentCMYK
                    | SRGB
                    | WindowsColorSpace
                    | ProfileEmbedded
                    | ProfileLinked
                    | UnknownColorSpace Word32
                    deriving (Eq, Show)

-- | BMPINFOHEADER with compatibility up to V5. This header was first introduced
-- with Windows 3.1, and was later extended in Windows 95 and Windows 98. The
-- original BMPINFOHEADER includes all fields up to 'importantColors'.
--
-- Some Windows 3.1 bitmaps with 16, 24 or 32 bits per pixel might also have
-- three bitmasks following the BITMAPINFOHEADER. These bitmasks were later
-- incorporated into the bitmap header structure in the unreleased
-- BITMAPV2INFOHEADER. The (also unreleased) BITMAPV3INFOHEADER added another
-- bitmask for an alpha channel.
--
-- The later Windows 95 and Windows 98 extensions to the BITMAPINFOHEADER extend
-- the BITMAPV3INFOHEADER, adding support for color correction.
--
--  * BITMAPV4HEADER (Windows 95) may include a simple color profile in a
--      proprietary format. The fields in this color profile (which includes gamma
--      values) are not to be used unless the 'colorSpaceType' field is
--      'CalibratedRGB'.
--
--  * BITMAPV5HEADER (Windows 98) adds support for an ICC color profile. The
--      presence of an ICC color profile is indicated by setting the 'colorSpaceType'
--      field to 'ProfileEmbedded' or 'ProfileLinked'. If it is 'ProfileLinked' then
--      the profile data is actually a Windows-1252 encoded string containing the
--      fully qualified path to an ICC color profile.
data BmpV5Header = BmpV5Header
    { size              :: !Word32 -- Header size in bytes
    , width             :: !Int32
    , height            :: !Int32
    , planes            :: !Word16 -- Number of colour planes
    , bitPerPixel       :: !Word16
    , bitmapCompression :: !Word32
    , byteImageSize     :: !Word32
    , xResolution       :: !Int32  -- ^ Pixels per meter
    , yResolution       :: !Int32  -- ^ Pixels per meter
    , colorCount        :: !Word32 -- ^ Number of colors in the palette
    , importantColours  :: !Word32
    -- Fields added to the header in V2
    , redMask           :: !Word32 -- ^ Red bitfield mask, set to 0 if not used
    , greenMask         :: !Word32 -- ^ Green bitfield mask, set to 0 if not used
    , blueMask          :: !Word32 -- ^ Blue bitfield mask, set to 0 if not used
    -- Fields added to the header in V3
    , alphaMask         :: !Word32 -- ^ Alpha bitfield mask, set to 0 if not used
    -- Fields added to the header in V4
    , colorSpaceType    :: !ColorSpaceType
    , colorSpace        :: !B.ByteString -- ^ Windows color space, not decoded
    -- Fields added to the header in V5
    , iccIntent         :: !Word32
    , iccProfileData    :: !Word32
    , iccProfileSize    :: !Word32
    }
    deriving Show

-- | Size of the Windows BITMAPV4INFOHEADER color space information.
sizeofColorProfile :: Int
sizeofColorProfile = 48

sizeofBmpHeader, sizeofBmpInfoHeader :: Word32
sizeofBmpHeader = 2 + 4 + 2 + 2 + 4
sizeofBmpInfoHeader = 40

sizeofBmpV2Header, sizeofBmpV3Header, sizeofBmpV4Header, sizeofBmpV5Header :: Word32
sizeofBmpV2Header = 52
sizeofBmpV3Header = 56
sizeofBmpV4Header = 108
sizeofBmpV5Header = 124

instance Binary ColorSpaceType where
    put CalibratedRGB         = putWord32le 0
    put DeviceDependentRGB    = putWord32le 1
    put DeviceDependentCMYK   = putWord32le 2
    put ProfileEmbedded       = putWord32le 0x4D424544
    put ProfileLinked         = putWord32le 0x4C494E4B
    put SRGB                  = putWord32le 0x73524742
    put WindowsColorSpace     = putWord32le 0x57696E20
    put (UnknownColorSpace x) = putWord32le x
    get = do
      w <- getWord32le
      return $ case w of
        0          -> CalibratedRGB
        1          -> DeviceDependentRGB
        2          -> DeviceDependentCMYK
        0x4D424544 -> ProfileEmbedded
        0x4C494E4B -> ProfileLinked
        0x73524742 -> SRGB
        0x57696E20 -> WindowsColorSpace
        _          -> UnknownColorSpace w

instance Binary BmpV5Header where
    put hdr = do
        putWord32le $ size hdr
        putInt32le $ width hdr
        putInt32le $ height hdr
        putWord16le $ planes hdr
        putWord16le $ bitPerPixel hdr
        putWord32le $ bitmapCompression hdr
        putWord32le $ byteImageSize hdr
        putInt32le $ xResolution hdr
        putInt32le $ yResolution hdr
        putWord32le $ colorCount hdr
        putWord32le $ importantColours hdr

        when (size hdr > sizeofBmpInfoHeader || bitmapCompression hdr == 3) $ do
          putWord32le $ redMask hdr
          putWord32le $ greenMask hdr
          putWord32le $ blueMask hdr

        when (size hdr > sizeofBmpV2Header) $
          putWord32le $ alphaMask hdr

        when (size hdr > sizeofBmpV3Header) $ do
          put $ colorSpaceType hdr
          putByteString $ colorSpace hdr

        when (size hdr > sizeofBmpV4Header) $ do
          put $ iccIntent hdr
          putWord32le $ iccProfileData hdr
          putWord32le $ iccProfileSize hdr
          putWord32le 0 -- reserved field

    get = do
        readSize <- getWord32le
        readWidth <- getInt32le
        readHeight <- getInt32le
        readPlanes <- getWord16le
        readBitPerPixel <- getWord16le
        readBitmapCompression <- getWord32le
        readByteImageSize <- getWord32le
        readXResolution <- getInt32le
        readYResolution <- getInt32le
        readColorCount <- getWord32le
        readImportantColours <- getWord32le

        (readRedMask, readGreenMask, readBlueMask) <-
          if readSize == sizeofBmpInfoHeader && readBitmapCompression /= 3
            then return (0, 0, 0)
            else do
              -- fields added to the header in V2, but sometimes present
              -- immediately after a plain BITMAPINFOHEADER
              innerReadRedMask <- getWord32le
              innerReadGreenMask <- getWord32le
              innerReadBlueMask <- getWord32le
              return (innerReadRedMask, innerReadGreenMask, innerReadBlueMask)

        -- field added in V3 (undocumented)
        readAlphaMask <- if readSize < sizeofBmpV3Header then return 0 else getWord32le

        (readColorSpaceType, readColorSpace) <-
          if readSize < sizeofBmpV4Header
            then return (DeviceDependentRGB, B.empty)
            else do
              -- fields added in V4 (Windows 95)
              csType <- get
              cs <- getByteString sizeofColorProfile
              return (csType, cs)

        (readIccIntent, readIccProfileData, readIccProfileSize) <-
          if readSize < sizeofBmpV5Header
            then return (0, 0, 0)
            else do
              -- fields added in V5 (Windows 98)
              innerIccIntent <- getWord32le
              innerIccProfileData <- getWord32le
              innerIccProfileSize <- getWord32le
              void getWord32le -- reserved field
              return (innerIccIntent, innerIccProfileData, innerIccProfileSize)

        return BmpV5Header {
            size = readSize,
            width = readWidth,
            height = readHeight,
            planes = readPlanes,
            bitPerPixel = readBitPerPixel,
            bitmapCompression = readBitmapCompression,
            byteImageSize = readByteImageSize,
            xResolution = readXResolution,
            yResolution = readYResolution,
            colorCount = readColorCount,
            importantColours = readImportantColours,
            redMask = readRedMask,
            greenMask = readGreenMask,
            blueMask = readBlueMask,
            alphaMask = readAlphaMask,
            colorSpaceType = readColorSpaceType,
            colorSpace = readColorSpace,
            iccIntent = readIccIntent,
            iccProfileData = readIccProfileData,
            iccProfileSize = readIccProfileSize
        }

newtype BmpPalette = BmpPalette [(Word8, Word8, Word8, Word8)]

putPalette :: BmpPalette -> Put
putPalette (BmpPalette p) = mapM_ (\(r, g, b, a) -> put r >> put g >> put b >> put a) p

putICCProfile :: Maybe B.ByteString -> Put
putICCProfile Nothing = return ()
putICCProfile (Just bytes) = put bytes

-- | All the instance of this class can be written as a bitmap file
-- using this library.
class BmpEncodable pixel where
    bitsPerPixel   :: pixel -> Int
    bmpEncode      :: Image pixel -> Put
    hasAlpha       :: Image pixel -> Bool
    defaultPalette :: pixel -> BmpPalette
    defaultPalette _ = BmpPalette []

stridePut :: M.STVector s Word8 -> Int -> Int -> ST s ()
{-# INLINE stridePut #-}
stridePut vec = inner
 where inner  _ 0 = return ()
       inner ix n = do
           (vec `M.unsafeWrite` ix) 0
           inner (ix + 1) (n - 1)

instance BmpEncodable Pixel8 where
    hasAlpha _ = False
    defaultPalette _ = BmpPalette [(x,x,x, 255) | x <- [0 .. 255]]
    bitsPerPixel _ = 8
    bmpEncode (Image {imageWidth = w, imageHeight = h, imageData = arr}) =
      forM_ [h - 1, h - 2 .. 0] $ \l -> putVector $ runST $ encodeLine l
        where stride = fromIntegral $ linePadding 8 w
              putVector vec = putByteString $ blitVector vec 0 lineWidth
              lineWidth = w + stride

              encodeLine :: forall s. Int -> ST s (VS.Vector Word8)
              encodeLine line = do
                  buff <- M.new lineWidth

                  let lineIdx = line * w
                      inner col | col >= w = return ()
                      inner col = do
                          let v = arr `VS.unsafeIndex` (lineIdx + col)
                          (buff `M.unsafeWrite` col) v
                          inner (col + 1)

                  inner 0

                  stridePut buff w stride
                  VS.unsafeFreeze buff

instance BmpEncodable PixelRGBA8 where
    hasAlpha _ = True
    bitsPerPixel _ = 32
    bmpEncode (Image {imageWidth = w, imageHeight = h, imageData = arr}) = 
      forM_ [h - 1, h - 2 .. 0] $ \l -> putVector $ runST $ putLine l
      where
        putVector vec = putByteString . blitVector vec 0 $ w * 4

        putLine :: forall s. Int -> ST s (VS.Vector Word8)
        putLine line = do
            buff <- M.new $ 4 * w
            let initialIndex = line * w * 4
                inner col _ _ | col >= w = return ()
                inner col writeIdx readIdx = do
                    let r = arr `VS.unsafeIndex` readIdx
                        g = arr `VS.unsafeIndex` (readIdx + 1)
                        b = arr `VS.unsafeIndex` (readIdx + 2)
                        a = arr `VS.unsafeIndex` (readIdx + 3)

                    (buff `M.unsafeWrite` writeIdx) b
                    (buff `M.unsafeWrite` (writeIdx + 1)) g
                    (buff `M.unsafeWrite` (writeIdx + 2)) r
                    (buff `M.unsafeWrite` (writeIdx + 3)) a

                    inner (col + 1) (writeIdx + 4) (readIdx + 4)

            inner 0 0 initialIndex
            VS.unsafeFreeze buff

instance BmpEncodable PixelRGB8 where
    hasAlpha _ = False
    bitsPerPixel _ = 24
    bmpEncode (Image {imageWidth = w, imageHeight = h, imageData = arr}) =
       forM_ [h - 1, h - 2 .. 0] $ \l -> putVector $ runST $ putLine l
        where
          stride = fromIntegral . linePadding 24 $ w

          putVector vec = putByteString $ blitVector vec 0 (w * 3 + stride)

          putLine :: forall s. Int -> ST s (VS.Vector Word8)
          putLine line = do
              buff <- M.new $ w * 3 + stride
              let initialIndex = line * w * 3
                  inner col _ _ | col >= w = return ()
                  inner col writeIdx readIdx = do
                      let r = arr `VS.unsafeIndex` readIdx
                          g = arr `VS.unsafeIndex` (readIdx + 1)
                          b = arr `VS.unsafeIndex` (readIdx + 2)
                      
                      (buff `M.unsafeWrite` writeIdx) b
                      (buff `M.unsafeWrite` (writeIdx + 1)) g
                      (buff `M.unsafeWrite` (writeIdx + 2)) r

                      inner (col + 1) (writeIdx + 3) (readIdx + 3)

              inner 0 0 initialIndex
              VS.unsafeFreeze buff

decodeImageRGBA8 :: BmpV5Header -> (Int, Int, Int, Int) -> B.ByteString -> Image PixelRGBA8
decodeImageRGBA8 (BmpV5Header { width = w, height = h }) (posR, posG, posB, posA) str = Image wi hi stArray where
  wi = fromIntegral w
  hi = abs $ fromIntegral h
  stArray = runST $ do
      arr <- M.new (fromIntegral $ w * abs h * 4)
      if h > 0 then
        foldM_ (readLine arr) 0 [0 .. hi - 1]
      else
        foldM_ (readLine arr) 0 [hi - 1, hi - 2 .. 0]
      VS.unsafeFreeze arr

  stride = linePadding 32 wi -- will be 0

  readLine :: forall s. M.MVector s Word8 -> Int -> Int -> ST s Int
  readLine arr readIndex line = inner readIndex writeIndex where
    lastIndex = wi * (hi - 1 - line + 1) * 4
    writeIndex = wi * (hi - 1 - line) * 4

    inner :: Int -> Int -> ST s Int
    inner readIdx writeIdx | writeIdx >= lastIndex = return $ readIdx + stride
    inner readIdx writeIdx = do
        -- 32-bit BMP pixels are BGRA
        (arr `M.unsafeWrite`  writeIdx     ) (str `B.index` (readIdx + posR))
        (arr `M.unsafeWrite` (writeIdx + 1)) (str `B.index` (readIdx + posG))
        (arr `M.unsafeWrite` (writeIdx + 2)) (str `B.index` (readIdx + posB))
        (arr `M.unsafeWrite` (writeIdx + 3)) (str `B.index` (readIdx + posA))
        inner (readIdx + 4) (writeIdx + 4)

data HiBPP = SixteenBPP | TwentyFourBPP deriving Show

decodeImageRGB8 :: HiBPP -> BmpV5Header -> B.ByteString -> Image PixelRGB8
decodeImageRGB8 hiBpp (BmpV5Header { width = w, height = h, bitPerPixel = bpp }) str = Image wi hi stArray where
  wi = fromIntegral w
  hi = abs $ fromIntegral h
  stArray = runST $ do
      arr <- M.new (fromIntegral $ w * abs h * 3)
      if h > 0 then
        foldM_ (readLine arr) 0 [0 .. hi - 1]
      else
        foldM_ (readLine arr) 0 [hi - 1, hi - 2 .. 0]
      VS.unsafeFreeze arr

  stride = linePadding (fromIntegral bpp) wi

  readLine :: forall s. M.MVector s Word8 -> Int -> Int -> ST s Int
  readLine arr readIndex line = case hiBpp of
      SixteenBPP -> inner16 readIndex writeIndex
      TwentyFourBPP -> inner24 readIndex writeIndex
    where
      lastIndex = wi * (hi - 1 - line + 1) * 3
      writeIndex = wi * (hi - 1 - line) * 3

      expand5To8Bits x = round $ 255/(31 :: Double) * fromIntegral (x .&. 0x1F)

      inner24 readIdx writeIdx | writeIdx >= lastIndex = return $ readIdx + stride
      inner24 readIdx writeIdx = do
          (arr `M.unsafeWrite`  writeIdx     ) (str `B.index` (readIdx + 2))
          (arr `M.unsafeWrite` (writeIdx + 1)) (str `B.index` (readIdx + 1))
          (arr `M.unsafeWrite` (writeIdx + 2)) (str `B.index`  readIdx)
          inner24 (readIdx + 3) (writeIdx + 3)

      inner16 readIdx writeIdx | writeIdx >= lastIndex = return $ readIdx + stride
      inner16 readIdx writeIdx = do
          let rawPixel = ((fromIntegral (str `B.index` (readIdx + 1)) `unsafeShiftL` 8)
                          .|. fromIntegral (str `B.index` readIdx)) :: Word16
          (arr `M.unsafeWrite`  writeIdx     ) (expand5To8Bits $ rawPixel `unsafeShiftR` 10)
          (arr `M.unsafeWrite` (writeIdx + 1)) (expand5To8Bits $ rawPixel `unsafeShiftR` 5)
          (arr `M.unsafeWrite` (writeIdx + 2)) (expand5To8Bits   rawPixel)
          inner16 (readIdx + 2) (writeIdx + 3)

data LowBPP = OneBPP | FourBPP | EightBPP deriving Show

decodeImageY8 :: LowBPP -> BmpV5Header -> B.ByteString -> Image Pixel8
decodeImageY8 lowBPP (BmpV5Header { width = w, height = h, bitPerPixel = bpp }) str = Image wi hi stArray where
  wi = fromIntegral w
  hi = abs $ fromIntegral h
  stArray = runST $ do
      arr <- M.new . fromIntegral $ w * abs h
      if h > 0 then
        foldM_ (readLine arr) 0 [0 .. hi - 1]
      else
        foldM_ (readLine arr) 0 [hi - 1, hi - 2 .. 0]
      VS.unsafeFreeze arr

  stride = linePadding (fromIntegral bpp) wi
  
  readLine :: forall s. M.MVector s Word8 -> Int -> Int -> ST s Int
  readLine arr readIndex line = case lowBPP of
      OneBPP -> inner1 readIndex writeIndex
      FourBPP -> inner4 readIndex writeIndex
      EightBPP -> inner8 readIndex writeIndex
    where
      lastIndex = wi * (hi - 1 - line + 1)
      writeIndex = wi * (hi - 1 - line)

      inner8 readIdx writeIdx | writeIdx >= lastIndex = return $ readIdx + stride
      inner8 readIdx writeIdx = do
        (arr `M.unsafeWrite` writeIdx) (str `B.index` readIdx)
        inner8 (readIdx + 1) (writeIdx + 1)

      inner4 readIdx writeIdx | writeIdx >= lastIndex = return $ readIdx + stride
      inner4 readIdx writeIdx = do
        let byte = str `B.index` readIdx
        if writeIdx >= lastIndex - 1 then do
          (arr `M.unsafeWrite` writeIdx) (byte `unsafeShiftR` 4)
          inner4 (readIdx + 1) (writeIdx + 1)
        else do
          (arr `M.unsafeWrite` writeIdx) (byte `unsafeShiftR` 4)
          (arr `M.unsafeWrite` (writeIdx + 1)) (byte .&. 0x0F)
          inner4 (readIdx + 1) (writeIdx + 2)

      inner1 readIdx writeIdx | writeIdx >= lastIndex = return $ readIdx + stride
      inner1 readIdx writeIdx = do
        let byte = str `B.index` readIdx
        let toWrite = (lastIndex - writeIdx) `min` 8
        forM_ [0 .. (toWrite - 1)] $ \i ->
          when (byte `testBit` (7 - i)) $ (arr `M.unsafeWrite` (writeIdx + i)) 1
        inner1 (readIdx + 1) (writeIdx + toWrite)

decodeImageY8RLE :: Bool -> BmpV5Header -> B.ByteString -> Image Pixel8
decodeImageY8RLE is4bpp (BmpV5Header { width = w, height = h, byteImageSize = sz }) str = Image wi hi stArray where
  wi = fromIntegral w
  hi = abs $ fromIntegral h
  xOffsetMax = wi - 1

  stArray = runST $ do
    arr <- M.new . fromIntegral $ w * abs h
    decodeRLE arr (B.unpack (B.take (fromIntegral sz) str)) ((hi - 1) * wi, 0)
    VS.unsafeFreeze arr

  decodeRLE :: forall s . M.MVector s Word8 -> [Word8] -> (Int, Int) -> ST s ()
  decodeRLE arr = inner
    where
      inner :: [Word8] -> (Int, Int) -> ST s ()
      inner [] _ = return ()
      inner (0 : 0 : rest) (yOffset, _) = inner rest (yOffset - wi, 0)
      inner (0 : 1 : _) _ = return ()
      inner (0 : 2 : hOffset : vOffset : rest) (yOffset, _) =
        inner rest (yOffset - (wi * fromIntegral vOffset), fromIntegral hOffset)
      inner (0 : n : rest) writePos = 
        let isPadded = if is4bpp then (n + 3) .&. 0x3 < 2 else odd n
        in copyN isPadded (fromIntegral n) rest writePos
      inner (n : b : rest) writePos = writeN (fromIntegral n) b rest writePos
      inner _ _ = return ()

      -- | Write n copies of a byte to the output array.
      writeN :: Int -> Word8 -> [Word8] -> (Int, Int) -> ST s ()
      writeN 0 _ rest writePos = inner rest writePos
      writeN n b rest writePos =
        case (is4bpp, n) of
          (True, 1) ->
            writeByte (b `unsafeShiftR` 4) writePos >>= writeN (n - 1) b rest
          (True, _) ->
            writeByte (b `unsafeShiftR` 4) writePos
              >>= writeByte (b .&. 0x0F) >>= writeN (n - 2) b rest
          (False, _) ->
            writeByte b writePos >>= writeN (n - 1) b rest

      -- | Copy the next byte to the output array, possibly ignoring a padding byte at the end.
      copyN :: Bool -> Int -> [Word8] -> (Int, Int) -> ST s ()
      copyN _ _ [] _ = return ()
      copyN False 0 rest writePos = inner rest writePos
      copyN True 0 (_:rest) writePos = inner rest writePos
      copyN isPadded n (b : rest) writePos =
        case (is4bpp, n) of
          (True, 1) ->
            writeByte (b `unsafeShiftR` 4) writePos >>= copyN isPadded (n - 1) rest
          (True, _) ->
            writeByte (b `unsafeShiftR` 4) writePos
              >>= writeByte (b .&. 0x0F) >>= copyN isPadded (n - 2) rest
          (False, _) ->
            writeByte b writePos >>= copyN isPadded (n - 1) rest

      -- | Write the next byte to the output array.
      writeByte :: Word8 -> (Int, Int) -> ST s (Int, Int)
      writeByte byte (yOffset, xOffset) = do
        (arr `M.unsafeWrite` (yOffset + xOffset)) byte
        return (yOffset, (xOffset + 1) `min` xOffsetMax)

pixelGet :: Get [Word8]
pixelGet = do
    b <- getWord8
    g <- getWord8
    r <- getWord8
    _ <- getWord8
    return [r, g, b]

metadataOfHeader :: BmpV5Header -> Maybe B.ByteString -> Metadatas
metadataOfHeader hdr iccProfile =
    cs <> Met.simpleMetadata Met.SourceBitmap (width hdr) (abs $ height hdr) dpiX dpiY
  where
    dpiX = Met.dotsPerMeterToDotPerInch . fromIntegral $ xResolution hdr
    dpiY = Met.dotsPerMeterToDotPerInch . fromIntegral $ yResolution hdr
    cs = case colorSpaceType hdr of
          CalibratedRGB -> Met.singleton
            Met.ColorSpace (Met.WindowsBitmapColorSpace $ colorSpace hdr)
          SRGB -> Met.singleton Met.ColorSpace Met.SRGB
          ProfileEmbedded -> case iccProfile of
                              Nothing -> Met.empty
                              Just profile -> Met.singleton Met.ColorSpace
                                                (Met.ICCProfile profile)
          _ -> Met.empty

-- | Try to decode a bitmap image.
-- Right now this function can output the following image:
--
--   - 'ImageY8'
--
--   - 'ImageRGB8'
--
--   - 'ImageRGBA8'
--
decodeBitmap :: B.ByteString -> Either String DynamicImage
decodeBitmap = fmap fst . decodeBitmapWithMetadata

-- | Same as 'decodeBitmap' but also extracts metadata.
decodeBitmapWithMetadata :: B.ByteString -> Either String (DynamicImage, Metadatas)
decodeBitmapWithMetadata byte =
  first palettedToTrueColor <$> decodeBitmapWithPaletteAndMetadata byte

-- | Same as 'decodeBitmap' but also extracts metadata and provide separated palette.
decodeBitmapWithPaletteAndMetadata :: B.ByteString -> Either String (PalettedImage, Metadatas)
decodeBitmapWithPaletteAndMetadata str = flip runGetStrict str $ do
  fileHeader <- get :: Get BmpHeader
  bmpHeader  <- get :: Get BmpV5Header

  readed <- bytesRead
  when (readed > fromIntegral (dataOffset fileHeader))
       (fail "Invalid bmp image, data in header")

  when (width bmpHeader <= 0)
       (fail $ "Invalid bmp width, " ++ show (width bmpHeader))

  when (height bmpHeader == 0)
       (fail $ "Invalid bmp height (0) ")

  decodeBitmapWithHeaders fileHeader bmpHeader

-- | Decode the rest of a bitmap, after the headers have been decoded.
decodeBitmapWithHeaders :: BmpHeader -> BmpV5Header -> Get (PalettedImage, Metadatas)
decodeBitmapWithHeaders fileHdr hdr = do
    img <- bitmapData
    profile <- getICCProfile
    return $ addMetadata profile img

  where
    bpp = fromIntegral $ bitPerPixel hdr :: Int
    paletteColorCount
      | colorCount hdr == 0 = 2 ^ bpp
      | otherwise = fromIntegral $ colorCount hdr

    addMetadata profile i = (i, metadataOfHeader hdr profile)

    getData = do
      readed <- bytesRead
      label "Start of pixel data" $
        skip . fromIntegral $ dataOffset fileHdr - fromIntegral readed
      let pixelBytes = if bitmapCompression hdr == 1 || bitmapCompression hdr == 2
                          then fromIntegral $ byteImageSize hdr
                          else sizeofPixelData bpp (fromIntegral $ width hdr)
                                                   (fromIntegral $ height hdr)
      label "Pixel data" $ getByteString pixelBytes

    getICCProfile =
      if size hdr >= sizeofBmpV5Header
          && colorSpaceType hdr == ProfileLinked
          && iccProfileData hdr > 0
          && iccProfileSize hdr > 0
      then do
        readSoFar <- bytesRead
        label "Start of embedded ICC color profile" $
          skip $ fromIntegral (iccProfileData hdr) - fromIntegral readSoFar
        profile <- label "Embedded ICC color profile" $
                      getByteString . fromIntegral $ iccProfileSize hdr
        return (Just profile)
      else return Nothing

    bitmapData = case (bitPerPixel hdr, planes hdr, bitmapCompression hdr) of
      (32, 1, 0) -> do
        rest <- getData
        return . TrueColorImage . ImageRGBA8
              $ decodeImageRGBA8 hdr (2, 1, 0, 3) rest
        -- (2, 1, 0, 3) means BGRA pixel order
      (32, 1, 3) -> do
        posRed   <- getBitfield $ redMask hdr
        posGreen <- getBitfield $ greenMask hdr
        posBlue  <- getBitfield $ blueMask hdr
        posAlpha <- getBitfield $ alphaMask hdr
        rest     <- getData
        return . TrueColorImage . ImageRGBA8 $
          decodeImageRGBA8 hdr (posRed, posGreen, posBlue, posAlpha) rest
      (24, 1, 0) -> do
        rest <- getData
        return . TrueColorImage . ImageRGB8 $
          decodeImageRGB8 TwentyFourBPP hdr rest
      (16, 1, 0) -> do
        rest <- getData
        return . TrueColorImage . ImageRGB8 $ 
          decodeImageRGB8 SixteenBPP hdr rest
      ( _, 1, compression) -> do
        table <- replicateM paletteColorCount pixelGet
        rest <- getData
        let palette = Palette'
              { _paletteSize = paletteColorCount
              , _paletteData = VS.fromListN (paletteColorCount * 3) $ concat table
              }
        image <-
          case (bpp, compression) of
            (8, 0) -> return $ decodeImageY8 EightBPP hdr rest
            (4, 0) -> return $ decodeImageY8 FourBPP hdr rest
            (1, 0) -> return $ decodeImageY8 OneBPP hdr rest
            (8, 1) -> return $ decodeImageY8RLE False hdr rest
            (4, 2) -> return $ decodeImageY8RLE True hdr rest
            (a, b) -> fail $ "Can't handle BMP file " ++ show (a, 1 :: Int, b)

        return $ PalettedRGB8 image palette

      a          -> fail $ "Can't handle BMP file " ++ show a

getBitfield :: Monad m => Word32 -> m Int
getBitfield w32 = case w32 of
    0xFF000000 -> return 3
    0x00FF0000 -> return 2
    0x0000FF00 -> return 1
    0x000000FF -> return 0
    _          -> fail $
      "Codec.Picture.Bitmap.getBitfield: unsupported bitfield of " ++ show w32

-- | Compute the size of the pixel data
sizeofPixelData :: Int -> Int -> Int -> Int
sizeofPixelData bpp lineWidth nLines = ((bpp * (abs lineWidth) + 31) `div` 32) * 4 * abs nLines

-- | Write an image in a file use the bitmap format.
writeBitmap :: (BmpEncodable pixel)
            => FilePath -> Image pixel -> IO ()
writeBitmap filename img = L.writeFile filename $ encodeBitmap img

linePadding :: Int -> Int -> Int
linePadding bpp imgWidth = (4 - (bytesPerLine `mod` 4)) `mod` 4
  where bytesPerLine = (bpp * imgWidth + 7) `div` 8

-- | Encode an image into a bytestring in .bmp format ready to be written
-- on disk.
encodeBitmap :: forall pixel. (BmpEncodable pixel) => Image pixel -> L.ByteString
encodeBitmap = encodeBitmapWithPalette (defaultPalette (undefined :: pixel))

-- | Equivalent to 'encodeBitmap' but also store
-- the following metadatas:
--
--  * 'Codec.Picture.Metadata.DpiX'
--  * 'Codec.Picture.Metadata.DpiY' 
--
encodeBitmapWithMetadata :: forall pixel. BmpEncodable pixel
                         => Metadatas -> Image pixel -> L.ByteString
encodeBitmapWithMetadata metas =
  encodeBitmapWithPaletteAndMetadata metas (defaultPalette (undefined :: pixel))

-- | Write a dynamic image in a .bmp image file if possible.
-- The same restriction as 'encodeDynamicBitmap' apply.
writeDynamicBitmap :: FilePath -> DynamicImage -> IO (Either String Bool)
writeDynamicBitmap path img = case encodeDynamicBitmap img of
        Left err -> return $ Left err
        Right b  -> L.writeFile path b >> return (Right True)

-- | Encode a dynamic image in BMP if possible, supported images are:
--
--   - 'ImageY8'
--
--   - 'ImageRGB8'
--
--   - 'ImageRGBA8'
--
encodeDynamicBitmap :: DynamicImage -> Either String L.ByteString
encodeDynamicBitmap (ImageRGB8 img) = Right $ encodeBitmap img
encodeDynamicBitmap (ImageRGBA8 img) = Right $ encodeBitmap img
encodeDynamicBitmap (ImageY8 img) = Right $ encodeBitmap img
encodeDynamicBitmap _ = Left "Unsupported image format for bitmap export"

extractDpiOfMetadata :: Metadatas -> (Word32, Word32)
extractDpiOfMetadata metas = (fetch Met.DpiX, fetch Met.DpiY) where
  fetch k = maybe 0 (fromIntegral . Met.dotPerInchToDotsPerMeter) $ Met.lookup k metas

-- | Convert an image to a bytestring ready to be serialized.
encodeBitmapWithPalette :: forall pixel. (BmpEncodable pixel)
                        => BmpPalette -> Image pixel -> L.ByteString
encodeBitmapWithPalette = encodeBitmapWithPaletteAndMetadata mempty

-- | Equivalent to 'encodeBitmapWithPalette' but also store
-- the following metadatas:
--
--  * 'Codec.Picture.Metadata.DpiX'
--  * 'Codec.Picture.Metadata.DpiY' 
--
encodeBitmapWithPaletteAndMetadata :: forall pixel. (BmpEncodable pixel)
                                   => Metadatas -> BmpPalette -> Image pixel
                                   -> L.ByteString
encodeBitmapWithPaletteAndMetadata metas pal@(BmpPalette palette) img =
  runPut $ put hdr >> put info >> putPalette pal >> bmpEncode img
                   >> putICCProfile colorProfileData

    where imgWidth = fromIntegral $ imageWidth img
          imgHeight = fromIntegral $ imageHeight img
          (dpiX, dpiY) = extractDpiOfMetadata metas
          cs = Met.lookup Met.ColorSpace metas
          colorType = case cs of
                        Just Met.SRGB -> SRGB
                        Just (Met.WindowsBitmapColorSpace _) -> CalibratedRGB
                        Just (Met.ICCProfile _) -> ProfileEmbedded
                        Nothing -> DeviceDependentRGB

          colorSpaceInfo = case cs of
                            Just (Met.WindowsBitmapColorSpace bytes) -> bytes
                            _ -> B.pack $ replicate sizeofColorProfile 0

          colorProfileData = case cs of
                              Just (Met.ICCProfile bytes) -> Just bytes
                              _ -> Nothing

          headerSize | colorType == ProfileEmbedded                = sizeofBmpV5Header
                     | colorType == CalibratedRGB || hasAlpha img  = sizeofBmpV4Header
                     | otherwise                                   = sizeofBmpInfoHeader

          paletteSize = fromIntegral $ length palette
          bpp = bitsPerPixel (undefined :: pixel)

          profileSize = fromIntegral $ maybe 0 B.length colorProfileData
          imagePixelSize = fromIntegral $ sizeofPixelData bpp imgWidth imgHeight
          offsetToData = sizeofBmpHeader + headerSize + 4 * paletteSize
          offsetToICCProfile = offsetToData + imagePixelSize <$ colorProfileData
          sizeOfFile = sizeofBmpHeader + headerSize + 4 * paletteSize
                        + imagePixelSize + profileSize

          hdr = BmpHeader {
              magicIdentifier = bitmapMagicIdentifier,
              fileSize = sizeOfFile,
              reserved1 = 0,
              reserved2 = 0,
              dataOffset = offsetToData
          }

          info = BmpV5Header {
              size = headerSize,
              width = fromIntegral imgWidth,
              height = fromIntegral imgHeight,
              planes = 1,
              bitPerPixel = fromIntegral bpp,
              bitmapCompression = if hasAlpha img then 3 else 0,
              byteImageSize = imagePixelSize,
              xResolution = fromIntegral dpiX,
              yResolution = fromIntegral dpiY,
              colorCount = paletteSize,
              importantColours = 0,
              redMask   = if hasAlpha img then 0x00FF0000 else 0,
              greenMask = if hasAlpha img then 0x0000FF00 else 0,
              blueMask  = if hasAlpha img then 0x000000FF else 0,
              alphaMask = if hasAlpha img then 0xFF000000 else 0,
              colorSpaceType = colorType,
              colorSpace = colorSpaceInfo,
              iccIntent = 0,
              iccProfileData = fromMaybe 0 offsetToICCProfile,
              iccProfileSize = profileSize
          }


{-# ANN module "HLint: ignore Reduce duplication" #-}

