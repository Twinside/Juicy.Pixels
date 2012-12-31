module Codec.Picture.HDR
        -- ( decodeHDR, encodeHDR )
        where

import Data.Char( ord, chr, isDigit )
import Data.Word( Word8 )
import Control.Applicative( pure, (<$>), (<*>) )
import Control.Monad( when )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as BC

import Data.List( partition )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( Get, getByteString, getWord8 )
import Data.Binary.Put( putByteString )

import Control.Monad.ST( ST )
import Foreign.Storable ( Storable )
import Control.Monad.Primitive ( PrimState, PrimMonad )
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

import Codec.Picture.InternalHelper
{-import Codec.Picture.Types-}

{-# INLINE (!!!) #-}
(!!!) :: (Storable e) => V.Vector e -> Int -> e
(!!!) = -- (!) 
        V.unsafeIndex

{-# INLINE (.!!!.) #-}
(.!!!.) :: (PrimMonad m, Storable a)
        => M.STVector (PrimState m) a -> Int -> m a
(.!!!.) = -- M.read
          M.unsafeRead

{-# INLINE (.<-.) #-}
(.<-.) :: (PrimMonad m, Storable a)
       => M.STVector (PrimState m) a -> Int -> a -> m ()
(.<-.) = -- M.write 
         M.unsafeWrite

data RGBE = RGBE !Word8 !Word8 !Word8 !Word8

instance Binary RGBE where
    put (RGBE r g b e) = put r >> put g >> put b >> put e
    get = RGBE <$> get <*> get <*> get <*> get

isOldRunLengthMarker :: RGBE -> Bool
isOldRunLengthMarker (RGBE 1 1 1 _) = True
isOldRunLengthMarker _ = False

isNewRunLengthMarker :: RGBE -> Bool
isNewRunLengthMarker (RGBE 2 2 2 _) = True
isNewRunLengthMarker _ = False

data RadianceFormat =
      FormatRGBE
    | FormatXYZE

radiance32bitRleRGBEFormat, radiance32bitRleXYZEFromat :: B.ByteString
radiance32bitRleRGBEFormat = BC.pack "32-bit_rle_rgbe"
radiance32bitRleXYZEFromat = BC.pack "32-bit_rle_xyze"

instance Binary RadianceFormat where
  put FormatRGBE = putByteString radiance32bitRleRGBEFormat
  put FormatXYZE = putByteString radiance32bitRleXYZEFromat

  get = getByteString (B.length radiance32bitRleRGBEFormat) >>= format
    where format sig
            | sig == radiance32bitRleRGBEFormat = pure FormatRGBE
            | sig == radiance32bitRleXYZEFromat = pure FormatXYZE
            | otherwise = fail "Unrecognized Radiance format"

dropUntil :: Word8 -> Get ()
dropUntil c = getWord8 >>= inner
  where inner val | val == c = pure ()
        inner _ = getWord8 >>= inner

getUntil :: (Word8 -> Bool) -> B.ByteString -> Get B.ByteString
getUntil f initialAcc = getWord8 >>= inner initialAcc
  where inner acc c | f c = pure acc
        inner acc c = getWord8 >>= inner (B.snoc acc c)

data RadianceHeader = RadianceHeader
  { radianceInfos :: [(B.ByteString, B.ByteString)]
  , radianceFormat :: RadianceFormat 
  , radianceWidth  :: !Int
  , radianceHeight :: !Int
  , radianceData   :: B.ByteString
  }

radianceFileSignature :: B.ByteString
radianceFileSignature = BC.pack "#?RADIANCE\n"

parsePair :: Get (B.ByteString, B.ByteString)
parsePair = do
    let eol c = c == fromIntegral (ord '\n')
    line <- getUntil eol B.empty
    case BC.split '=' line of
      [] -> pure (B.empty, B.empty)
      [val] -> pure (B.empty, val)
      [key, val] -> pure (key, val)
      (key : vals) -> pure (key, B.concat vals)

decodeInfos :: Get [(B.ByteString, B.ByteString)]
decodeInfos = do
    char <- getChar8
    case char of
      -- comment
      '#' -> dropUntil (fromIntegral $ ord '\n') >> decodeInfos
      -- end of header, no more information
      '\n' -> pure []
      -- Classical parsing
      _ -> (:) <$> parsePair <*> decodeInfos


decodeHDR :: a -> Get ()
decodeHDR _ = pure ()

encodeHDR :: a -> L.ByteString
encodeHDR _ = L.empty

getChar8 :: Get Char
getChar8 = chr . fromIntegral <$> getWord8

isSign :: Char -> Bool
isSign c = c == '+' || c == '-'

isAxisLetter :: Char -> Bool
isAxisLetter c = c == 'X' || c == 'Y'

decodeNum :: Get Int
decodeNum = do
    sign <- getChar8
    letter <- getChar8
    space <- getChar8

    when (not $ isSign sign && isAxisLetter letter && space == ' ')
         (fail "Invalid radiance size declaration")

    let numDec acc c | isDigit c =
            getChar8 >>= numDec (acc * 10 + ord c - (ord '0'))
        numDec acc _
            | sign == '-' = pure $ negate acc
            | otherwise = pure acc

    getChar8 >>= numDec 0
    

decodeHeader :: Get RadianceHeader
decodeHeader = do
    sig <- getByteString $ B.length radianceFileSignature
    when (sig /= radianceFileSignature)
         (fail "Invalid radiance file signature")

    infos <- decodeInfos
    let formatKey = BC.pack "FORMAT"
    case partition (\(k,_) -> k /= formatKey) infos of
      (_, []) -> fail "No radiance format specified"
      (info, [(_, formatString)]) ->
        case runGet get $ L.fromChunks [formatString] of
          Left err -> fail err
          Right format ->
              RadianceHeader info format <$> decodeNum
                                         <*> decodeNum
                                         <*> getRemainingBytes

      _ -> fail "Multiple radiance format specified"

