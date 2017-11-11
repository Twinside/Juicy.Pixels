-- | This module provide a totally partial and incomplete maping
-- of Exif values. Used for Tiff parsing and reused for Exif extraction.
module Codec.Picture.Metadata.Exif ( ExifTag( .. )
                                   , ExifData( .. )

                                   , tagOfWord16
                                   , word16OfTag

                                   , isInIFD0
                                   ) where

import Control.DeepSeq( NFData( .. ) )
import Data.Int( Int32 )
import Data.Word( Word16, Word32 )
import qualified Data.Vector as V
import qualified Data.ByteString as B

-- | Tag values used for exif fields. Completly incomplete
data ExifTag
  = TagPhotometricInterpretation
  | TagCompression -- ^ Short type
  | TagImageWidth  -- ^ Short or long type
  | TagImageLength -- ^ Short or long type
  | TagXResolution -- ^ Rational type
  | TagYResolution -- ^ Rational type
  | TagResolutionUnit --  ^ Short type
  | TagRowPerStrip -- ^ Short or long type
  | TagStripByteCounts -- ^ Short or long
  | TagStripOffsets -- ^ Short or long
  | TagBitsPerSample --  ^ Short
  | TagColorMap -- ^ Short
  | TagTileWidth
  | TagTileLength
  | TagTileOffset
  | TagTileByteCount
  | TagSamplesPerPixel -- ^ Short
  | TagArtist
  | TagDocumentName
  | TagSoftware
  | TagPlanarConfiguration -- ^ Short
  | TagOrientation
  | TagSampleFormat -- ^ Short
  | TagInkSet
  | TagSubfileType
  | TagFillOrder
  | TagYCbCrCoeff
  | TagYCbCrSubsampling
  | TagYCbCrPositioning
  | TagReferenceBlackWhite
  | TagXPosition
  | TagYPosition
  | TagExtraSample
  | TagImageDescription
  | TagPredictor
  | TagCopyright
  | TagMake
  | TagModel
  | TagDateTime
  | TagGPSInfo
  | TagLightSource -- ^ Short
  | TagFlash -- ^ Short

  | TagJpegProc
  | TagJPEGInterchangeFormat
  | TagJPEGInterchangeFormatLength
  | TagJPEGRestartInterval
  | TagJPEGLosslessPredictors
  | TagJPEGPointTransforms
  | TagJPEGQTables
  | TagJPEGDCTables
  | TagJPEGACTables

  | TagExifOffset
  | TagUnknown !Word16
  deriving (Eq, Ord, Show)

-- | Convert a value to it's corresponding Exif tag.
-- Will often be written as 'TagUnknown'
tagOfWord16 :: Word16 -> ExifTag
tagOfWord16 v = case v of
  255 -> TagSubfileType
  256 -> TagImageWidth
  257 -> TagImageLength
  258 -> TagBitsPerSample
  259 -> TagCompression
  262 -> TagPhotometricInterpretation
  266 -> TagFillOrder
  269 -> TagDocumentName
  270 -> TagImageDescription
  271 -> TagMake
  272 -> TagModel
  273 -> TagStripOffsets
  274 -> TagOrientation
  277 -> TagSamplesPerPixel
  278 -> TagRowPerStrip
  279 -> TagStripByteCounts
  282 -> TagXResolution
  283 -> TagYResolution
  284 -> TagPlanarConfiguration
  286 -> TagXPosition
  287 -> TagYPosition
  296 -> TagResolutionUnit
  305 -> TagSoftware
  306 -> TagDateTime
  315 -> TagArtist
  317 -> TagPredictor
  320 -> TagColorMap
  322 -> TagTileWidth
  323 -> TagTileLength
  324 -> TagTileOffset
  325 -> TagTileByteCount
  332 -> TagInkSet
  338 -> TagExtraSample
  339 -> TagSampleFormat
  529 -> TagYCbCrCoeff
  512 -> TagJpegProc
  513 -> TagJPEGInterchangeFormat
  514 -> TagJPEGInterchangeFormatLength
  515 -> TagJPEGRestartInterval
  517 -> TagJPEGLosslessPredictors
  518 -> TagJPEGPointTransforms
  519 -> TagJPEGQTables
  520 -> TagJPEGDCTables
  521 -> TagJPEGACTables
  530 -> TagYCbCrSubsampling
  531 -> TagYCbCrPositioning
  532 -> TagReferenceBlackWhite
  33432 -> TagCopyright
  34665 -> TagExifOffset
  34853 -> TagGPSInfo
  37384 -> TagLightSource
  37385 -> TagFlash
  vv -> TagUnknown vv

-- | Convert a tag to it's corresponding value.
word16OfTag :: ExifTag -> Word16
word16OfTag t = case t of
  TagSubfileType -> 255
  TagImageWidth -> 256
  TagImageLength -> 257
  TagBitsPerSample -> 258
  TagCompression -> 259
  TagPhotometricInterpretation -> 262
  TagFillOrder -> 266
  TagDocumentName -> 269
  TagImageDescription -> 270
  TagMake -> 271
  TagModel -> 272
  TagStripOffsets -> 273
  TagOrientation -> 274
  TagSamplesPerPixel -> 277
  TagRowPerStrip -> 278
  TagStripByteCounts -> 279
  TagXResolution -> 282
  TagYResolution -> 283
  TagPlanarConfiguration -> 284
  TagXPosition -> 286
  TagYPosition -> 287
  TagResolutionUnit -> 296
  TagSoftware -> 305
  TagDateTime -> 306
  TagArtist -> 315
  TagPredictor -> 317
  TagColorMap -> 320
  TagTileWidth -> 322
  TagTileLength -> 323
  TagTileOffset -> 324
  TagTileByteCount -> 325
  TagInkSet -> 332
  TagExtraSample -> 338
  TagSampleFormat -> 339
  TagYCbCrCoeff -> 529
  TagJpegProc -> 512
  TagJPEGInterchangeFormat -> 513
  TagJPEGInterchangeFormatLength -> 514
  TagJPEGRestartInterval -> 515
  TagJPEGLosslessPredictors -> 517
  TagJPEGPointTransforms -> 518
  TagJPEGQTables -> 519
  TagJPEGDCTables -> 520
  TagJPEGACTables -> 521
  TagYCbCrSubsampling -> 530
  TagYCbCrPositioning -> 531
  TagReferenceBlackWhite -> 532
  TagCopyright -> 33432
  TagExifOffset -> 34665
  TagGPSInfo -> 34853
  TagLightSource -> 37384
  TagFlash -> 37385
  (TagUnknown v) -> v

isInIFD0 :: ExifTag -> Bool
isInIFD0 t = word16OfTag t <= lastTag || isRedirectTag where
  lastTag = word16OfTag TagCopyright
  isRedirectTag = t `elem` [TagExifOffset, TagGPSInfo]

-- | Possible data held by an Exif tag
data ExifData
  = ExifNone
  | ExifLong      !Word32
  | ExifShort     !Word16
  | ExifString    !B.ByteString
  | ExifUndefined !B.ByteString
  | ExifShorts    !(V.Vector Word16)
  | ExifLongs     !(V.Vector Word32)
  | ExifRational  !Word32 !Word32
  | ExifSignedRational  !Int32 !Int32
  | ExifIFD       ![(ExifTag, ExifData)]
  deriving Show

instance NFData ExifTag where
  rnf a = a `seq` ()

instance NFData ExifData where
  rnf (ExifIFD ifds) = rnf ifds `seq` ()
  rnf (ExifLongs l) = rnf l `seq` ()
  rnf (ExifShorts l) = rnf l `seq` ()
  rnf a = a `seq` ()
