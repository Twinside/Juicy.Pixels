{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
module Codec.Picture.Jpg.Common
    ( DctCoefficients
    , JpgUnpackerParameter( .. )
    , decodeInt
    , dcCoefficientDecode
    , deQuantize
    , decodeRrrrSsss
    , zigZagReorderForward 
    , zigZagReorderForwardv
    , zigZagReorder
    , inverseDirectCosineTransform
    , unpackInt
    , unpackMacroBlock
    , rasterMap
    , decodeMacroBlock
    , decodeRestartInterval
    , toBlockSize
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( pure, (<$>) )
#endif

import Control.Monad( when )
import Control.Monad.ST( ST, runST )
import Data.Bits( unsafeShiftL, unsafeShiftR, (.&.) )
import Data.Int( Int16, Int32 )
import Data.Maybe( fromMaybe )
import Data.Word( Word8 )
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as M
import Foreign.Storable ( Storable )

import Codec.Picture.Types
import Codec.Picture.BitWriter
import Codec.Picture.Jpg.Types
import Codec.Picture.Jpg.FastIdct
import Codec.Picture.Jpg.DefaultTable

-- | Same as for DcCoefficient, to provide nicer type signatures
type DctCoefficients = DcCoefficient

data JpgUnpackerParameter = JpgUnpackerParameter
    { dcHuffmanTree        :: !HuffmanPackedTree
    , acHuffmanTree        :: !HuffmanPackedTree
    , componentIndex       :: {-# UNPACK #-} !Int
    , restartInterval      :: {-# UNPACK #-} !Int
    , componentWidth       :: {-# UNPACK #-} !Int
    , componentHeight      :: {-# UNPACK #-} !Int
    , subSampling          :: !(Int, Int)
    , coefficientRange     :: !(Int, Int)
    , successiveApprox     :: !(Int, Int)
    , readerIndex          :: {-# UNPACK #-} !Int
      -- | When in progressive mode, we can have many
      -- color in a scan are only one. The indices changes
      -- on this fact, when mixed, there is whole 
      -- MCU for all color components, spanning multiple
      -- block lines. With only one color component we use
      -- the normal raster order.
    , indiceVector         :: {-# UNPACK #-} !Int
    , blockIndex           :: {-# UNPACK #-} !Int
    , blockMcuX            :: {-# UNPACK #-} !Int
    , blockMcuY            :: {-# UNPACK #-} !Int
    }
    deriving Show

toBlockSize :: Int -> Int
toBlockSize v = (v + 7) `div` 8

decodeRestartInterval :: BoolReader s Int32
decodeRestartInterval = return (-1) {-  do
  bits <- replicateM 8 getNextBitJpg
  if bits == replicate 8 True
     then do
         marker <- replicateM 8 getNextBitJpg
         return $ packInt marker
     else return (-1)
        -}

{-# INLINE decodeInt #-}
decodeInt :: Int -> BoolReader s Int32
decodeInt ssss = do
    signBit <- getNextBitJpg
    let dataRange = 1 `unsafeShiftL` fromIntegral (ssss - 1)
        leftBitCount = ssss - 1
    -- First following bits store the sign of the coefficient, and counted in
    -- SSSS, so the bit count for the int, is ssss - 1
    if signBit
       then (\w -> dataRange + fromIntegral w) <$> unpackInt leftBitCount
       else (\w -> 1 - dataRange * 2 + fromIntegral w) <$> unpackInt leftBitCount

decodeRrrrSsss :: HuffmanPackedTree -> BoolReader s (Int, Int)
decodeRrrrSsss tree = do
    rrrrssss <- huffmanPackedDecode tree
    let rrrr = (rrrrssss `unsafeShiftR` 4) .&. 0xF
        ssss =  rrrrssss .&. 0xF
    pure (fromIntegral rrrr, fromIntegral ssss)

dcCoefficientDecode :: HuffmanPackedTree -> BoolReader s DcCoefficient
dcCoefficientDecode dcTree = do
    ssss <- huffmanPackedDecode dcTree
    if ssss == 0
       then return 0
       else fromIntegral <$> decodeInt (fromIntegral ssss)

-- | Apply a quantization matrix to a macroblock
{-# INLINE deQuantize #-}
deQuantize :: MacroBlock Int16 -> MutableMacroBlock s Int16
           -> ST s (MutableMacroBlock s Int16)
deQuantize table block = update 0
    where update 64 = return block
          update i = do
              val <- block `M.unsafeRead` i
              let finalValue = val * (table `VS.unsafeIndex` i)
              (block `M.unsafeWrite` i) finalValue
              update $ i + 1

inverseDirectCosineTransform :: MutableMacroBlock s Int16
                             -> ST s (MutableMacroBlock s Int16)
inverseDirectCosineTransform mBlock =
    fastIdct mBlock >>= mutableLevelShift

zigZagOrder :: MacroBlock Int
zigZagOrder = makeMacroBlock $ concat
    [[ 0, 1, 5, 6,14,15,27,28]
    ,[ 2, 4, 7,13,16,26,29,42]
    ,[ 3, 8,12,17,25,30,41,43]
    ,[ 9,11,18,24,31,40,44,53]
    ,[10,19,23,32,39,45,52,54]
    ,[20,22,33,38,46,51,55,60]
    ,[21,34,37,47,50,56,59,61]
    ,[35,36,48,49,57,58,62,63]
    ]

zigZagReorderForwardv :: (Storable a, Num a) => VS.Vector a -> VS.Vector a
zigZagReorderForwardv vec = runST $ do
    v <- M.new 64
    mv <- VS.thaw vec
    zigZagReorderForward v mv >>= VS.freeze

zigZagOrderForward :: MacroBlock Int
zigZagOrderForward = VS.generate 64 inv
  where inv i = fromMaybe 0 $ VS.findIndex (i ==) zigZagOrder

zigZagReorderForward :: (Storable a)
                     => MutableMacroBlock s a
                     -> MutableMacroBlock s a
                     -> ST s (MutableMacroBlock s a)
{-# SPECIALIZE INLINE zigZagReorderForward :: MutableMacroBlock s Int32
                                           -> MutableMacroBlock s Int32
                                           -> ST s (MutableMacroBlock s Int32) #-}
{-# SPECIALIZE INLINE zigZagReorderForward :: MutableMacroBlock s Int16
                                           -> MutableMacroBlock s Int16
                                           -> ST s (MutableMacroBlock s Int16) #-}
{-# SPECIALIZE INLINE zigZagReorderForward :: MutableMacroBlock s Word8
                                           -> MutableMacroBlock s Word8
                                           -> ST s (MutableMacroBlock s Word8) #-}
zigZagReorderForward zigzaged block = ordering zigZagOrderForward >> return zigzaged
  where ordering !table = reorder (0 :: Int)
          where reorder !i | i >= 64 = return ()
                reorder i  = do
                     let idx = table `VS.unsafeIndex` i
                     v <- block `M.unsafeRead` idx
                     (zigzaged `M.unsafeWrite` i) v
                     reorder (i + 1)

zigZagReorder :: MutableMacroBlock s Int16 -> MutableMacroBlock s Int16
              -> ST s (MutableMacroBlock s Int16)
zigZagReorder zigzaged block = do
    let update i =  do
            let idx = zigZagOrder `VS.unsafeIndex` i
            v <- block `M.unsafeRead` idx
            (zigzaged `M.unsafeWrite` i) v

        reorder 63 = update 63
        reorder i  = update i >> reorder (i + 1)

    reorder (0 :: Int)
    return zigzaged

-- | Unpack an int of the given size encoded from MSB to LSB.
unpackInt :: Int -> BoolReader s Int32
unpackInt = getNextIntJpg

{-# INLINE rasterMap #-}
rasterMap :: (Monad m)
          => Int -> Int -> (Int -> Int -> m ())
          -> m ()
rasterMap width height f = liner 0
  where liner y | y >= height = return ()
        liner y = columner 0
          where columner x | x >= width = liner (y + 1)
                columner x = f x y >> columner (x + 1)

pixelClamp :: Int16 -> Word8
pixelClamp n = fromIntegral . min 255 $ max 0 n

-- | Given a size coefficient (how much a pixel span horizontally
-- and vertically), the position of the macroblock, return a list
-- of indices and value to be stored in an array (like the final
-- image)
unpackMacroBlock :: Int    -- ^ Component count
                 -> Int -- ^ Width coefficient
                 -> Int -- ^ Height coefficient
                 -> Int -- ^ Component index
                 -> Int -- ^ x
                 -> Int -- ^ y
                 -> MutableImage s PixelYCbCr8
                 -> MutableMacroBlock s Int16
                 -> ST s ()
unpackMacroBlock compCount wCoeff hCoeff compIdx x y
                 (MutableImage { mutableImageWidth = imgWidth,
                                 mutableImageHeight = imgHeight, mutableImageData = img })
                 block = rasterMap dctBlockSize dctBlockSize unpacker
  where unpacker i j = do
          let yBase = y * dctBlockSize + j * hCoeff
          compVal <- pixelClamp <$> (block `M.unsafeRead` (i + j * dctBlockSize))
          rasterMap wCoeff hCoeff $ \wDup hDup -> do
             let xBase = x * dctBlockSize + i * wCoeff
                 xPos = xBase + wDup
                 yPos = yBase + hDup

             when (xPos < imgWidth && yPos < imgHeight)
                  (do let mutableIdx = (xPos + yPos * imgWidth) * compCount + compIdx
                      (img `M.unsafeWrite` mutableIdx) compVal)

-- | This is one of the most important function of the decoding,
-- it form the barebone decoding pipeline for macroblock. It's all
-- there is to know for macro block transformation
decodeMacroBlock :: MacroBlock DctCoefficients
                 -> MutableMacroBlock s Int16
                 -> MutableMacroBlock s Int16
                 -> ST s (MutableMacroBlock s Int16)
decodeMacroBlock quantizationTable zigZagBlock block =
    deQuantize quantizationTable block >>= zigZagReorder zigZagBlock
                                       >>= inverseDirectCosineTransform

