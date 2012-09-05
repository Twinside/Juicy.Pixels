module Codec.Picture.Gif.LZW( lzw ) where

import Data.Word( Word8, Word16 )
import Control.Applicative( pure, (<$>), (<*>) )

import Control.Monad.ST( ST )
import Foreign.Storable ( Storable )
import Control.Monad.Primitive ( PrimState, PrimMonad )

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Storable.Mutable as M

import Codec.Picture.BitWriter

--------------------------------------------------
----            My LZW
--------------------------------------------------
{-# INLINE (!!!) #-}
(!!!) :: (V.Unbox e) => V.Vector e -> Int -> e
(!!!) = (V.!) 
        -- V.unsafeIndex

{-# INLINE (.!!!.) #-}
(.!!!.) :: (PrimMonad m, Storable a)
        => M.STVector (PrimState m) a -> Int -> m a
(.!!!.) = M.read
          -- M.unsafeRead

{-# INLINE (.<-.) #-}
(.<-.) :: (PrimMonad m, Storable a)
       => M.STVector (PrimState m) a -> Int -> a -> m ()
(.<-.) = M.write 
         -- M.unsafeWrite

data LZWEntry = LZWEntry {-# UNPACK #-} !Word16
                         {-# UNPACK #-} !Int

data LZWContext s = LZWContext
    { lzwTable   :: M.STVector s Int
    , lzwData    :: M.STVector s Word8
    , lzwMaxCode :: {-# UNPACK #-} !Int
    }

duplicateData :: (PrimMonad m, Storable a)
              => M.STVector (PrimState m) a -> M.STVector (PrimState m) a
              -> Int -> Int -> Int -> m ()
duplicateData src dest sourceIndex size destIndex = aux size
  where aux 0 = return ()
        aux n = do
          src .!!!. (sourceIndex + n) >>= (dest .<-. (destIndex + n))
          aux (n - 1)

rangeSetter :: (PrimMonad m, Storable a, Num a)
            => Int -> M.STVector (PrimState m) a
            -> m (M.STVector (PrimState m) a)
rangeSetter count vec = aux 0
  where aux n | n == count = return vec
        aux n = (vec .<-. n) (fromIntegral n) >> aux (n + 1)

-- | Gif image constraint from spec-gif89a, code size max : 12 bits.
lzw :: Int -> Int -> BoolReader s (V.Vector Word8)
lzw nMaxBitKeySize initialKeySize = fail ""
  where tableEntryCount =  2 ^ min 12 nMaxBitKeySize
        maxDataSize = tableEntryCount `div` 2 * (1 + tableEntryCount)
        initialElementCount = 2 ^ min 12 initialKeySize

        -- Allocate buffer of maximum size.
        initialContext = 
            LZWContext <$> (M.new tableEntryCount >>= rangeSetter initialElementCount)
                       <*> (M.new maxDataSize >>= rangeSetter initialElementCount)
                       <*> (pure initialElementCount )

--------------------------------------------------
----            Meh
--------------------------------------------------
{-  
data DecodeState = DecodeState
     { stringTable     :: Map Int [Int]
     , compressionSize :: Int
     , oldCode         :: [Int]
     , currentIndex    :: Int
     } deriving (Show)

type Decoder a = StateT DecodeState BitGet a

runDecoder :: B.ByteString   -- ^ Input BS
           -> DecodeState     -- ^ Initial state
           -> Decoder a       -- ^ Our decoder
           -> a               -- ^ Result
runDecoder bs initial decoder =
    let x = evalStateT decoder initial -- x = BitGet a
    in case runBitGet (reverseBytes bs) x of
        Left err -> error err
        Right x  -> x

getNextCode :: Decoder Int
getNextCode = do
  cs <- gets compressionSize
  bs <- lift $ getLeftByteString cs
  return $ (fromIntegral . toWord) bs

getCurrentIndex :: Decoder Int
getCurrentIndex = gets currentIndex

lzwDecode rootSize string =
  let initial = DecodeState
                { stringTable = fromList $ map (\x -> (x, [x])) [0 .. (2 ^ rootSize - 1)]
                , currentIndex = 2 ^ rootSize + 1
                , compressionSize = rootSize + 1
                , oldCode = []
                }
  in runDecoder string initial (decodeS initial)


decodeS :: DecodeState -> Decoder [Int]
decodeS initial = decode
  where
    decode = do
      code <- getNextCode
      evalCode code

    clearCode = 2 ^ (compressionSize initial - 1)
    endOfInfo = clearCode + 1

    reset = put initial

    lookupCode code = do
      res <- lookup code `liftM` gets stringTable
      prev <- gets oldCode
      add $ prev ++ [head $ fromMaybe prev res]
      return $ fromMaybe (prev ++ [head prev]) res

    adjustDS r = modify func
      where
        func ds = ds { currentIndex = (currentIndex ds) + 1
                     , compressionSize = comp ds
                     , oldCode = r
                     }
        comp ds = min 12 $ if (currentIndex ds) == (2 ^ compressionSize ds) - 1
                           then compressionSize ds + 1 else compressionSize ds

    add x = modify (\ds -> ds { stringTable = insert (currentIndex ds) x (stringTable ds)})

    evalCode code
      | code == clearCode = reset >> decodeS initial
      | code == endOfInfo = return []
      | otherwise = do
          r <- lookupCode code
          adjustDS r
          (r ++) `liftM` decode

lzwEncode rootSize arr =
    B.pack . mapAdd $ encode rootSize arr
  where
    mapAdd :: [(Int, Int)] -> [Word8]
    mapAdd [] = []
    mapAdd [(c,w)]
      | c <= 8 = [(fromIntegral w)]
      | otherwise = (fromIntegral w) : mapAdd [(c-8, w `shiftR` 8)]
    mapAdd (x@(b, w):y:xs)
      | b == 8 = (fromIntegral w) : (mapAdd (y:xs))
      | otherwise = mapAdd $ (add x y) ++ xs

-- Adds 2 numbers together based on how many bits we're allowed to take
-- this works by returning an array the numbers it produces
--
-- Possibilites:
--   * All bits of the second number are consumed -> discard second number from result
--   * The bits of the produced number are > 8 -> we have a valid Word8 we can return (set bits == 8)
--   * The bits of the left input number > 8   -> take 8 bits (this is a valid Word8)
--                                             -> remaining bits are added as a next input
--                                             -> original left input is just added again
add :: (Int,Int) -> (Int, Int) -> [(Int, Int)]
add (c1, v1) (c2, v2) =
    if c1 > 8 then
      [(8, v1), (c1 - 8, v1 `shiftR` 8), (c2, v2)]
    else
      let leftShifted = v2 `shiftL` c1                      -- the right input shifted the correct amount
          combined    = v1 .|. leftShifted                  -- the 2 numbers added together
          next        = v2 `shiftR` (c2 - bitsLeft)         -- next input (deduced from right input)
          bitsLeft    = c2 - (8 - c1)                       -- remaining bits in the next input
          bits        = if c1 + c2 > 8 then 8 else c1 + c2  -- set to 8 so mapAdd knows it can output a Word8
      in  if bitsLeft <= 0 then [(bits, combined)]          -- discard right input when there are no remaining bits
          else [(bits, combined),(bitsLeft, next)]

-- Convert the given array with given rootSize to a array where each element
-- is of the following form (bits to take, actual element)
encode :: Int -> [Int] -> [(Int,Int)]
encode rootSize arr =
    (compressionSize, clearCode) : encode' arr startStringTable compressionSize [] startingPoint
  where
    startStringTable = fromList $ map (\x -> ([x], x)) [0 .. (2 ^ rootSize + 1)] :: Map [Int] Int
    compressionSize  = rootSize + 1 :: Int
    clearCode        = 2 ^ rootSize :: Int
    endOfInfo        = clearCode + 1
    startingPoint    = endOfInfo + 1

    -- Don't forget to output our last code and the endOfInfo
    encode' [] strTable c last i  = [(c, strTable ! last), (newC, endOfInfo)]
      where
        newC = min 12 $ if i == (2 ^ c)
                        then c + 1 else c
    encode' (x:xs) strTable compSize last currentIndex =
      let string = (last ++ [x])
      in  if string `member` strTable then
            encode' xs strTable compSize string currentIndex
          else
            let newStrTable = insert string currentIndex strTable
                newCompSize = if currentIndex == (2 ^ compSize)
                              then compSize + 1 else compSize
            in  (compSize, strTable ! last) :
                if newCompSize == 13
                then (12, clearCode) : encode' (x:xs) startStringTable compressionSize [] startingPoint
                else encode' xs newStrTable newCompSize [x] (currentIndex + 1)


-- source: http://graphics.stanford.edu/~seander/bithacks.html
reverseBytes :: B.ByteString -> B.ByteString
reverseBytes = B.map (\b ->
  fromIntegral $ (`shiftR` 32)
               $ (* 0x0101010101)
               $ (.&. 0x0884422110)
               $ (* 0x80200802) (fromIntegral b :: Word64))

toWord :: B.ByteString -> Word16
toWord s =
  case B.unpack $ reverseBytes s of
    [small] -> fromIntegral small
    [l,r]   -> r <+> l
    _       -> error "more than 2 bytes should never happen"

(<+>) l r = (fromIntegral l) `shiftL` 8 .|. (fromIntegral r)
 -- -}
