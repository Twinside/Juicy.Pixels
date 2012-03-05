{-# LANGUAGE Rank2Types #-}
module Main where

import Data.Int( Int16 )
import Control.Monad( when )
import Control.Monad.ST
import Data.Word
import qualified Control.Monad.Trans.State.Strict as S
import System.Exit( exitSuccess, exitFailure )
import Test.HUnit
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import Text.Printf
import Debug.Trace

import Codec.Picture.Jpg.FastDct( fastDct, slowFdct )
import Codec.Picture.Jpg.FastIdct
import Codec.Picture.Jpg.DefaultTable
import Codec.Picture.BitWriter
import qualified Data.ByteString as B
import Codec.Picture.Jpg.Types

testBoolWriter :: (forall s. BoolWriter s b) -> [Word8]
testBoolWriter = B.unpack . runBoolWriter

writerTest :: Test
writerTest = test
    [ "Single 1"    ~: [0x80] ~=? testBoolWriter (writeBits 1 1)
    , "Single 0"    ~: [0x00] ~=? testBoolWriter (writeBits 0 1)
    , "Multi 1"     ~: [0xF0] ~=? testBoolWriter (writeBits 0xF 4)
    , "Multi 1010"  ~: [0xA0] ~=? testBoolWriter (writeBits 0xA 4)
    , "8 bits"      ~: [0xFF, 0] ~=? testBoolWriter (writeBits 0xFF 8)
    , "0 bits"      ~: []     ~=? testBoolWriter (writeBits 0xFF 0)
    , "8 bits + 2"
        ~: [0xFF, 0x00, 0x40] ~=? testBoolWriter (writeBits 0xFF 8 >> writeBits 01 2)

    , "16 bits"     ~: [0xDE, 0xAD] ~=? testBoolWriter (writeBits 0xDEAD 16)

    , "Simple combination" 
        ~: [0xF8] ~=? testBoolWriter (writeBits 0xF 4 >> writeBits 1 1)

    , "Across"
        ~: [0x9B, 0xEE, 0xF0] ~=? testBoolWriter
                                      (writeBits 4 3
                                    >> writeBits 1 1
                                    >> writeBits 0xBEEF 16)
    , "Real case"
        ~: [0xFE, 0xE2, 0x80] ~=? testBoolWriter
                            (writeBits 0xFE 8 >> writeBits 0x38A 10)
    ]

sampleMacroBlock :: MacroBlock Int16
sampleMacroBlock = makeMacroBlock
 [233,  234,  234,  234,  234,  232,  233,  233,
  233,  233,  231,  230,  229,  228,  230,  230,
  234,  231,  225,  207,  206,  205,  206,  214,
  233,  229,  176,  177,  178,  177,  179,  175,
  234,  227,  174,  181,  179,  176,  174,  176,
  234,  226,  180,  181,  218,  222,  224,  224,
  232,  227,  180,  176,  222,  229,  230,  232,
  231,  229,  175,  176,  207,  232,  230,  227]

{-dctIdct :: MacroBlock Int16 -> MacroBlock Int16-}
dctIdct blk = runST $ do
    let shifted = V.map (\a -> a - 128) blk
    mblk <- trace (printvMacroBlock blk) $ V.thaw shifted
    work <- createEmptyMutableMacroBlock 
    dctized <- trace (printvMacroBlock shifted) $ slowFdct work mblk
    
    strv <- printMacroBlock dctized
    u32dct <- trace strv $ V.freeze dctized
    u16dct <- V.thaw $ V.map fromIntegral u32dct
    strv' <- printMacroBlock dctized

    udctized <- trace strv' $ fastIdct u16dct
    frozen <- V.freeze udctized
    return . trace (printvMacroBlock frozen) $ ("", frozen)

printvMacroBlock :: MacroBlock Int16 -> String
printvMacroBlock block = pLn 0
    where pLn 64 = "===============================\n"
          pLn i =
              printf (if i `mod` 8 == 0 then "\n%5d "
                                       else "%5d ") (block !!! i) ++ pLn (i+1)

main :: IO ()
main = do
    let (dumb, inv) = dctIdct sampleMacroBlock
    putStrLn dumb
    rez <- runTestTT writerTest

    exitFailure
    when (errors rez + failures rez > 0)
          exitFailure

    exitSuccess

