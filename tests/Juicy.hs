{-# LANGUAGE Rank2Types #-}
module Main where

import Control.Monad( when )
import Data.Word
import qualified Control.Monad.Trans.State.Strict as S
import System.Exit( exitSuccess, exitFailure )
import Test.HUnit

import Codec.Picture.BitWriter
import qualified Data.ByteString as B

testBoolWriter :: (forall s. BoolWriter s b) -> [Word8]
testBoolWriter = B.unpack . runBoolWriter

writerTest :: Test
writerTest = test
    [ "Single 1"    ~: [0x80] ~=? testBoolWriter (writeBits 1 1)
    , "Single 0"    ~: [0x00] ~=? testBoolWriter (writeBits 0 1)
    , "Multi 1"     ~: [0xF0] ~=? testBoolWriter (writeBits 0xF 4)
    , "Multi 1010"  ~: [0xA0] ~=? testBoolWriter (writeBits 0xA 4)
    , "8 bits"      ~: [0xFF] ~=? testBoolWriter (writeBits 0xFF 8)
    , "0 bits"      ~: []     ~=? testBoolWriter (writeBits 0xFF 0)
    , "8 bits + 2"
        ~: [0xFF, 0x40] ~=? testBoolWriter (writeBits 0xFF 8 >> writeBits 01 2)

    , "16 bits"     ~: [0xDE, 0xAD] ~=? testBoolWriter (writeBits 0xDEAD 16)

    , "Simple combination" 
        ~: [0xF8] ~=? testBoolWriter (writeBits 0xF 4 >> writeBits 1 1)

    , "Across"
        ~: [0x9B, 0xEE, 0xF0] ~=? testBoolWriter
                                      (writeBits 4 3
                                    >> writeBits 1 1
                                    >> writeBits 0xBEEF 16)
    ]

main :: IO ()
main = do
    rez <- runTestTT writerTest

    when (errors rez + failures rez > 0)
          exitFailure

    exitSuccess

