{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Module used by the jpeg decoder internally, shouldn't be used
-- in user code.
module Codec.Picture.Jpg.DefaultTable( DctComponent( .. )
									 , HuffmanTree( .. )
									 , MacroBlock
									 , makeMacroBlock
									 , buildHuffmanTree
									 {-  
									 , defaultChromaQuantizationTable
									 , defaultLumaQuantizationTable
									 , defaultAcChromaHuffmanTable
									 , defaultAcLumaHuffmanTable 
									 , defaultDcChromaHuffmanTable
									 , defaultDcLumaHuffmanTable
                                     -}
									 ) where

import Foreign.Storable ( Storable )
import qualified Data.Vector.Storable as V
import Data.Word( Word8 )
import Data.List( foldl' )

-- | Tree storing the code used for huffman encoding.
data HuffmanTree = Branch HuffmanTree HuffmanTree -- ^ If bit is 0 take the first subtree, if 1, the right.
                 | Leaf Word8       -- ^ We should output the value
                 | Empty            -- ^ no value present
                 deriving (Eq, Show)

-- | Represent a compact array of 8 * 8 values. The size
-- is not guarenteed by type system, but if makeMacroBlock is
-- used, everything should be fine size-wise
type MacroBlock a = V.Vector a

-- | Helper function to create pure macro block of the good size.
makeMacroBlock :: (Storable a) => [a] -> MacroBlock a
makeMacroBlock = V.fromListN 64

-- | Enumeration used to search in the tables for different components.
data DctComponent = DcComponent | AcComponent
    deriving (Eq, Show)

-- | Transform parsed coefficients from the jpeg header to a
-- tree which can be used to decode data.
buildHuffmanTree :: [[Word8]] -> HuffmanTree
buildHuffmanTree table = foldl' insertHuffmanVal Empty
                       . concatMap (\(i, t) -> map (i + 1,) t)
                       $ zip ([0..] :: [Int]) table
  where isTreeFullyDefined Empty = False
        isTreeFullyDefined (Leaf _) = True
        isTreeFullyDefined (Branch l r) = isTreeFullyDefined l && isTreeFullyDefined r

        insertHuffmanVal Empty (0, val) = Leaf val
        insertHuffmanVal Empty (d, val) = Branch (insertHuffmanVal Empty (d - 1, val)) Empty
        insertHuffmanVal (Branch l r) (d, val)
            | isTreeFullyDefined l = Branch l (insertHuffmanVal r (d - 1, val))
            | otherwise            = Branch (insertHuffmanVal l (d - 1, val)) r
        insertHuffmanVal (Leaf _) _ = error "Inserting in value, shouldn't happen"

{- 
defaultLumaQuantizationTable :: MacroBlock Int16
defaultLumaQuantizationTable = makeMacroBlock
    [16, 11, 10, 16,  24,  40,  51,  61
    ,12, 12, 14, 19,  26,  58,  60,  55
    ,14, 13, 16, 24,  40,  57,  69,  56
    ,14, 17, 22, 29,  51,  87,  80,  62
    ,18, 22, 37, 56,  68, 109, 103,  77
    ,24, 35, 55, 64,  81, 104, 113,  92
    ,49, 64, 78, 87, 103, 121, 120, 101
    ,72, 92, 95, 98, 112, 100, 103,  99
    ]

defaultChromaQuantizationTable :: MacroBlock Int16
defaultChromaQuantizationTable = makeMacroBlock
    [17, 18, 24, 47, 99, 99, 99, 99
    ,18, 21, 26, 66, 99, 99, 99, 99
    ,24, 26, 56, 99, 99, 99, 99, 99
    ,47, 66, 99, 99, 99, 99, 99, 99
    ,99, 99, 99, 99, 99, 99, 99, 99
    ,99, 99, 99, 99, 99, 99, 99, 99
    ,99, 99, 99, 99, 99, 99, 99, 99
    ,99, 99, 99, 99, 99, 99, 99, 99
    ]
-- | From the Table K.3 of ITU-81 (p153)
defaultDcLumaHuffmanTable :: HuffmanTree
defaultDcLumaHuffmanTable = buildHuffmanTree
    [ []
    , [0]
    , [1, 2, 3, 4, 5]
    , [6]
    , [7]
    , [8]
    , [9]
    , [10]
    , [11]
    , []
    , []
    , []
    , []
    , []
    , []
    , []
    ]

-- | From the Table K.4 of ITU-81 (p153)
defaultDcChromaHuffmanTable :: HuffmanTree
defaultDcChromaHuffmanTable = buildHuffmanTree
    [ []
    , [0, 1, 2]
    , [3]
    , [4]
    , [5]
    , [6]
    , [7]
    , [8]
    , [9]
    , [10]
    , [11]
    , []
    , []
    , []
    , []
    , []
    ]

-- | From the Table K.5 of ITU-81 (p154)
defaultAcLumaHuffmanTable :: HuffmanTree
defaultAcLumaHuffmanTable = buildHuffmanTree
    [ []
    , [0x01, 0x02]
    , [0x03]
    , [0x00, 0x04, 0x11]
    , [0x05, 0x12, 0x21]
    , [0x31, 0x41]
    , [0x06, 0x13, 0x51, 0x61]
    , [0x07, 0x22, 0x71]
    , [0x14, 0x32, 0x81, 0x91, 0xA1]
    , [0x08, 0x23, 0x42, 0xB1, 0xC1]
    , [0x15, 0x52, 0xD1, 0xF0]
    , [0x24, 0x33, 0x62, 0x72]
    , []
    , []
    , [0x82]
    , [0x09, 0x0A, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x34, 0x35
      ,0x36, 0x37, 0x38, 0x39, 0x3A, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4A, 0x53, 0x54
      ,0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x73
      ,0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A
      ,0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7
      ,0xA8, 0xA9, 0xAA, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xC2, 0xC3, 0xC4
      ,0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA
      ,0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5
      ,0xF6, 0xF7, 0xF8, 0xF9, 0xFA]
    ]

defaultAcChromaHuffmanTable :: HuffmanTree
defaultAcChromaHuffmanTable = buildHuffmanTree
    [ []
    , [0x00, 0x01]
    , [0x02]
    , [0x03, 0x11]
    , [0x04, 0x05, 0x21, 0x31]
    , [0x06, 0x12, 0x41, 0x51]
    , [0x07, 0x61, 0x71]
    , [0x13, 0x22, 0x32, 0x81]
    , [0x08, 0x14, 0x42, 0x91, 0xA1, 0xB1, 0xC1]
    , [0x09, 0x23, 0x33, 0x52, 0xF0]
    , [0x15, 0x62, 0x72, 0xD1]
    , [0x0A, 0x16, 0x24, 0x34]
    , []
    , [0xE1]
    , [0x25, 0xF1]
    , [ 0x17, 0x18, 0x19, 0x1A, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x35
      , 0x36, 0x37, 0x38, 0x39, 0x3A, 0x43, 0x44, 0x45, 0x46, 0x47
      , 0x48, 0x49, 0x4A, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59
      , 0x5A, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x73
      , 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x82, 0x83, 0x84
      , 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x92, 0x93, 0x94, 0x95
      , 0x96, 0x97, 0x98, 0x99, 0x9A, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6
      , 0xA7, 0xA8, 0xA9, 0xAA, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7
      , 0xB8, 0xB9, 0xBA, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8
      , 0xC9, 0xCA, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9
      , 0xDA, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA
      , 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA
      ]
    ]
-}
