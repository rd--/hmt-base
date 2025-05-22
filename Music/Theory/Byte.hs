-- | Byte functions.
module Music.Theory.Byte where

import Control.Monad.ST {- base -}
import Data.Char {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}
import Numeric {- base -}

import Data.Array.ST {- array -}
import Data.Array.Unsafe {- array -}

import qualified Data.ByteString as ByteString {- bytestring -}
import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.Math.Convert as Convert {- hmt-base -}
import qualified Music.Theory.Read as Read {- hmt-base -}

-- * Enumerations & Char

-- | U8 to Enum.
word8_to_enum :: Enum e => Word8 -> e
word8_to_enum = toEnum . Convert.word8_to_int

-- | Enum to U8.
enum_to_word8 :: Enum e => e -> Maybe Word8
enum_to_word8 = Convert.int_to_word8_maybe . fromEnum

{- | Type-specialised 'toEnum'

>>> map word8_to_char [60,62]
"<>"
-}
word8_to_char :: Word8 -> Char
word8_to_char = word8_to_enum

-- | Type-specialised 'fromEnum'
char_to_word8 :: Char -> Word8
char_to_word8 = Convert.int_to_word8 . fromEnum

-- | Type-specialised 'digitToInt'
digit_to_word8 :: Char -> Word8
digit_to_word8 = Convert.int_to_word8 . digitToInt

-- | Type-specialised 'intToDigit'.
word8_to_digit :: Word8 -> Char
word8_to_digit = intToDigit . Convert.word8_to_int

-- * Indexing

-- | Type-specialised '!!'
word8_at :: [t] -> Word8 -> t
word8_at l = (!!) l . Convert.word8_to_int

-- * Text

{- | Given /n/ in (0,255) make two character hex string.

>>> mapMaybe byte_hex_pp [0x0F,0xF0,0xF0F]
["0F","F0"]
-}
byte_hex_pp :: (Integral i, Show i) => i -> Maybe String
byte_hex_pp n =
  case showHex n "" of
    [c] -> Just ['0', toUpper c]
    [c, d] -> Just (map toUpper [c, d])
    _ -> Nothing

-- | Erroring variant.
byte_hex_pp_err :: (Integral i, Show i) => i -> String
byte_hex_pp_err = fromMaybe (error "byte_hex_pp") . byte_hex_pp

{- | 'byte_hex_pp_err' either plain (ws = False) or with spaces (ws = True).
  Plain is the same format written by xxd -p and read by xxd -r -p.

>>> byte_seq_hex_pp True [0x0F,0xF0]
"0F F0"
-}
byte_seq_hex_pp :: (Integral i, Show i) => Bool -> [i] -> String
byte_seq_hex_pp ws = (if ws then unwords else concat) . map byte_hex_pp_err

{- | Read two character hexadecimal string.

>>> mapMaybe read_hex_byte (Split.chunksOf 2 "0FF0F") == [0x0F,0xF0]
True
-}
read_hex_byte :: (Eq t, Integral t) => String -> Maybe t
read_hex_byte s =
  case s of
    [_, _] -> Read.reads_to_read_precise readHex s
    _ -> Nothing

-- | Erroring variant.
read_hex_byte_err :: (Eq t, Integral t) => String -> t
read_hex_byte_err = fromMaybe (error "read_hex_byte") . read_hex_byte

{- | Sequence of 'read_hex_byte_err'

>>> read_hex_byte_seq "000FF0FF" == [0x00,0x0F,0xF0,0xFF]
True
-}
read_hex_byte_seq :: (Eq t, Integral t) => String -> [t]
read_hex_byte_seq = map read_hex_byte_err . Split.chunksOf 2

{- | Variant that filters white space.

>>> read_hex_byte_seq_ws "00 0F F0 FF" == [0x00,0x0F,0xF0,0xFF]
True
-}
read_hex_byte_seq_ws :: (Eq t, Integral t) => String -> [t]
read_hex_byte_seq_ws = read_hex_byte_seq . filter (not . isSpace)

-- * IO

-- | Load binary 'U8' sequence from file.
load_byte_seq :: Integral i => FilePath -> IO [i]
load_byte_seq = fmap (map fromIntegral . ByteString.unpack) . ByteString.readFile

-- | Store binary 'U8' sequence to file.
store_byte_seq :: Integral i => FilePath -> [i] -> IO ()
store_byte_seq fn = ByteString.writeFile fn . ByteString.pack . map fromIntegral

-- | Load hexadecimal text 'U8' sequences from file.
load_hex_byte_seq :: Integral i => FilePath -> IO [[i]]
load_hex_byte_seq = fmap (map read_hex_byte_seq . lines) . readFile

-- | Store 'U8' sequences as hexadecimal text, one sequence per line.
store_hex_byte_seq :: (Integral i, Show i) => FilePath -> [[i]] -> IO ()
store_hex_byte_seq fn = writeFile fn . unlines . map (byte_seq_hex_pp False)

{-

import qualified Data.ByteString.Base64 as Base64 {- base64-bytestring -}
let fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7/rom/ROM1A.syx"
b <- load_byte_seq fn :: IO [Word8]
let e = ByteString.unpack (Base64.encode (ByteString.pack b))
let r = ByteString.unpack (Base64.decodeLenient (ByteString.pack e))
(length b,length e,length r,b == r) == (4104,5472,4104,True)
map word8_to_char e

-}

-- * Cast

{- | Cast Float to Word32

>>> castFloatToWord32 3.141
1078527525
-}
castFloatToWord32 :: Float -> Word32
castFloatToWord32 d = runST ((flip readArray 0 =<< castSTUArray =<< newArray (0, 0 :: Int) d) :: ST s Word32)

{- | Case Word32 to Float

>>> castWord32ToFloat 1078527525
3.141
-}
castWord32ToFloat :: Word32 -> Float
castWord32ToFloat d = runST ((flip readArray 0 =<< castSTUArray =<< newArray (0, 0 :: Int) d) :: ST s Float)

{- | Cast Double to Word64

>>> castDoubleToWord64 3.141
4614255322014802772
-}
castDoubleToWord64 :: Double -> Word64
castDoubleToWord64 d = runST ((flip readArray 0 =<< castSTUArray =<< newArray (0, 0 :: Int) d) :: ST s Word64)

{- | Case Word64 to Double

>>> castWord64ToDouble 4614255322014802772
3.141
-}
castWord64ToDouble :: Word64 -> Double
castWord64ToDouble d = runST ((flip readArray 0 =<< castSTUArray =<< newArray (0, 0 :: Int) d) :: ST s Double)
