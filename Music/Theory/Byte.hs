-- | Byte functions.
module Music.Theory.Byte where

import qualified Data.Char {- base -}
import qualified Data.Maybe {- base -}
import qualified Data.Word {- base -}
import qualified Numeric {- base -}

import qualified Data.ByteString as ByteString {- bytestring -}
import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.Math.Convert as Convert {- hmt-base -}
import qualified Music.Theory.Read as Read {- hmt-base -}

-- * Enumerations & Char

-- | U8 to Enum.
word8_to_enum :: Enum e => Data.Word.Word8 -> e
word8_to_enum = toEnum . Convert.word8_to_int

-- | Enum to U8.
enum_to_word8 :: Enum e => e -> Maybe Data.Word.Word8
enum_to_word8 = Convert.int_to_word8_maybe . fromEnum

{- | Type-specialised 'toEnum'

>>> map word8_to_char [60,62]
"<>"
-}
word8_to_char :: Data.Word.Word8 -> Char
word8_to_char = word8_to_enum

-- | Type-specialised 'fromEnum'
char_to_word8 :: Char -> Data.Word.Word8
char_to_word8 = Convert.int_to_word8 . fromEnum

-- | Type-specialised 'fromEnum'
char_to_word32 :: Char -> Data.Word.Word32
char_to_word32 = Convert.int_to_word32 . fromEnum

-- | Type-specialised 'Data.Char.digitToInt'
digit_to_word8 :: Char -> Data.Word.Word8
digit_to_word8 = Convert.int_to_word8 . Data.Char.digitToInt

-- | Type-specialised 'Data.Char.intToDigit'.
word8_to_digit :: Data.Word.Word8 -> Char
word8_to_digit = Data.Char.intToDigit . Convert.word8_to_int

-- * Indexing

-- | Type-specialised '!!'
word8_at :: [t] -> Data.Word.Word8 -> t
word8_at l = (!!) l . Convert.word8_to_int

-- * Text

{- | Given /n/ in (0,255) make two character hex string.

>>> mapMaybe byte_hex_pp [0x0F,0xF0,0xF0F]
["0F","F0"]
-}
byte_hex_pp :: (Integral i, Show i) => i -> Maybe String
byte_hex_pp n =
  case Numeric.showHex n "" of
    [c] -> Just ['0', Data.Char.toUpper c]
    [c, d] -> Just (map Data.Char.toUpper [c, d])
    _ -> Nothing

-- | Erroring variant.
byte_hex_pp_err :: (Integral i, Show i) => i -> String
byte_hex_pp_err = Data.Maybe.fromMaybe (error "byte_hex_pp") . byte_hex_pp

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
    [_, _] -> Read.reads_to_read_precise Numeric.readHex s
    _ -> Nothing

-- | Erroring variant.
read_hex_byte_err :: (Eq t, Integral t) => String -> t
read_hex_byte_err = Data.Maybe.fromMaybe (error "read_hex_byte") . read_hex_byte

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
read_hex_byte_seq_ws = read_hex_byte_seq . filter (not . Data.Char.isSpace)

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
let fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7/rom/DX7-ROM1A.syx"
b <- load_byte_seq fn :: IO [Word8]
let e = ByteString.unpack (Base64.encode (ByteString.pack b))
let r = ByteString.unpack (Base64.decodeLenient (ByteString.pack e))
(length b,length e,length r,b == r) == (4104,5472,4104,True)
map word8_to_char e

-}
