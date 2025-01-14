-- | Json (Javascript Object Notation)
module Music.Theory.Json where

import Data.Maybe {- base -}
import Data.Word {- base -}

import qualified Data.ByteString.Lazy as ByteString {- bytestring -}
import qualified Data.ByteString.Lazy.Char8 as ByteString.Char8 {- bytestring -}
import qualified Data.Map as Map {- containers -}
import qualified Data.Text as Text {- text -}

import qualified Data.Aeson.Micro as Json {- microaeson -}

import qualified Music.Theory.Math.Predicate as Math {- hmt-base -}

-- | Json object.
type Object = Json.Object

-- | Json value.
type Value = Json.Value

-- | Association with string key.
type Association = (String, Value)

{- | Is 53-bit integral

>>> isSafeIntegral (maxBound :: Int)
False
-}
isSafeIntegral :: Integral i => i -> Bool
isSafeIntegral i = i >= -9007199254740991 && i <= 9007199254740991

-- * Encoding

-- | 'Json.encode'
encode_value :: Value -> ByteString.ByteString
encode_value = Json.encode

-- | 'Json.encode' to String
encode_value_str :: Value -> String
encode_value_str = ByteString.Char8.unpack . encode_value

-- | Encode integral (unsafe)
encode_integral :: Integral n => n -> Value
encode_integral = Json.Number . fromIntegral

-- | Encode floating
encode_floating :: Real n => n -> Value
encode_floating = Json.Number . realToFrac

-- | Encode boolean.
boolean :: Bool -> Value
boolean = Json.Bool

-- | Encode string.
string :: String -> Value
string = Json.String . Text.pack

-- | Encode integer if safe, else Nothing.
safeIntegral :: Integral i => i -> Maybe Value
safeIntegral i =
  if isSafeIntegral i
    then Just (Json.Number (fromIntegral i))
    else Nothing

-- | Encode integer if safe, else error.
unsafeIntegral :: Integral i => i -> Value
unsafeIntegral = fromMaybe (error "Json.unsafeIntegral") . safeIntegral

-- | Encode Int if safe, else Nothing.
safeInt :: Int -> Maybe Value
safeInt = safeIntegral

-- | Encode Int if safe, else error.
int :: Int -> Value
int = unsafeIntegral

-- | Encode Integer if safe, else Nothing.
safeInteger :: Integer -> Maybe Value
safeInteger = safeIntegral

-- | Encode Integer if safe, else error.
integer :: Integer -> Value
integer = unsafeIntegral

-- | Encode double.
double :: Double -> Value
double = Json.Number

-- | Partially encode association.
association :: Association -> Json.Pair
association (k, v) = (Text.pack k, v)

-- | Encode association list as object.
object :: [Association] -> Value
object = Json.object . map association

-- | Encode array.
array :: [Value] -> Value
array = Json.Array

-- | Write file.
writeFile :: FilePath -> Value -> IO ()
writeFile fn json = ByteString.writeFile fn (Json.encode json)

-- * Decode

-- | Erroring 'Json.decode'
decode_value_err :: ByteString.ByteString -> Value
decode_value_err = fromMaybe (error "decode_value") . Json.decode

{- | Decode string or error.

>>> decode_str "1"
Number 1.0

>>> decode_str "1.5"
Number 1.5

>>> decode_str "\"str\""
String "str"

>>> decode_str "[0,1]"
Array [Number 0.0,Number 1.0]

>>> decode_str "{\"blob\":[0,1]}"
Object (fromList [("blob",Array [Number 0.0,Number 1.0])])
-}
decode_str :: String -> Value
decode_str = decode_value_err . ByteString.Char8.pack

object_lookup :: String -> Object -> Maybe Value
object_lookup k = Map.lookup (Text.pack k)

object_lookup_err :: String -> Object -> Value
object_lookup_err k o =
  let err = error ("object_lookup: " ++ k ++ " -- " ++ show o)
  in fromMaybe err (object_lookup k o)

-- | Require value to be an object, unpack as a list of associations.
associations :: Value -> [Association]
associations v =
  case v of
    Json.Object x -> map (\(i, j) -> (Text.unpack i, j)) (Map.toList x)
    _ -> error "associations?"

{- | Require value to be an object, the entries of which can be parsed by _f_.

>>> value_to_assoc_list value_to_int_list (decode_str "{\"x\": [1, 2], \"y\": [3, 4]}")
[("x",[1,2]),("y",[3,4])]
-}
value_to_assoc_list :: (Value -> t) -> Value -> [(String, t)]
value_to_assoc_list f = map (\(i, j) -> (i, f j)) .associations

{- | Require value be a number and read as Double.

>>> value_to_double_err (encode_floating 3.141)
3.141

>>> value_to_double_err (encode_integral 42)
42.0
-}
value_to_double_err :: Value -> Double
value_to_double_err v =
  case v of
    Json.Number x -> x
    _ -> error "value_to_double?"

{- | Require value be a number and read as Int.

>>> value_to_int_err (encode_integral 42)
42

> value_to_int_err (encode_floating 3.141) -- error
-}
value_to_int_err :: Value -> Int
value_to_int_err v =
  case v of
    Json.Number x -> let (i, f) = properFraction x in if f == 0 then i else error "value_to_int_err: float?"
    _ -> error "value_to_int?"

-- | Require value be a number and read as Word8.
value_to_word8_err :: Value -> Word8
value_to_word8_err v =
  case v of
    Json.Number x -> if Math.double_is_word8 x then floor x else error "value_to_word8?"
    _ -> error "value_to_word8?"

value_to_list_err :: Value -> [Value]
value_to_list_err v =
  case v of
    Json.Array x -> x
    _ -> error "value_to_list?"

value_to_int_list :: Value -> [Int]
value_to_int_list = map value_to_int_err . value_to_list_err

value_to_int_list_list :: Value -> [[Int]]
value_to_int_list_list = map value_to_int_list . value_to_list_err

value_to_string_list :: Value -> [String]
value_to_string_list = map value_to_string_err . value_to_list_err

value_to_object_err :: Value -> Object
value_to_object_err j =
  case j of
    Json.Object o -> o
    _ -> error "value_to_object?"

value_to_string_err :: Value -> String
value_to_string_err j =
  case j of
    Json.String x -> Text.unpack x
    _ -> error "value_to_string?"

value_to_bytestring_err :: Value -> ByteString.ByteString
value_to_bytestring_err v =
  case v of
    Json.Array x -> ByteString.pack (map value_to_word8_err x)
    _ -> error "value_to_double?"

readFile :: FilePath -> IO Value
readFile fn = fmap (fromMaybe (error "decode_value") . Json.decode) (ByteString.readFile fn)
