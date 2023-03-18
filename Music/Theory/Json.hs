module Music.Theory.Json where

import Data.Maybe {- base -}

import qualified Data.ByteString.Lazy as ByteString {- bytestring -}
import qualified Data.Text as Text {- text -}

import qualified Data.Aeson.Micro as Json {- microaeson -}

type Value = Json.Value

boolean :: Bool -> Value
boolean = Json.Bool

string :: String -> Value
string = Json.String . Text.pack

-- > isSafeIntegral (maxBound :: Int) == False
isSafeIntegral :: Integral i => i -> Bool
isSafeIntegral i = i >= -9007199254740991 && i <= 9007199254740991

safeIntegral :: Integral i => i -> Maybe Value
safeIntegral i =
  if isSafeIntegral i
  then Just (Json.Number (fromIntegral i))
  else Nothing

unsafeIntegral :: Integral i => i -> Value
unsafeIntegral = fromMaybe (error "Json.unsafeIntegral") . safeIntegral

safeInt :: Int -> Maybe Value
safeInt = safeIntegral

int :: Int -> Value
int = unsafeIntegral

safeInteger :: Integer -> Maybe Value
safeInteger = safeIntegral

integer :: Integer -> Value
integer = unsafeIntegral

double :: Double -> Value
double = Json.Number

type Association = (String, Value)

association :: Association -> Json.Pair
association (k, v) = (Text.pack k, v)

object :: [Association] -> Value
object = Json.object . map association

array :: [Value] -> Value
array = Json.Array

writeFile :: FilePath -> Value -> IO ()
writeFile fn json = ByteString.writeFile fn (Json.encode json)
