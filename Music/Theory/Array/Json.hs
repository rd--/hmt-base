-- | Regular matrix array data, Json.
module Music.Theory.Array.Json where

import qualified Text.JSON as Json {- json -}

import qualified Music.Theory.Math as Math

-- | Report failure as error.
reportError :: String -> Json.Result t -> t
reportError m r =
  case r of
    Json.Ok t -> t
    Json.Error e -> error (m ++ e)

{- | Decode Int

>>> decodeInt "1"
1
-}
decodeInt :: String -> Int
decodeInt = reportError "Int" . Json.decode

{- | Decode Double

>>> decodeDouble "3.141"
3.141
-}
decodeDouble :: String -> Double
decodeDouble = reportError "Double" . Json.decode

decodeArray :: Json.JSON t => String -> [t]
decodeArray = reportError "Array" . Json.decodeStrict

decodeArrayOfJson :: String -> [Json.JSValue]
decodeArrayOfJson = decodeArray

-- | Convert value to Rational, else error.
jsonToRational :: Json.JSValue -> Rational
jsonToRational j =
  case j of
    Json.JSRational _ r -> r
    _ -> error "jsonToRational"

{- | Decode array of Rational

>>> decodeArrayOfRational "[1,1.5,2]"
[1 % 1,3 % 2,2 % 1]
-}
decodeArrayOfRational :: String -> [Rational]
decodeArrayOfRational = map jsonToRational . decodeArrayOfJson

{- | Decode array of Integer

>>> decodeArrayOfInteger "[1,2,3]"
[1,2,3]
-}
decodeArrayOfInteger :: String -> [Integer]
decodeArrayOfInteger = map Math.rational_whole_err . decodeArrayOfRational

{- | Decode array of Int

>>> decodeArrayOfInt "[1,2,3]"
[1,2,3]

> decodeArrayOfInt "[1.2,3.141]" -- Error
-}
decodeArrayOfInt :: String -> [Int]
decodeArrayOfInt = map fromInteger . decodeArrayOfInteger

{- | Decode array of Double

>>> decodeArrayOfDouble "[1.2,3.141]"
[1.2,3.141]
-}
decodeArrayOfDouble :: String -> [Double]
decodeArrayOfDouble = decodeArray

{- | Decode matrix of Double

>>> decodeMatrixOfDouble "[[1.2,3.141],[7,23]]"
[[1.2,3.141],[7.0,23.0]]
-}
decodeMatrixOfDouble :: String -> [[Double]]
decodeMatrixOfDouble = decodeArray
