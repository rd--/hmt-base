-- | ByteString functions.
module Music.Theory.ByteString where

import Data.Int {- base -}

import qualified Data.ByteString as ByteString {- bytestring -}
import qualified Data.ByteString.Lazy as ByteString.Lazy {- bytestring -}

-- * Bs = ByteString

{- | ByteString variant of Data.List.Split.splitPlaces

>>> Data.List.Split.splitPlaces [1,2,3] "string"
["s","tr","ing"]

>>> bs_split_places [1,2,3] (ByteString.pack [115,116,114,105,110,103])
["s","tr","ing"]
-}
bs_split_places :: [Int] -> ByteString.ByteString -> [ByteString.ByteString]
bs_split_places i b =
    case i of
      [] -> []
      x:xs -> let (l,r) = ByteString.splitAt (fromIntegral x) b
              in l : bs_split_places xs r

{- | ByteString to Ascii string.

>>> bs_to_ascii_string (ByteString.pack [115,116,114,105,110,103])
"string"
-}
bs_to_ascii_string :: ByteString.ByteString -> String
bs_to_ascii_string = map (toEnum . fromIntegral) . takeWhile (/= 0) . ByteString.unpack

-- * Lbs = Lazy ByteString

{- | Section function for ByteString, ie. from (n,m).

>>> let a = ByteString.Lazy.pack [1..10]
>>> let b = lbs_slice 4 5 a
>>> let c = ByteString.Lazy.pack [5,6,7,8,9]
>>> b == c
True
-}
lbs_slice :: Int64 -> Int64 -> ByteString.Lazy.ByteString -> ByteString.Lazy.ByteString
lbs_slice n m = ByteString.Lazy.take m . ByteString.Lazy.drop n

{- | Variant of slice with start and end indices (zero-indexed).

>>> let a = ByteString.Lazy.pack [1..]
>>> let b = lbs_section 4 8 a
>>> let c = ByteString.Lazy.pack [5,6,7,8,9]
>>> b == c
True
-}
lbs_section :: Int64 -> Int64 -> ByteString.Lazy.ByteString -> ByteString.Lazy.ByteString
lbs_section l r = ByteString.Lazy.take (r - l + 1) . ByteString.Lazy.drop l

-- | ByteString variant of Data.List.Split.splitPlaces
lbs_split_places :: [Int] -> ByteString.Lazy.ByteString -> [ByteString.Lazy.ByteString]
lbs_split_places i b =
    case i of
      [] -> []
      x:xs -> let (l,r) = ByteString.Lazy.splitAt (fromIntegral x) b
              in l : lbs_split_places xs r

{- | Lazy ByteString to Ascii string.

>>> lbs_to_ascii_string (ByteString.Lazy.pack [115,116,114,105,110,103])
"string"
-}
lbs_to_ascii_string :: ByteString.Lazy.ByteString -> String
lbs_to_ascii_string = map (toEnum . fromIntegral) . takeWhile (/= 0) . ByteString.Lazy.unpack
