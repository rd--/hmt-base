{- | Json string association database.

Json objects do no allow multiple keys.
Here multiple keys are read & written as arrays.
This is no longer built since it is little used and introduces dependencies.
-}
module Music.Theory.Db.Json where

import Data.Bifunctor {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Data.ByteString as ByteString {- bytestring -}
import qualified Data.Map as Map {- containers -}
import qualified Data.Text as Text {- containers -}

import qualified Data.Aeson.Micro as Json {- microaeson -}

import qualified Music.Theory.Db.Common as Db {- hmt -}

-- | Load 'Db' from 'FilePath'.
db_load_utf8 :: FilePath -> IO Db.TextDb
db_load_utf8 fn = do
  let decode_assoc a =
        case a of
          Json.Object o -> List.head_err (map (first Text.unpack) (Map.toList o))
          _ -> error "decode_assoc?"
      decode_record r =
        case r of
          Json.Array l -> Db.record_uncollate (map (second (maybe_list_to_list . json_to_maybe_list_err) . decode_assoc) l)
          _ -> error "decode_record?"
  b <- ByteString.readFile fn
  case Json.decodeStrict b of
    Just (Json.Array l) -> return (map decode_record l)
    _ -> return []

{- | Store 'Db' to 'FilePath'.

> import qualified Music.Theory.Db.Plain as Db
> let fn = "/home/rohan/ut/www-spr/data/db.text"
> db <- Db.db_load_utf8 Db.sep_plain fn
> length db == 1480
> db_store_utf8 "/tmp/sp.js" db
-}
db_store_utf8 :: FilePath -> Db.TextDb -> IO ()
db_store_utf8 fn db = do
  let encode_assoc = Json.Object . Map.fromList . return . first Text.pack
      f =
        Json.Array
          . map (encode_assoc . second (maybe_list_to_json . list_to_maybe_list))
          . Db.record_collate
      b = Json.encodeStrict (Json.Array (map f db))
  ByteString.writeFile fn b

-- * Maybe List of String

data Maybe_List_Of_String = S String | L [String] deriving (Eq, Show)

maybe_list_to_list :: Maybe_List_Of_String -> [String]
maybe_list_to_list m =
  case m of
    S s -> [s]
    L l -> l

list_to_maybe_list :: [String] -> Maybe_List_Of_String
list_to_maybe_list l =
  case l of
    [s] -> S s
    _ -> L l

{- | Maybe_List_Of_String to Json.

>>> maybe_list_to_json (S "x")
String "x"

>>> maybe_list_to_json (L ["x","y"])
Array [String "x",String "y"]
-}
maybe_list_to_json :: Maybe_List_Of_String -> Json.Value
maybe_list_to_json m =
  case m of
    S s -> Json.String (Text.pack s)
    L l -> Json.Array (map (Json.String . Text.pack) l)

json_to_string_err :: Json.Value -> String
json_to_string_err j =
  case j of
    Json.String s -> Text.unpack s
    _ -> error "json_to_string?"

{- | Json to Maybe_List_Of_String.

>>> import qualified Data.Text.Encoding as Text
>>> let f = fromJust . Json.decodeStrict . Text.encodeUtf8 . Text.pack
>>> json_to_maybe_list (f "\"x\"")
Just (S "x")

>>> json_to_maybe_list (f "[\"x\",\"y\"]")
Just (L ["x","y"])
-}
json_to_maybe_list :: Json.Value -> Maybe Maybe_List_Of_String
json_to_maybe_list j =
  case j of
    Json.String s -> Just (S (Text.unpack s))
    Json.Array l -> Just (L (map json_to_string_err l))
    _ -> Nothing

json_to_maybe_list_err :: Json.Value -> Maybe_List_Of_String
json_to_maybe_list_err = fromMaybe (error "json_to_maybe_list") . json_to_maybe_list
