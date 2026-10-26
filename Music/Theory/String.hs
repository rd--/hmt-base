-- | String functions.
module Music.Theory.String where

import qualified Data.Char {- base -}
import qualified Data.List {- base -}

import qualified Data.List.Split {- split -}

{- | Case-insensitive '=='.

>>> map (str_eq_ci "ci") (words "CI ci Ci cI")
[True,True,True,True]
-}
str_eq_ci :: String -> String -> Bool
str_eq_ci x y =
  map Data.Char.toUpper x
  ==
  map Data.Char.toUpper y

{- | Remove @\r@.

>>> filter_cr "a\rb\r"
"ab"
-}
filter_cr :: String -> String
filter_cr = filter (not . (==) '\r')

{- | Delete trailing 'Char' where 'isSpace' holds.

>>> delete_trailing_whitespace "   str   "
"   str"

>>> delete_trailing_whitespace "\t\n        \t\n"
""
-}
delete_trailing_whitespace :: String -> String
delete_trailing_whitespace =
  reverse
  . dropWhile Data.Char.isSpace
  . reverse

{- | Variant of 'unwords' that does not write spaces for NIL elements.

>>> unwords_nil []
""

>>> unwords_nil ["a"]
"a"

>>> unwords_nil ["a",""]
"a"

>>> unwords_nil ["a","b"]
"a b"

>>> unwords_nil ["a","","b"]
"a b"

>>> unwords_nil ["a","","","b"]
"a b"

>>> unwords_nil ["a","b",""]
"a b"

>>> unwords_nil ["a","b","",""]
"a b"

>>> unwords_nil ["","a","b"]
"a b"

>>> unwords_nil ["","","a","b"]
"a b"
-}
unwords_nil :: [String] -> String
unwords_nil = unwords . filter (not . null)

-- | Variant of 'unlines' that does not write empty lines for NIL elements.
unlines_nil :: [String] -> String
unlines_nil = unlines . filter (not . null)

{- | unlines without a trailing newline.

>>> unlines (words "a b c")
"a\nb\nc\n"

>>> unlinesNoTrailingNewline (words "a b c")
"a\nb\nc"
-}
unlinesNoTrailingNewline :: [String] -> String
unlinesNoTrailingNewline = Data.List.intercalate "\n"

{- | unlines without a trailing newline.

>>> unlinesExceptLast (words "a b c")
"a\nb\nc"
-}
unlinesExceptLast :: [String] -> String
unlinesExceptLast x =
  case Data.List.unsnoc x of
    Just (l, e) -> unlines l ++ e
    _ -> concat x

{- | Capitalise first character of word.

>>> capitalise "freqShift"
"FreqShift"
-}
capitalise :: String -> String
capitalise x =
  case Data.List.uncons x of
    Nothing -> error "capitalise"
    Just (h, t) -> Data.Char.toUpper h : t

{- | Downcase first character of word.

>>> unCapitalise "FreqShift"
"freqShift"
-}
unCapitalise :: String -> String
unCapitalise x =
  case Data.List.uncons x of
    Nothing -> error "unCapitalise"
    Just (h, t) -> Data.Char.toLower h : t

{- | Apply function at each line of string.

>>> on_lines reverse "ab\ncde\nfg"
"ba\nedc\ngf\n"
-}
on_lines :: (String -> String) -> String -> String
on_lines f = unlines . map f . lines

{- | Is string prefixed by k tabs?

>>> tabPrefixed 1 "\tyes"
True

>>> tabPrefixed 2 "\tno"
False
-}
tabPrefixed :: Int -> String -> Bool
tabPrefixed k = Data.List.isPrefixOf (replicate k '\t')

{- | Is string prefixed by k spaces?

>>> spacePrefixed 4 "    yes"
True

>>> spacePrefixed 8 "    no"
False
-}
spacePrefixed :: Int -> String -> Bool
spacePrefixed k = Data.List.isPrefixOf (replicate k ' ')

{- | Remove spaces from both ends of input.

>>> stringTrim " string "
"string"
-}
stringTrim :: String -> String
stringTrim =
  let f = dropWhile Data.Char.isSpace
  in f . reverse . f . reverse

{- | Split on empty lines.

>>> paragraphs "a\nb\n\nc\nd\n"
["a\nb","c\nd"]
-}
paragraphs :: String -> [String]
paragraphs = Data.List.Split.splitOn "\n\n"

{- | Unparagraphs

>>> unparagraphs ["a\nb","c\nd"]
"a\nb\n\nc\nd\n"
-}
unparagraphs :: [String] -> String
unparagraphs = (++ "\n") . Data.List.intercalate "\n\n"
