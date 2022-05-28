-- | String functions.
module Music.Theory.String where

import Data.Char {- base -}
import Data.List {- base -}

-- | Case-insensitive '=='.
--
-- > map (str_eq_ci "ci") (words "CI ci Ci cI")
str_eq_ci :: String -> String -> Bool
str_eq_ci x y = map toUpper x == map toUpper y

-- | Remove @\r@.
filter_cr :: String -> String
filter_cr = filter (not . (==) '\r')

-- | Delete trailing 'Char' where 'isSpace' holds.
--
-- > delete_trailing_whitespace "   str   " == "   str"
-- > delete_trailing_whitespace "\t\n        \t\n" == ""
delete_trailing_whitespace :: String -> String
delete_trailing_whitespace = reverse . dropWhile isSpace . reverse

{- | Variant of 'unwords' that does not write spaces for NIL elements.

> unwords_nil [] == ""
> unwords_nil ["a"] == "a"
> unwords_nil ["a",""] == "a"
> unwords_nil ["a","b"] == "a b"
> unwords_nil ["a","","b"] == "a b"
> unwords_nil ["a","","","b"] == "a b"
> unwords_nil ["a","b",""] == "a b"
> unwords_nil ["a","b","",""] == "a b"
> unwords_nil ["","a","b"] == "a b"
> unwords_nil ["","","a","b"] == "a b"
-}
unwords_nil :: [String] -> String
unwords_nil = unwords . filter (not . null)

-- | Variant of 'unlines' that does not write empty lines for NIL elements.
unlines_nil :: [String] -> String
unlines_nil = unlines . filter (not . null)

{- | unlines without a trailing newline.

> unlines (words "a b c") == "a\nb\nc\n"
> unlinesNoTrailingNewline (words "a b c") == "a\nb\nc"
-}
unlinesNoTrailingNewline :: [String] -> String
unlinesNoTrailingNewline = intercalate "\n"

{- | Capitalise first character of word.

> capitalise "freqShift" == "FreqShift"
-}
capitalise :: String -> String
capitalise x = toUpper (head x) : tail x

{- | Downcase first character of word.

> unCapitalise "FreqShift" == "freqShift"
-}
unCapitalise :: String -> String
unCapitalise x = toLower (head x) : tail x

-- | Apply function at each line of string.
--
-- > on_lines reverse "ab\ncde\nfg" == "ba\nedc\ngf\n"
on_lines :: (String -> String) -> String -> String
on_lines f = unlines . map f . lines
