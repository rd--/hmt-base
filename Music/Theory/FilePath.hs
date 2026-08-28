-- | FilePath functions.
module Music.Theory.FilePath where

import qualified System.FilePath {- filepath -}

{- | Construct a FilePath from a list of paths to be joined by the directory separator,
and a possibly empty list of extensions to be joined by the extension separator,
then joined by the extension separator.

>>> filePathJoin ["a"] []
"a"

>>> filePathJoin ["a", "b"] []
"a/b"

>>> filePathJoin ["a.x"] []
"a.x"

>>> filePathJoin ["a"] ["x"]
"a.x"

>>> filePathJoin ["a.x"] ["y"]
"a.x.y"

>>> filePathJoin ["a"] ["x", "y"]
"a.x.y"

>>> filePathJoin ["a", "b", "c"] ["x", "y"]
"a/b/c.x.y"

>>> filePathJoin ["a/", "b/", "c"] [".x", ".y"]
"a/b/c.x.y"
-}
filePathJoin :: [FilePath] -> [FilePath] -> FilePath
filePathJoin x y =
  let x' = foldl1 (System.FilePath.</>) x
  in if null y
     then x'
     else let y' = foldl1 (System.FilePath.<.>) y
          in x' System.FilePath.<.> y'

