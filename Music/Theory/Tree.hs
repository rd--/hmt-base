-- | Tree functions
module Music.Theory.Tree where

import qualified Data.Tree as Tree {- containers -}

-- | Print forest as markdown list.
mdForest :: Tree.Forest String -> String
mdForest = unlines . concatMap (mdTree 0)

-- | Print tree as markdown list with indicated starting indent level.
mdTree :: Int -> Tree.Tree String -> [String]
mdTree k (Tree.Node txt st) = (replicate (k * 2) ' ' ++ "- " ++ txt) : concatMap (mdTree (k + 1)) st
