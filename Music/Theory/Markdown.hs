-- | Markdown functions.
module Music.Theory.Markdown where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.String as String {- hmt-base -}

-- | Minus=False Zero=Unknown Plus=True (https://en.wikipedia.org/wiki/Three-valued_logic)
data BalancedTernary = Minus | Zero | Plus
  deriving (Eq, Show, Read, Enum, Bounded)

isIndented :: String -> Bool
isIndented s = String.tabPrefixed 1 s || String.spacePrefixed 4 s

isCodeFence :: String -> Bool
isCodeFence s = ("```" `isPrefixOf` s) || ("~~~" `isPrefixOf` s)

-- | (Previous Line, In Block?)
type BlockState = (String, Bool)

type BlockAccum t = BlockState -> String -> (BlockState, t)

-- | The opening fence is marked Plus, and the closing fence Minus.
fencedCodeBlockBoundariesAccum :: BlockAccum BalancedTernary
fencedCodeBlockBoundariesAccum (previous, inBlock) current =
  if null previous && not inBlock && isCodeFence current
    then ((current, True), Plus)
    else
      if inBlock && isCodeFence current
        then ((current, False), Minus)
        else ((current, inBlock), Zero)

{- | Fenced code block boundaries.

> s <- readFile "/home/rohan/sw/spl/help/Reference/SinOsc.help.sl"
> fencedCodeBlockBoundaries (lines s)
-}
fencedCodeBlockBoundaries :: [String] -> [BalancedTernary]
fencedCodeBlockBoundaries = snd . mapAccumL fencedCodeBlockBoundariesAccum ("", False)

indentedCodeBlockBoundariesAccum :: BlockAccum BalancedTernary
indentedCodeBlockBoundariesAccum (previous, inBlock) current =
  if null previous && not inBlock && isIndented current
    then ((current, True), Plus)
    else
      if inBlock && current == ""
        then ((current, False), Minus)
        else ((current, inBlock), Zero)

{- | Indented code block boundaries.

> s <- readFile "/home/rohan/sw/spl/help/Reference/abs.help.sl"
> indentedCodeBlockBoundaries (lines s)
-}
indentedCodeBlockBoundaries :: [String] -> [BalancedTernary]
indentedCodeBlockBoundaries = snd . mapAccumL indentedCodeBlockBoundariesAccum ("", False)

extractCodeBlocksAccum :: Bool -> BlockAccum BalancedTernary -> BlockAccum (Maybe String)
extractCodeBlocksAccum includeClosing boundaries state current =
  let (_, inBlock) = state
      (state', indent) = boundaries state current
  in case indent of
      Minus -> (state', if includeClosing then Just current else Nothing)
      Zero -> (state', if inBlock then Just current else Nothing)
      Plus -> (state', Just current)

{- | Extract indented code blocks

> import Data.Maybe
> s <- readFile "/home/rohan/sw/spl/help/Reference/abs.help.sl"
> putStr $ unlines $ catMaybes $ extractIndentedCodeBlocks (lines s)
-}
extractIndentedCodeBlocks :: [String] -> [Maybe String]
extractIndentedCodeBlocks =
  snd
    . mapAccumL (extractCodeBlocksAccum False indentedCodeBlockBoundariesAccum) ("", False)

{- | Extract fenced code blocks

> import Data.Maybe
> s <- readFile "/home/rohan/sw/spl/help/Reference/SinOsc.help.sl"
> putStr $ unlines $ catMaybes $ extractFencedCodeBlocks (lines s)
-}
extractFencedCodeBlocks :: [String] -> [Maybe String]
extractFencedCodeBlocks =
  snd
    . mapAccumL (extractCodeBlocksAccum True fencedCodeBlockBoundariesAccum) ("", False)

indentedToFencedCodeBlocksAccum :: BlockState -> String -> (BlockState, String)
indentedToFencedCodeBlocksAccum state current =
  let (_, inBlock) = state
      (state', indent) = indentedCodeBlockBoundariesAccum state current
  in case indent of
      Minus -> (state', "```\n")
      Zero -> (state', if inBlock then tail current else current)
      Plus -> (state', "```\n" ++ tail current)

{- | Indented to fenced code blocks

> s <- readFile "/home/rohan/sw/spl/help/Reference/abs.help.sl"
> putStr $ unlines $ indentedToFencedCodeBlocks (lines s)
-}
indentedToFencedCodeBlocks :: [String] -> [String]
indentedToFencedCodeBlocks = snd . mapAccumL indentedToFencedCodeBlocksAccum ("", False)

fencedToIndentedCodeBlocksAccum :: BlockState -> String -> (BlockState, Maybe String)
fencedToIndentedCodeBlocksAccum state current =
  let (_, inBlock) = state
      (state', indent) = fencedCodeBlockBoundariesAccum state current
  in case indent of
      Minus -> (state', Nothing)
      Zero -> (state', Just (if inBlock then "\t" ++ current else current))
      Plus -> (state', Nothing)

{- | Indented to fenced code blocks

> s <- readFile "/home/rohan/sw/spl/help/Reference/SinOsc.help.sl"
> putStr $ unlines $ indentedToFencedCodeBlocks (lines s)
-}
fencedToIndentedCodeBlocks :: [String] -> [String]
fencedToIndentedCodeBlocks = catMaybes . snd . mapAccumL fencedToIndentedCodeBlocksAccum ("", False)
