-- | Either
module Music.Theory.Either where

import Data.Maybe {- base -}

-- | Maybe 'Left' of 'Either'.
from_left :: Either a b -> Maybe a
from_left e =
    case e of
      Left x -> Just x
      _ -> Nothing

-- | 'fromJust' of 'from_left'
from_left_err :: Either t e -> t
from_left_err = fromMaybe (error "from_left_err") . from_left

-- | Maybe 'Right' of 'Either'.
from_right :: Either x t -> Maybe t
from_right e =
    case e of
      Left _ -> Nothing
      Right r -> Just r

-- | 'fromJust' of 'from_right'
from_right_err :: Either e t -> t
from_right_err = fromMaybe (error "from_right_err") . from_right

-- | Flip from right to left, ie. 'either' 'Right' 'Left'
either_swap :: Either a b -> Either b a
either_swap = either Right Left

{- | Variant of 'Data.Either.rights' that preserves first 'Left'.

> all_right (map Right [1..3]) == Right [1..3]
> all_right [Right 1,Left 'a',Left 'b'] == Left 'a'
-}
all_right :: [Either a b] -> Either a [b]
all_right x =
    case x of
      [] -> Right []
      Right i:x' -> fmap (i :) (all_right x')
      Left i:_ -> Left i

-- | Lower 'Either' to 'Maybe' by discarding 'Left'.
either_to_maybe :: Either a b -> Maybe b
either_to_maybe = either (const Nothing) Just

-- | Data.Either.isLeft, which however hugs doesn't know of.
is_left :: Either a b -> Bool
is_left e = case e of { Left  _ -> True; Right _ -> False }

-- | Data.Either.isRight, which however hugs doesn't know of.
is_right :: Either a b -> Bool
is_right e = case e of { Left  _ -> False; Right _ -> True }

-- | Data.Either.partitionEithers, which however hugs doesn't know of.
partition_eithers :: [Either a b] -> ([a],[b])
partition_eithers =
  let left  a ~(l, r) = (a:l, r)
      right a ~(l, r) = (l, a:r)
  in foldr (either left right) ([],[])
