-- | Boolean functions.
module Music.Theory.Bool where

import Data.List {- base -}

{- | If-then-else as a function.

> ifThenElse True "true" "false" == "true"
-}
ifThenElse :: Bool -> a -> a -> a
ifThenElse p q r = if p then q else r

{- | Case analysis as a function.
     Find first key that is True else elseValue.

> caseElse "z" [(True,"x"),(False,"y")] == "x"
> caseElse "z" [(False,"x"),(False,"y")] == "z"
-}
caseElse :: t -> [(Bool, t)] -> t
caseElse elseValue = maybe elseValue snd . find fst

{- | Case-of analysis as a function.
     Find first key that compares equal to selectValue else elseValue.

> caseOfElse "z" 'b' [('a',"x"),('b',"y")] == "y"
-}
caseOfElse :: Eq k => v -> k -> [(k, v)] -> v
caseOfElse elseValue selectValue = maybe elseValue snd . find ((== selectValue) . fst)
