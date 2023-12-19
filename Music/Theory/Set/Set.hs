-- | Set operations on 'Set's.
module Music.Theory.Set.Set where

import qualified Data.Set as Set {- containers -}

import qualified Music.Theory.Set.List as Set.List {- hmt-base -}

toSet :: (Ord a) => [a] -> Set.Set a
toSet = Set.fromList

{- | Powerset of set

>>> setPowerset (toSet [1,2])
fromList [fromList [],fromList [1],fromList [1,2],fromList [2]]
-}
setPowerset :: Ord a => Set.Set a -> Set.Set (Set.Set a)
setPowerset = Set.fromList . map Set.fromList . Set.List.powerset . Set.elems

setPairs :: Ord a => Set.Set a -> Set.Set (a, a)
setPairs = Set.fromList . Set.List.pairs . Set.elems
