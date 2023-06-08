-- | List permutation functions.
module Music.Theory.Permutations.List where

import Data.List {- base -}

import qualified Math.Combinatorics.Multiset as Multiset {- multiset-comb -}

import qualified Music.Theory.Permutations as Permutations {- hmt-base -}

{- | Generate all permutations.

> permutations_l [0,3] == [[0,3],[3,0]]
> length (permutations_l [1..5]) == Permutations.n_permutations 5
-}
permutations_l :: [a] -> [[a]]
permutations_l i =
    let f p = Permutations.apply_permutation p i
    in map f (Permutations.permutations_n (length i))

{- | /k/-element permutations of a set of /n/-elements.

> permutations_nk_l 3 2 "abc" == ["ab","ac","ba","bc","ca","cb"]
-}
permutations_nk_l :: Eq e => Int -> Int -> [e] -> [[e]]
permutations_nk_l n k e =
  if length e /= n
  then error "permutations_nk_l"
  else nub (map (take k) (permutations_l e))

{- | Generate all distinct permutations of a multi-set.

> multiset_permutations [0,1,1] == [[0,1,1],[1,1,0],[1,0,1]]
-}
multiset_permutations :: Ord a => [a] -> [[a]]
multiset_permutations = Multiset.permutations . Multiset.fromList

{- | Calculate number of permutations of a multiset.

> let r = Permutations.factorial 11 `div` product (map Permutations.factorial [1,4,4,2])
> multiset_permutations_n "MISSISSIPPI" == r

> multiset_permutations_n "MISSISSIPPI" == 34650
> length (multiset_permutations "MISSISSIPPI") == 34650
-}
multiset_permutations_n :: Ord a => [a] -> Int
multiset_permutations_n x =
    let occ = map length . group . sort
        n = Permutations.factorial (length x)
        d = product $ map Permutations.factorial $ occ x
    in n `div` d
