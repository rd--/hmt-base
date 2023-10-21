-- | List functions.
module Music.Theory.List where

import Data.Bifunctor {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ord {- base -}

import qualified Safe {- safe -}

import qualified Data.IntMap as Map {- containers -}
import qualified Data.List.Ordered as O {- data-ordlist -}
import qualified Data.List.Split as Split {- split -}
import qualified Data.Tree as Tree {- containers -}

import qualified Music.Theory.Either as T {- hmt-base -}

-- | Data.List stops at zipWith7.
zipWith8 :: (a->b->c->d->e->f->g->h->i) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]
zipWith8 z a b c d e f g h =
  case (a,b,c,d,e,f,g,h) of
    (a0:as,b0:bs,c0:cs,d0:ds,e0:es,f0:fs,g0:gs,h0:hs) ->
      z a0 b0 c0 d0 e0 f0 g0 h0 : zipWith8 z as bs cs ds es fs gs hs
    _ -> []

{- | Zip eight

>>> zip8 "abc" ['b'..] ['c'..] ['d'..] ['e'..] ['f'..] ['g'..] ['h'..]
[('a','b','c','d','e','f','g','h'),('b','c','d','e','f','g','h','i'),('c','d','e','f','g','h','i','j')]
-}
zip8 :: [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[(a,b,c,d,e,f,g,h)]
zip8 = zipWith8 (\a b c d e f g h -> (a,b,c,d,e,f,g,h))

-- | Data.List stops at zipWith7.
zipWith9 :: (a->b->c->d->e->f->g->h->i->j) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]->[j]
zipWith9 z a b c d e f g h i =
  case (a,b,c,d,e,f,g,h,i) of
    (a0:as,b0:bs,c0:cs,d0:ds,e0:es,f0:fs,g0:gs,h0:hs,i0:is) ->
      z a0 b0 c0 d0 e0 f0 g0 h0 i0 : zipWith9 z as bs cs ds es fs gs hs is
    _ -> []

{- | Zip nine

>>> zip9 "abc" ['b'..] ['c'..] ['d'..] ['e'..] ['f'..] ['g'..] ['h'..] ['i'..]
[('a','b','c','d','e','f','g','h','i'),('b','c','d','e','f','g','h','i','j'),('c','d','e','f','g','h','i','j','k')]
-}
zip9 :: [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]->[(a,b,c,d,e,f,g,h,i)]
zip9 = zipWith9 (\a b c d e f g h i -> (a,b,c,d,e,f,g,h,i))

{- | Error checking 'take' variant. -}
take_err :: Int -> [a] -> [a]
take_err n e = let r = take n e in if length r /= n then error "take_err?" else r

{- | 'Data.Vector.slice', ie. starting index (zero-indexed) and number of elements.

>>> slice 4 5 [1..]
[5,6,7,8,9]
-}
slice :: Int -> Int -> [a] -> [a]
slice i n = take n . drop i

{- | Variant of slice with start and end indices (zero-indexed).

>>> section 4 8 [1..]
[5,6,7,8,9]
-}
section :: Int -> Int -> [a] -> [a]
section l r = take (r - l + 1) . drop l

{- | Bracket sequence with left and right values.

>>> bracket ('<','>') "1,2,3"
"<1,2,3>"
-}
bracket :: (a,a) -> [a] -> [a]
bracket (l,r) x = l : x ++ [r]

{- | Variant where brackets are sequences.

>>> bracket_l ("<:",":>") "1,2,3"
"<:1,2,3:>"
-}
bracket_l :: ([a],[a]) -> [a] -> [a]
bracket_l (l,r) s = l ++ s ++ r

{- | The first & middle & last elements of a list.

>>> map unbracket_el ["","{12}"]
[(Nothing,"",Nothing),(Just '{',"12",Just '}')]
-}
unbracket_el :: [a] -> (Maybe a,[a],Maybe a)
unbracket_el x =
    case x of
      [] -> (Nothing,[],Nothing)
      l:x' -> let (m,r) = separate_last' x' in (Just l,m,r)

{- | The first & middle & last elements of a list.

>>> map unbracket ["","{12}"]
[Nothing,Just ('{',"12",'}')]
-}
unbracket :: [t] -> Maybe (t,[t],t)
unbracket x =
    case unbracket_el x of
      (Just l,m,Just r) -> Just (l,m,r)
      _ -> Nothing

{- | Erroring variant. -}
unbracket_err :: [t] -> (t,[t],t)
unbracket_err = fromMaybe (error "unbracket") . unbracket

-- * Split

head_err :: [t] -> t
head_err = Safe.headNote "head_err"

tail_err :: [t] -> [t]
tail_err = Safe.tailNote "tail_err"

last_err :: [t] -> t
last_err = Safe.lastNote "last_err"

{- | Relative of 'Split.splitOn', but only makes first separation.

>>> Split.splitOn "//" "lhs//rhs//rem"
["lhs","rhs","rem"]

>>> separate_at "//" "lhs//rhs//rem"
Just ("lhs","rhs//rem")
-}
separate_at :: Eq a => [a] -> [a] -> Maybe ([a],[a])
separate_at x =
    let n = length x
        f lhs rhs =
            if null rhs
            then Nothing
            else if x == take n rhs
                 then Just (reverse lhs,drop n rhs)
                 else f (head_err rhs : lhs) (tail_err rhs)
    in f []

{- | Variant of 'Split.splitWhen' that keeps delimiters at left.

>>> split_when_keeping_left (== 'r') "rab rcd re rf r"
["","rab ","rcd ","re ","rf ","r"]
-}
split_when_keeping_left :: (a -> Bool) -> [a] -> [[a]]
split_when_keeping_left = Split.split . Split.keepDelimsL . Split.whenElt

{- | Split before the indicated element, keeping it at the left of the sub-sequence it begins.
     'split_when_keeping_left' of '=='

>>> split_before 'x' "axbcxdefx"
["a","xbc","xdef","x"]

>>> split_before 'x' "xa"
["","xa"]

>>> map (flip split_before "abcde") "ae_"
[["","abcde"],["abcd","e"],["abcde"]]

>>> map (flip break "abcde" . (==)) "ae_"
[("","abcde"),("abcd","e"),("abcde","")]

>>> split_before 'r' "rab rcd re rf r"
["","rab ","rcd ","re ","rf ","r"]
-}
split_before :: Eq a => a -> [a] -> [[a]]
split_before x = split_when_keeping_left (== x)

{- | Split before any of the indicated set of delimiters.

>>> split_before_any ",;" ";a,b,c;d;"
["",";a",",b",",c",";d",";"]
-}
split_before_any :: Eq a => [a] -> [a] -> [[a]]
split_before_any = Split.split . Split.keepDelimsL . Split.oneOf

{- | Singleton variant of 'Split.splitOn'.

>>> split_on_1 ":" "graph:layout"
Just ("graph","layout")
-}
split_on_1 :: Eq t => [t] -> [t] -> Maybe ([t],[t])
split_on_1 e l =
    case Split.splitOn e l of
      [p,q] -> Just (p,q)
      _ -> Nothing

{- | Erroring variant. -}
split_on_1_err :: Eq t => [t] -> [t] -> ([t],[t])
split_on_1_err e = fromMaybe (error "split_on_1") . split_on_1 e

{- | Split function that splits only once, ie. a variant of 'break'.

>>> split1 ' ' "three word sentence"
Just ("three","word sentence")
-}
split1 :: Eq a => a -> [a] -> Maybe ([a],[a])
split1 c l =
    case break (== c) l of
      (lhs,_:rhs) -> Just (lhs,rhs)
      _ -> Nothing

{- | Erroring variant. -}
split1_err :: (Eq a, Show a) => a -> [a] -> ([a], [a])
split1_err e s = fromMaybe (error (show ("split1",e,s))) (split1 e s)

{- | If length is not even the second "half" is longer.

>>> split_into_halves []
([],[])

>>> split_into_halves [1]
([],[1])

>>> split_into_halves [1 .. 2]
([1],[2])

>>> split_into_halves [1 .. 8]
([1,2,3,4],[5,6,7,8])

>>> split_into_halves [1 .. 9]
([1,2,3,4],[5,6,7,8,9])
-}
split_into_halves :: [t] -> ([t], [t])
split_into_halves l =
  let n = length l `div` 2
      m = if n == 1 then 1 else n + (n `mod` 2) -- the two element list is a special case
  in (take m l, drop m l)

-- * Rotate

{- | Generic form of 'rotate_left'. -}
genericRotate_left :: Integral i => i -> [a] -> [a]
genericRotate_left n =
    let f (p,q) = q ++ p
    in f . genericSplitAt n

{- | Left rotation.

>>> rotate_left 1 [1..3]
[2,3,1]

>>> rotate_left 3 [1..5]
[4,5,1,2,3]
-}
rotate_left :: Int -> [a] -> [a]
rotate_left = genericRotate_left

{- | Generic form of 'rotate_right'. -}
genericRotate_right :: Integral n => n -> [a] -> [a]
genericRotate_right n = reverse . genericRotate_left n . reverse

{- | Right rotation.

>>> rotate_right 1 [1..3]
[3,1,2]
-}
rotate_right :: Int -> [a] -> [a]
rotate_right = genericRotate_right

{- | Rotate left by /n/ 'mod' /#p/ places.  Therefore negative n rotate right.


>>> rotate 1 [1..3]
[2,3,1]

>>> rotate 8 [1..5]
[4,5,1,2,3]

>>> (rotate (-1) "ABCD",rotate 1 "ABCD")
("DABC","BCDA")
-}
rotate :: (Integral n) => n -> [a] -> [a]
rotate n p =
    let m = n `mod` genericLength p
    in genericRotate_left m p

{- | Rotate right by /n/ places.

>>> rotate_r 8 [1..5]
[3,4,5,1,2]
-}
rotate_r :: (Integral n) => n -> [a] -> [a]
rotate_r = rotate . negate

{- | All rotations.

>>> rotations [0,1,3]
[[0,1,3],[1,3,0],[3,0,1]]
-}
rotations :: [a] -> [[a]]
rotations p = map (`rotate_left` p) [0 .. length p - 1]

{- | Rotate list so that is starts at indicated element.

>>> rotate_starting_from 'c' "abcde"
Just "cdeab"

>>> rotate_starting_from '_' "abc"
Nothing
-}
rotate_starting_from :: Eq a => a -> [a] -> Maybe [a]
rotate_starting_from x l =
    case break (== x) l of
      (_,[]) -> Nothing
      (lhs,rhs) -> Just (rhs ++ lhs)

{- | Erroring variant. -}
rotate_starting_from_err :: Eq a => a -> [a] -> [a]
rotate_starting_from_err x =
    fromMaybe (error "rotate_starting_from: non-element") .
    rotate_starting_from x

{- | Sequence of /n/ adjacent elements, moving forward by /k/ places.
The last element may have fewer than /n/ places, but will reach the end of the input sequence.

>>> adj 3 2 "adjacent"
["adj","jac","cen","nt"]
-}
adj :: Int -> Int -> [a] -> [[a]]
adj n k l =
    case take n l of
      [] -> []
      r -> r : adj n k (drop k l)

{- | Variant of 'adj' where the last element has /n/ places but may not reach the end of the input sequence.

>>> adj_trunc 4 1 "adjacent"
["adja","djac","jace","acen","cent"]

>>> adj_trunc 3 2 "adjacent"
["adj","jac","cen"]
-}
adj_trunc :: Int -> Int -> [a] -> [[a]]
adj_trunc n k l =
    let r = take n l
    in if length r == n then r : adj_trunc n k (drop k l) else []

{- | 'adj_trunc' of 'close' by /n/-1.

>>> adj_cyclic_trunc 3 1 "adjacent"
["adj","dja","jac","ace","cen","ent","nta","tad"]
-}
adj_cyclic_trunc :: Int -> Int -> [a] -> [[a]]
adj_cyclic_trunc n k = adj_trunc n k . close (n - 1)

{- | Generic form of 'adj2'. -}
genericAdj2 :: (Integral n) => n -> [t] -> [(t,t)]
genericAdj2 n l =
    case l of
      p:q:_ -> (p,q) : genericAdj2 n (genericDrop n l)
      _ -> []

{- | Adjacent elements of list, at indicated distance, as pairs.

>>> adj2 1 [1..5]
[(1,2),(2,3),(3,4),(4,5)]

>>> let l = [1..5] in zip l (tail_err l) == adj2 1 l
True

>>> adj2 2 [1..4]
[(1,2),(3,4)]

>>> adj2 3 [1..5]
[(1,2),(4,5)]
-}
adj2 :: Int -> [t] -> [(t,t)]
adj2 = genericAdj2

{- | Append first /n/-elements to end of list.

>>> close 1 [1..3]
[1,2,3,1]
-}
close :: Int -> [a] -> [a]
close k x = x ++ take k x

{- | 'adj2' '.' 'close' 1.

>>> adj2_cyclic 1 [1..3]
[(1,2),(2,3),(3,1)]
-}
adj2_cyclic :: Int -> [t] -> [(t,t)]
adj2_cyclic n = adj2 n . close 1

{- | Adjacent triples.

>>> adj3 3 [1..6]
[(1,2,3),(4,5,6)]
-}
adj3 :: Int -> [t] -> [(t,t,t)]
adj3 n l =
  case l of
      p:q:r:_ -> (p,q,r) : adj3 n (drop n l)
      _ -> []

{- | 'adj3' '.' 'close' 2.

>>> adj3_cyclic 1 [1..4]
[(1,2,3),(2,3,4),(3,4,1),(4,1,2)]
-}
adj3_cyclic :: Int -> [t] -> [(t,t,t)]
adj3_cyclic n = adj3 n . close 2

{- | Adjacent quadruples.

>>> adj4 2 [1..8]
[(1,2,3,4),(3,4,5,6),(5,6,7,8)]

>>> adj4 4 [1..8]
[(1,2,3,4),(5,6,7,8)]
-}
adj4 :: Int -> [t] -> [(t,t,t,t)]
adj4 n l =
  case l of
      p:q:r:s:_ -> (p,q,r,s) : adj4 n (drop n l)
      _ -> []

{- | Interleave elements of /p/ and /q/.  If not of equal length elements are discarded.

>>> interleave [1..3] [4..6]
[1,4,2,5,3,6]

>>> interleave ".+-" "abc"
".a+b-c"

>>> interleave [1..3] []
[]
-}
interleave :: [a] -> [a] -> [a]
interleave p = concat . zipWith (\i j -> [i, j]) p -- concatMap (\(i, j) -> [i, j]) . zip p

{- | Interleave list of lists.  Allows lists to be of non-equal lengths, or infinite.

>>> interleave_set ["abcd","efgh","ijkl"]
"aeibfjcgkdhl"

>>> interleave_set ["abc","defg","hijkl"]
"adhbeicfjgkl"
-}
interleave_set :: [[a]] -> [a]
interleave_set = concat . transpose

{- | Interleave list of lists.  Allows lists to be of non-equal lenghts.

>>> interleave_set_folding ["abcd","efgh","ijkl"]
"aebicfdjgkhl"

>>> interleave_set_folding ["abc","defg","hijkl"]
"adbhceifjgkl"
-}
interleave_set_folding :: [[a]] -> [a]
interleave_set_folding p =
  let f xs ys =
        case xs of
          [] -> ys
          x:xs' -> x : f ys xs'
  in foldr f [] p

{-
import Safe {- safe -}

interleave_set l =
    case mapMaybe headMay l of
      [] -> []
      r -> r ++ interleave_set (mapMaybe tailMay l)
-}

{- | De-interleave /n/ lists.

>>> deinterleave 2 ".a+b-c"
[".+-","abc"]

>>> deinterleave 3 "aeibfjcgkdhl"
["abcd","efgh","ijkl"]
-}
deinterleave :: Int -> [a] -> [[a]]
deinterleave n = transpose . Split.chunksOf n

{- | Special case for two-part deinterleaving.

>>> deinterleave2 ".a+b-c"
(".+-","abc")
-}
deinterleave2 :: [t] -> ([t], [t])
deinterleave2 =
    let f l =
            case l of
              p:q:l' -> (p,q) : f l'
              _ -> []
    in unzip . f

{-
deinterleave2 =
    let f p q l =
            case l of
              [] -> (reverse p,reverse q)
              [a] -> (reverse (a:p),reverse q)
              a:b:l' -> rec (a:p) (b:q) l'
    in f [] []
-}

{- | Variant that continues with the longer input.

>>> interleave_continue ".+-" "abc"
".a+b-c"

>>> interleave_continue [1..3] []
[1,2,3]

>>> interleave_continue [] [1..3]
[1,2,3]
-}
interleave_continue :: [a] -> [a] -> [a]
interleave_continue p q =
    case (p,q) of
      ([],_) -> q
      (_,[]) -> p
      (i:p',j:q') -> i : j : interleave_continue p' q'

{- | 'interleave' of 'rotate_left' by /i/ and /j/.

>>> interleave_rotations 9 3 [1..13]
[10,4,11,5,12,6,13,7,1,8,2,9,3,10,4,11,5,12,6,13,7,1,8,2,9,3]
-}
interleave_rotations :: Int -> Int -> [b] -> [b]
interleave_rotations i j s = interleave (rotate_left i s) (rotate_left j s)

{- | 'unzip', apply /f1/ and /f2/ and 'zip'. -}
rezip :: ([t] -> [u]) -> ([v] -> [w]) -> [(t,v)] -> [(u,w)]
rezip f1 f2 l = let (p,q) = unzip l in zip (f1 p) (f2 q)

{- | Generalised histogram, with equality function for grouping and comparison function for sorting. -}
generic_histogram_by :: Integral i => (a -> a-> Bool) -> Maybe (a -> a-> Ordering) -> [a] -> [(a,i)]
generic_histogram_by eq_f cmp_f x =
    let g = groupBy eq_f (maybe x (`sortBy` x) cmp_f)
    in zip (map head_err g) (map genericLength g)

{- | Type specialised 'generic_histogram_by'. -}
histogram_by :: (a -> a-> Bool) -> Maybe (a -> a-> Ordering) -> [a] -> [(a,Int)]
histogram_by = generic_histogram_by

{- | Count occurences of elements in list, 'histogram_by' of '==' and 'compare'. -}
generic_histogram :: (Ord a,Integral i) => [a] -> [(a,i)]
generic_histogram = generic_histogram_by (==) (Just compare)

{- | Type specialised 'generic_histogram'.  Elements will be in ascending order.

>>> map histogram ["","hohoh","yxx"]
[[],[('h',3),('o',2)],[('x',2),('y',1)]]
-}
histogram :: Ord a => [a] -> [(a,Int)]
histogram = generic_histogram

{- | Join two histograms, which must be sorted.

>>> histogram_join (zip "ab" [1,1]) (zip "bc" [1,1])
[('a',1),('b',2),('c',1)]
-}
histogram_join :: Ord a => [(a,Int)] -> [(a,Int)] -> [(a,Int)]
histogram_join p q =
  let f (e1,n1) (e2,n2) = if e1 == e2 then Just (e1,n1 + n2) else Nothing
  in case (p,q) of
       (_,[]) -> p
       ([],_) -> q
       (p1:p',q1:q') -> case f p1 q1 of
                          Just r -> r : histogram_join p' q'
                          Nothing -> if p1 < q1
                                     then p1 : histogram_join p' q
                                     else q1 : histogram_join p q'

{- | 'foldr' of 'histogram_join'.

>>> let f x = zip x (repeat 1) in histogram_merge (map f ["ab","bcd","de"])
[('a',1),('b',2),('c',1),('d',2),('e',1)]
-}
histogram_merge :: Ord a => [[(a,Int)]] -> [(a,Int)]
histogram_merge = foldr histogram_join []

{- | Given (k,#) histogram where k is enumerable generate filled histogram with 0 for empty k.

>>> histogram_fill (histogram "histogram") == zip ['a'..'t'] [1,0,0,0,0,0,1,1,1,0,0,0,1,0,1,0,0,1,1,1]
True
-}
histogram_fill :: (Ord a, Enum a) => [(a,Int)] -> [(a,Int)]
histogram_fill h =
  let k = map fst h
      e = [minimum k .. maximum k]
      f x = fromMaybe 0 (lookup x h)
  in zip e (map f e)

{- | Given two histograms p & q (sorted by key) make composite
histogram giving for all keys the counts for (p,q).

>>> histogram_composite (zip "ABCD" [4,3,2,1]) (zip "ABCE" [2,3,4,5])
[('A',(4,2)),('B',(3,3)),('C',(2,4)),('D',(1,0)),('E',(0,5))]

-}
histogram_composite :: Ord a => [(a,Int)] -> [(a,Int)] -> [(a,(Int,Int))]
histogram_composite p q =
  case (p,q) of
    ([],_) -> map (\(k,n) -> (k,(0,n))) q
    (_,[]) -> map (\(k,n) -> (k,(n,0))) p
    ((k1,n1):p',(k2,n2):q') -> case compare k1 k2 of
                                 LT -> (k1,(n1,0)) : histogram_composite p' q
                                 EQ -> (k1,(n1,n2)) : histogram_composite p' q'
                                 GT -> (k2,(0,n2)) : histogram_composite p q'

{- | Apply '-' at count of 'histogram_composite', ie. 0 indicates
equal number at p and q, negative indicates more elements at p than
q and positive more elements at q than p.

>>> histogram_diff (zip "ABCD" [4,3,2,1]) (zip "ABCE" [2,3,4,5]) == zip "ABCDE" [-2,0,2,-1,5]
True
-}
histogram_diff :: Ord a => [(a,Int)] -> [(a,Int)] -> [(a,Int)]
histogram_diff p = map (\(k,(n,m)) -> (k,m - n)) . histogram_composite p

{- | Elements that appear more than once in the input given equality predicate. -}
duplicates_by :: Ord a => (a -> a -> Bool) -> [a] -> [a]
duplicates_by f = map fst . filter (\(_,n) -> n > 1) . histogram_by f (Just compare)

{- | 'duplicates_by' of '=='.

>>> map duplicates ["duplicates","redundant"]
["","dn"]
-}
duplicates :: Ord a => [a] -> [a]
duplicates = duplicates_by (==)

{- | List segments of length /i/ at distance /j/.

>>> segments 2 1 [1..5]
[[1,2],[2,3],[3,4],[4,5]]

>>> segments 2 2 [1..5]
[[1,2],[3,4]]
-}
segments :: Int -> Int -> [a] -> [[a]]
segments i j p =
    let q = take i p
        p' = drop j p
    in if length q /= i then [] else q : segments i j p'

{- | 'foldl1' 'intersect'.

>>> intersect_l [[1,2],[1,2,3],[1,2,3,4]]
[1,2]
-}
intersect_l :: Eq a => [[a]] -> [a]
intersect_l = foldl1 intersect

{- | 'foldl1' 'union'.

>>> sort (union_l [[1,3],[2,3],[3]])
[1,2,3]
-}
union_l :: Eq a => [[a]] -> [a]
union_l = foldl1 union

{- | Intersection of adjacent elements of list at distance /n/.

>>> adj_intersect 1 [[1,2],[1,2,3],[1,2,3,4]]
[[1,2],[1,2,3]]
-}
adj_intersect :: Eq a => Int -> [[a]] -> [[a]]
adj_intersect n = map intersect_l . segments 2 n

{- | List of cycles at distance /n/.

>>> cycles 2 [1..6]
[[1,3,5],[2,4,6]]

>>> cycles 3 [1..9]
[[1,4,7],[2,5,8],[3,6,9]]

>>> cycles 4 [1..8]
[[1,5],[2,6],[3,7],[4,8]]
-}
cycles :: Int -> [a] -> [[a]]
cycles n = transpose . Split.chunksOf n

{- | Variant of 'filter' that has a predicate to halt processing, ie. 'filter' of 'takeWhile'.

>>> filter_halt (even . fst) ((< 5) . snd) (zip [1..] [0..])
[(2,1),(4,3)]
-}
filter_halt :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filter_halt sel end = filter sel . takeWhile end

{- | Variant of 'Data.List.filter' that retains 'Nothing' as a placeholder for removed elements.

>>> filter_maybe even [1..4]
[Nothing,Just 2,Nothing,Just 4]
-}
filter_maybe :: (a -> Bool) -> [a] -> [Maybe a]
filter_maybe f = map (\e -> if f e then Just e else Nothing)

{- | Select only the elements from the list that lie in the indicated range, which is (inclusive, exclusive).

>>> filterInRange (3, 5) [1, 1.5 .. 9]
[3.0,3.5,4.0,4.5]
-}
filterInRange :: Ord a => (a, a) -> [a] -> [a]
filterInRange (lhs, rhs) = filter (\n -> n >= lhs && n < rhs)

{- | Replace all /p/ with /q/ in /s/.

>>> replace "_x_" "-X-" "an _x_ string"
"an -X- string"

>>> replace "ab" "cd" "ab ab cd ab"
"cd cd cd cd"
-}
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace p q s =
    let n = length p
    in case s of
         [] -> []
         c:s' -> if p `isPrefixOf` s
                 then q ++ replace p q (drop n s)
                 else c : replace p q s'

{- | Replace the /i/th value at /ns/ with /x/.

>>> replace_at "test" 2 'n'
"tent"
-}
replace_at :: Integral i => [a] -> i -> a -> [a]
replace_at ns i x =
    let f j y = if i == j then x else y
    in zipWith f [0..] ns

{- | Data.List.stripPrefix, which however hugs doesn't know of. -}
strip_prefix :: Eq a => [a] -> [a] -> Maybe [a]
strip_prefix lhs rhs =
  case (lhs,rhs) of
    ([], ys) -> Just ys
    (_, []) -> Nothing
    (x:xs, y:ys) -> if x == y then strip_prefix xs ys else Nothing

{- | 'error' of 'stripPrefix' -}
strip_prefix_err :: Eq t => [t] -> [t] -> [t]
strip_prefix_err pfx = fromMaybe (error "strip_prefix") . strip_prefix pfx

-- * Association lists

{- | Equivalent to 'groupBy' /eq/ 'on' /f/.

>>> group_by_on (==) snd (zip [0..] "abbc")
[[(0,'a')],[(1,'b'),(2,'b')],[(3,'c')]]
-}
group_by_on :: (x -> x -> Bool) -> (t -> x) -> [t] -> [[t]]
group_by_on eq f = groupBy (eq `on` f)

{- | 'group_by_on' of '=='.

>>> group_on fst (zip [1,1,2,3,3,4] "abcdef")
[[(1,'a'),(1,'b')],[(2,'c')],[(3,'d'),(3,'e')],[(4,'f')]]
-}
group_on :: Eq x => (a -> x) -> [a] -> [[a]]
group_on = group_by_on (==)

{- | Given an equality predicate and accesors for /key/ and /value/ collate adjacent values. -}
collate_by_on_adjacent :: (k -> k -> Bool) -> (a -> k) -> (a -> v) -> [a] -> [(k,[v])]
collate_by_on_adjacent eq f g =
    let h l = case l of
                [] -> error "collate_by_on_adjacent"
                l0:_ -> (f l0,map g l)
    in map h . group_by_on eq f

{- | 'collate_by_on_adjacent' of '==' -}
collate_on_adjacent :: Eq k => (a -> k) -> (a -> v) -> [a] -> [(k,[v])]
collate_on_adjacent = collate_by_on_adjacent (==)

{- | 'collate_on_adjacent' of 'fst' and 'snd'.

>>> collate_adjacent (zip "TDD" "xyz")
[('T',"x"),('D',"yz")]
-}
collate_adjacent :: Eq a => [(a,b)] -> [(a,[b])]
collate_adjacent = collate_on_adjacent fst snd

{- | Data.List.sortOn, which however hugs doesn't know of. -}
sort_on :: Ord b => (a -> b) -> [a] -> [a]
sort_on f = map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

{- | 'sortOn' prior to 'collate_on_adjacent'.

>>> collate_on fst snd (zip "ABCBCD" "abcdef")
[('A',"a"),('B',"bd"),('C',"ce"),('D',"f")]
-}
collate_on :: Ord k => (a -> k) -> (a -> v) -> [a] -> [(k,[v])]
collate_on f g = collate_on_adjacent f g . sort_on f

{- | 'collate_on' of 'fst' and 'snd'.

>>> collate (zip "TDD" "xyz")
[('D',"yz"),('T',"x")]

>>> collate (zip [1,2,1] "abc")
[(1,"ac"),(2,"b")]
-}
collate :: Ord a => [(a,b)] -> [(a,[b])]
collate = collate_on fst snd

{- | Reverse of 'collate', inverse if order is not considered.

>>> uncollate [(1,"ac"),(2,"b")]
[(1,'a'),(1,'c'),(2,'b')]
-}
uncollate :: [(k,[v])] -> [(k,v)]
uncollate = concatMap (\(k,v) -> zip (repeat k) v)

{- | Make /assoc/ list with given /key/.

>>> with_key 'a' [1..3]
[('a',1),('a',2),('a',3)]
-}
with_key :: k -> [v] -> [(k,v)]
with_key h = zip (repeat h)

{- | Left biased merge of association lists /p/ and /q/.

>>> assoc_merge [(5,"a"),(3,"b")] [(5,"A"),(7,"C")]
[(5,"a"),(3,"b"),(7,"C")]
-}
assoc_merge :: Eq k => [(k,v)] -> [(k,v)] -> [(k,v)]
assoc_merge p q =
    let p_k = map fst p
        q' = filter ((`notElem` p_k) . fst) q
    in p ++ q'

{- | Keys are in ascending order, the entry retrieved is the rightmose with a key less than or equal to the key requested.
If the key requested is less than the initial key, or the list is empty, returns 'Nothing'.

>>> let m = [(1,'a'),(4,'x'),(4,'b'),(5,'c')]
>>> mapMaybe (ord_map_locate m) [1 .. 6]
[(1,'a'),(1,'a'),(1,'a'),(4,'b'),(5,'c'),(5,'c')]

>>> ord_map_locate m 0
Nothing
-}
ord_map_locate :: Ord k => [(k,v)] -> k -> Maybe (k,v)
ord_map_locate mp i =
    let f (k0,v0) xs =
          case xs of
            [] -> if i >= k0 then Just (k0,v0) else error "ord_map_locate?"
            ((k1,v1):xs') -> if i >= k0 && i < k1 then Just (k0,v0) else f (k1,v1) xs'
    in case mp of
         [] -> Nothing
         (k0,v0):mp' -> if i < k0 then Nothing else f (k0,v0) mp'

-- * Δ

{- | Intervals to values, zero is /n/.

>>> dx_d 5 [1,2,3]
[5,6,8,11]
-}
dx_d :: (Num a) => a -> [a] -> [a]
dx_d = scanl (+)

{- | Variant that takes initial value and separates final value.
This is an appropriate function for 'mapAccumL'.

>>> dx_d' 5 [1,2,3]
(11,[5,6,8])

>>> dx_d' 0 [1,1,1]
(3,[0,1,2])
-}
dx_d' :: Num t => t -> [t] -> (t,[t])
dx_d' n l =
    case reverse (scanl (+) n l) of
      e:r -> (e,reverse r)
      _ -> error "dx_d'"

{- | Integration with /f/, ie. apply flip of /f/ between elements of /l/.

>>> d_dx_by (,) "abcd"
[('b','a'),('c','b'),('d','c')]

>>> d_dx_by (-) [0,2,4,1,0]
[2,2,-3,-1]

>>> d_dx_by (-) [2,3,0,4,1]
[1,-3,4,-3]
-}
d_dx_by :: (t -> t -> u) -> [t] -> [u]
d_dx_by f l = if null l then [] else zipWith f (tail_err l) l

{- | Integrate, 'd_dx_by' '-', ie. pitch class segment to interval sequence.

>>> d_dx [5,6,8,11]
[1,2,3]

>>> d_dx []
[]
-}
d_dx :: (Num a) => [a] -> [a]
d_dx = d_dx_by (-)

{- | Elements of /p/ not in /q/.

>>> [1,2,3] `difference` [1,2]
[3]
-}
difference :: Eq a => [a] -> [a] -> [a]
difference p q = filter (`notElem` q) p

{- | Is /p/ a subset of /q/, ie. is 'intersect' of /p/ and /q/ '==' /p/.

>>> map (is_subset [1,2]) [[1],[1,2],[1,2,3]]
[False,True,True]
-}
is_subset :: Eq a => [a] -> [a] -> Bool
is_subset p q = p `intersect` q == p

{- | Is /p/ a subset of /q/, ie. are all elements of /p/ elements of /q/.

>>> map (isSubsetOf [1,2]) [[1],[1,2],[1,2,3]]
[False,True,True]
-}
isSubsetOf :: Eq t => [t] -> [t] -> Bool
isSubsetOf p q = all (\e -> e `elem` q) p

{- | Is /p/ a proper subset of /q/, 'is_subset' and 'not' equal.

>>> map (is_proper_subset [1,2]) [[1],[1,2],[1,2,3]]
[False,False,True]
-}
is_proper_subset :: Eq a => [a] -> [a] -> Bool
is_proper_subset p q = is_subset p q && p /= p `union` q

{- | Is /p/ a superset of /q/, ie. 'flip' 'is_subset'.

>>> is_superset [1,2,3] [1,2]
True
-}
is_superset :: Eq a => [a] -> [a] -> Bool
is_superset = flip is_subset

{- | Is /p/ a subsequence of /q/, ie. synonym for 'isInfixOf'.

>>> subsequence [1,2] [1,2,3]
True
-}
subsequence :: Eq a => [a] -> [a] -> Bool
subsequence = isInfixOf

{- | Erroring variant of 'findIndex'. -}
findIndex_err :: (a -> Bool) -> [a] -> Int
findIndex_err f = fromMaybe (error "findIndex?") . findIndex f

{- | Erroring variant of 'elemIndex'. -}
elemIndex_err :: Eq a => a -> [a] -> Int
elemIndex_err x = fromMaybe (error "ix_of") . elemIndex x

{- | Variant of 'elemIndices' that requires /e/ to be unique in /p/.

> elem_index_unique 'a' "abcda" == undefined
-}
elem_index_unique :: Eq a => a -> [a] -> Int
elem_index_unique e p =
    case elemIndices e p of
      [i] -> i
      _ -> error "elem_index_unique"

{- | Lookup that errors and prints message and key. -}
lookup_err_msg :: (Eq k,Show k) => String -> k -> [(k,v)] -> v
lookup_err_msg err k = fromMaybe (error (err ++ ": " ++ show k)) . lookup k

{- | Error variant. -}
lookup_err :: Eq k => k -> [(k,v)] -> v
lookup_err n = fromMaybe (error "lookup") . lookup n

{- | 'lookup' variant with default value. -}
lookup_def :: Eq k => k -> v -> [(k,v)] -> v
lookup_def k d = fromMaybe d . lookup k

{- | If /l/ is empty 'Nothing', else 'Just' /l/. -}
non_empty :: [t] -> Maybe [t]
non_empty l = if null l then Nothing else Just l

{- | Variant on 'filter' that selects all matches.

>>> lookup_set 1 (zip [1,2,3,4,1] "abcde")
Just "ae"
-}
lookup_set :: Eq k => k -> [(k,v)] -> Maybe [v]
lookup_set k = non_empty . map snd . filter ((== k) . fst)

{- | Erroring variant. -}
lookup_set_err :: Eq k => k -> [(k,v)] -> [v]
lookup_set_err k = fromMaybe (error "lookup_set?") . lookup_set k

{- | Reverse lookup.

>>> reverse_lookup 'c' []
Nothing

>>> reverse_lookup 'b' (zip [1..] ['a'..])
Just 2

>>> lookup 2 (zip [1..] ['a'..])
Just 'b'
-}
reverse_lookup :: Eq v => v -> [(k,v)] -> Maybe k
reverse_lookup k = fmap fst . find ((== k) . snd)

{- | Erroring variant. -}
reverse_lookup_err :: Eq v => v -> [(k,v)] -> k
reverse_lookup_err k = fromMaybe (error "reverse_lookup") . reverse_lookup k

{-
reverse_lookup :: Eq b => b -> [(a,b)] -> Maybe a
reverse_lookup key ls =
    case ls of
      [] -> Nothing
      (x,y):ls' -> if key == y then Just x else reverse_lookup key ls'
-}

{- | Erroring variant of 'find'. -}
find_err :: (t -> Bool) -> [t] -> t
find_err f = fromMaybe (error "find") . find f

{- | Basis of 'find_bounds_scl', indicates if /x/ is to the left or right of the list, and if to the right whether equal or not.
'Right' values will be correct if the list is not ascending, however 'Left' values only make sense for ascending ranges.

>>> map (find_bounds_cmp compare [(0,1),(1,2)]) [-1,0,1,2,3]
[Left ((0,1),GT),Right (0,1),Right (1,2),Left ((1,2),EQ),Left ((1,2),LT)]
-}
find_bounds_cmp :: (t -> s -> Ordering) -> [(t,t)] -> s -> Either ((t,t),Ordering) (t,t)
find_bounds_cmp f l x =
    let g (p,q) = f p x /= GT && f q x == GT
    in case l of
         [] -> error "find_bounds_cmp: nil"
         [(p,q)] -> if g (p,q) then Right (p,q) else Left ((p,q),f q x)
         (p,q):l' -> if f p x == GT
                     then Left ((p,q),GT)
                     else if g (p,q) then Right (p,q) else find_bounds_cmp f l' x

{- | Decide if value is nearer the left or right value of a range, return 'fst' or 'snd'. -}
decide_nearest_f :: Ord o => Bool -> (p -> o) -> (p,p) -> ((x,x) -> x)
decide_nearest_f bias_left f (p,q) =
  case compare (f p) (f q) of
    LT -> fst
    EQ -> if bias_left then fst else snd
    GT -> snd

{- | 'decide_nearest_f' with 'abs' of '-' as measure.

>>> (decide_nearest True 2 (1,3)) ("left","right")
"left"
-}
decide_nearest :: (Num o,Ord o) => Bool -> o -> (o,o) -> ((x,x) -> x)
decide_nearest bias_left x = decide_nearest_f bias_left (abs . (x -))

{- | /sel_f/ gets comparison key from /t/. -}
find_nearest_by :: (Ord n,Num n) => (t -> n) -> Bool -> [t] -> n -> t
find_nearest_by sel_f bias_left l x =
  let cmp_f i j = compare (sel_f i) j
  in case find_bounds_cmp cmp_f (adj2 1 l) x of
       Left ((p,_),GT) -> p
       Left ((_,q),_) -> q
       Right (p,q) -> decide_nearest bias_left x (sel_f p,sel_f q) (p,q)

{- | Find the number that is nearest the requested value in an ascending list of numbers.

>>> map (find_nearest_err True [0,3.5,4,7]) [-1,1,3,5,7,9]
[0.0,0.0,3.5,4.0,7.0,7.0]
-}
find_nearest_err :: (Num n,Ord n) => Bool -> [n] -> n -> n
find_nearest_err = find_nearest_by id

{- | 'find_nearest_err' allowing 'null' input list (which returns 'Nothing') -}
find_nearest :: (Num n,Ord n) => Bool -> [n] -> n -> Maybe n
find_nearest bias_left l x = if null l then Nothing else Just (find_nearest_err bias_left l x)

{- | Basis of 'find_bounds'.
There is an option to consider the last element specially, and if equal to the last span is given.
scl=special-case-last
-}
find_bounds_scl :: Bool -> (t -> s -> Ordering) -> [(t,t)] -> s -> Maybe (t,t)
find_bounds_scl scl f l x =
    case find_bounds_cmp f l x of
         Right r -> Just r
         Left (r,EQ) -> if scl then Just r else Nothing
         _ -> Nothing

{- | Find adjacent elements of list that bound element under given comparator.

>>> map (find_bounds True compare [1..5]) [0,1,3.5,5]
[Nothing,Just (1.0,2.0),Just (3.0,4.0),Just (4.0,5.0)]
-}
find_bounds :: Bool -> (t -> s -> Ordering) -> [t] -> s -> Maybe (t,t)
find_bounds scl f l = find_bounds_scl scl f (adj2 1 l)

{- | Special case of 'dropRight'.

>>> map drop_last ["","?","remove"]
["","","remov"]
-}
drop_last :: [t] -> [t]
drop_last l =
    case l of
      [] -> []
      [_] -> []
      e:l' -> e : drop_last l'

{- | Variant of 'drop' from right of list.

>>> dropRight 1 [1..9] == [1..8]
True
-}
dropRight :: Int -> [a] -> [a]
dropRight n = reverse . drop n . reverse

{- | Variant of 'dropWhile' from right of list.

>>> dropWhileRight Data.Char.isDigit "A440"
"A"
-}
dropWhileRight :: (a -> Bool) -> [a] -> [a]
dropWhileRight p = reverse . dropWhile p . reverse

{- | Data.List.dropWhileEnd, which however hugs doesn't know of. -}
drop_while_end :: (a -> Bool) -> [a] -> [a]
drop_while_end p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

{- | 'foldr' form of 'dropWhileRight'.

>>> drop_while_right Data.Char.isDigit "A440"
"A"
-}
drop_while_right :: (a -> Bool) -> [a] -> [a]
drop_while_right p = foldr (\x xs -> if p x && null xs then [] else x:xs) []

{- | 'take' from right.

>>> take_right 3 "taking"
"ing"
-}
take_right :: Int -> [a] -> [a]
take_right n = reverse . take n . reverse

{- | 'takeWhile' from right.

>>> takeWhileRight Data.Char.isDigit "A440"
"440"
-}
takeWhileRight :: (a -> Bool) -> [a] -> [a]
takeWhileRight p = reverse . takeWhile p . reverse

{- | 'foldr' form of 'takeWhileRight'.

>>> take_while_right Data.Char.isDigit "A440"
"440"
-}
take_while_right :: (a -> Bool) -> [a] -> [a]
take_while_right p =
  snd .
  foldr (\x xys -> (if p x && fst xys then bimap id (x:) else bimap (const False) id) xys) (True, [])

{- | Variant of 'take' that allows 'Nothing' to indicate the complete list.

>>> maybe_take (Just 5) [1 .. ] == [1 .. 5]
True

>>> maybe_take Nothing [1 .. 9] == [1 .. 9]
True
-}
maybe_take :: Maybe Int -> [a] -> [a]
maybe_take n l = maybe l (`take` l) n

{- | Take until /f/ is true.  This is not the same as 'not' at
     'takeWhile' because it keeps the last element. It is an error
     if the predicate never succeeds.

>>> take_until (== 'd') "tender"
"tend"

>>> takeWhile (not . (== 'd')) "tend"
"ten"

> take_until (== 'd') "seven" == undefined
-}
take_until :: (a -> Bool) -> [a] -> [a]
take_until f l =
  case l of
    [] -> error "take_until?"
    e:l' -> if f e then [e] else e : take_until f l'

{- | Apply /f/ at first element, and /g/ at all other elements.

>>> at_head negate id [1..5]
[-1,2,3,4,5]
-}
at_head :: (a -> b) -> (a -> b) -> [a] -> [b]
at_head f g x =
    case x of
      [] -> []
      e:x' -> f e : map g x'

{- | Apply /f/ at all but last element, and /g/ at last element.

>>> at_last (* 2) negate [1..4]
[2,4,6,-4]
-}
at_last :: (a -> b) -> (a -> b) -> [a] -> [b]
at_last f g x =
    case x of
      [] -> []
      [i] -> [g i]
      i:x' -> f i : at_last f g x'

{- | Separate list into an initial list and perhaps the last element tuple.

>>> separate_last' []
([],Nothing)
-}
separate_last' :: [a] -> ([a],Maybe a)
separate_last' x =
    case reverse x of
      [] -> ([],Nothing)
      e:x' -> (reverse x',Just e)

{- | Error on null input.

>>> separate_last [1..5] == ([1..4],5)
True
-}
separate_last :: [a] -> ([a],a)
separate_last = fmap (fromMaybe (error "separate_last")) . separate_last'

{- | Replace directly repeated elements with 'Nothing'.

>>> indicate_repetitions "abba"
[Just 'a',Just 'b',Nothing,Just 'a']
-}
indicate_repetitions :: Eq a => [a] -> [Maybe a]
indicate_repetitions =
    let f l = case l of
                [] -> []
                e:l' -> Just e : map (const Nothing) l'
    in concatMap f . group

{- | 'zipWith' of list and it's own tail.

>>> zip_with_adj (,) "abcde"
[('a','b'),('b','c'),('c','d'),('d','e')]
-}
zip_with_adj :: (a -> a -> b) -> [a] -> [b]
zip_with_adj f xs = zipWith f xs (tail_err xs)

{- | Type-specialised 'zip_with_adj'. -}
compare_adjacent_by :: (a -> a -> Ordering) -> [a] -> [Ordering]
compare_adjacent_by = zip_with_adj

{- | 'compare_adjacent_by' of 'compare'.

>>> compare_adjacent [0,1,3,2]
[LT,LT,GT]
-}
compare_adjacent :: Ord a => [a] -> [Ordering]
compare_adjacent = compare_adjacent_by compare

{- | Head and tail of list.  Useful to avoid "incomplete-uni-patterns" warnings.  It's an error if the list is empty. -}
headTail :: [a] -> (a, [a])
headTail l = (head_err l, tail_err l)

{- | Second element of list -}
second :: [t] -> t
second l = l !! 1

{- | First and second elements of list. Useful to avoid "incomplete-uni-patterns" warnings.  It's an error if the list has less than two elements. -}
firstSecond :: [t] -> (t, t)
firstSecond l = (l !! 0, l !! 1)

{- | 'Data.List.groupBy' does not make adjacent comparisons, it compares each new element to the start of the group.
This function is the adjacent variant.

>>> groupBy (<) [1,2,3,2,4,1,5,9]
[[1,2,3,2,4],[1,5,9]]

>>> adjacent_groupBy (<) [1,2,3,2,4,1,5,9]
[[1,2,3],[2,4],[1,5,9]]
-}
adjacent_groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
adjacent_groupBy f p =
    case p of
      [] -> []
      [x] -> [[x]]
      x:y:p' -> let r = adjacent_groupBy f (y:p')
                    (r0, r') = headTail r
                in if f x y
                   then (x:r0) : r'
                   else [x] : r

{- | Reduce sequences of consecutive values to ranges.

>>> group_ranges [-1,0,3,4,5,8,9,12]
[(-1,0),(3,5),(8,9),(12,12)]

>>> group_ranges [3,2,3,4,3]
[(3,3),(2,4),(3,3)]
-}
group_ranges :: (Num t, Eq t) => [t] -> [(t,t)]
group_ranges =
    let f l = (head_err l,last_err l)
    in map f . adjacent_groupBy (\p q -> p + 1 == q)

{- | 'groupBy' on /structure/ of 'Maybe', ie. all 'Just' compare equal.

>>> group_just [Just 1,Nothing,Nothing,Just 4,Just 5]
[[Just 1],[Nothing,Nothing],[Just 4,Just 5]]
-}
group_just :: [Maybe a] -> [[Maybe a]]
group_just = group_on isJust

{- | Predicate to determine if all elements of the list are '=='.

>>> all_equal "aaa"
True
-}
all_equal :: Eq a => [a] -> Bool
all_equal l =
    case l of
      [] -> True
      [_] -> True
      x:xs -> all (== x) xs

{- | Variant using 'nub'. -}
all_eq :: Eq n => [n] -> Bool
all_eq = (== 1) . length . nub

{- | 'nubBy' '==' 'on' /f/.

>>> nub_on snd (zip "ABCD" "xxyy")
[('A','x'),('C','y')]
-}
nub_on :: Eq b => (a -> b) -> [a] -> [a]
nub_on f = nubBy ((==) `on` f)

{- | 'group_on' of 'sortOn'.

>>> sort_group_on fst (zip "13123" "abcde")
[[('1','a'),('1','c')],[('2','d')],[('3','b'),('3','e')]]
-}
sort_group_on :: Ord b => (a -> b) -> [a] -> [[a]]
sort_group_on f = group_on f . sort_on f

{- | Maybe cons element onto list.

>>> Nothing `mcons` "something"
"something"

>>> Just 's' `mcons` "omething"
"something"
-}
mcons :: Maybe a -> [a] -> [a]
mcons e l = maybe l (:l) e

{- | Cons onto end of list.

>>> snoc 4 [1,2,3]
[1,2,3,4]
-}
snoc :: a -> [a] -> [a]
snoc e l = l ++ [e]

-- * Ordering

{- | Comparison function type. -}
type Compare_F a = a -> a -> Ordering

{- | If /f/ compares 'EQ', defer to /g/. -}
two_stage_compare :: Compare_F a -> Compare_F a -> Compare_F a
two_stage_compare f g p q =
    case f p q of
      EQ -> g p q
      r -> r

{- | 'compare' 'on' of 'two_stage_compare' -}
two_stage_compare_on :: (Ord i, Ord j) => (t -> i) -> (t -> j) -> t -> t -> Ordering
two_stage_compare_on f g = two_stage_compare (compare `on` f) (compare `on` g)

{- | Sequence of comparison functions, continue comparing until not EQ.

>>> compare (1,0) (0,1)
GT

>>> n_stage_compare [compare `on` snd,compare `on` fst] (1,0) (0,1)
LT
-}
n_stage_compare :: [Compare_F a] -> Compare_F a
n_stage_compare l p q =
    case l of
      [] -> EQ
      f:l' -> case f p q of
                EQ -> n_stage_compare l' p q
                r -> r

{- | 'compare' 'on' of 'two_stage_compare' -}
n_stage_compare_on :: Ord i => [t -> i] -> t -> t -> Ordering
n_stage_compare_on l = n_stage_compare (map (compare `on`) l)

{- | Sort sequence /a/ based on ordering of sequence /b/.

>>> sort_to "abc" [1,3,2]
"acb"

>>> sort_to "adbce" [1,4,2,3,5]
"abcde"
-}
sort_to :: Ord i => [e] -> [i] -> [e]
sort_to e = map fst . sort_on snd . zip e

{- | 'flip' of 'sort_to'.

>>> sort_to_rev [1,4,2,3,5] "adbce"
"abcde"
-}
sort_to_rev :: Ord i => [i] -> [e] -> [e]
sort_to_rev = flip sort_to

{- | 'sortBy' of 'two_stage_compare'. -}
sort_by_two_stage :: Compare_F a -> Compare_F a -> [a] -> [a]
sort_by_two_stage f g = sortBy (two_stage_compare f g)

{- | 'sortBy' of 'n_stage_compare'. -}
sort_by_n_stage :: [Compare_F a] -> [a] -> [a]
sort_by_n_stage f = sortBy (n_stage_compare f)

{- | 'sortBy' of 'two_stage_compare_on'. -}
sort_by_two_stage_on :: (Ord b,Ord c) => (a -> b) -> (a -> c) -> [a] -> [a]
sort_by_two_stage_on f g = sortBy (two_stage_compare_on f g)

{- | 'sortBy' of 'n_stage_compare_on'. -}
sort_by_n_stage_on :: Ord b => [a -> b] -> [a] -> [a]
sort_by_n_stage_on f = sortBy (n_stage_compare_on f)

{- | Given a comparison function, merge two ascending lists. Alias for 'O.mergeBy'

>>> merge_by compare [1,3,5] [2,4] == [1..5]
True
-}
merge_by :: Compare_F a -> [a] -> [a] -> [a]
merge_by = O.mergeBy

{- | 'merge_by' 'compare' 'on'. -}
merge_on :: Ord x => (a -> x) -> [a] -> [a] -> [a]
merge_on f = merge_by (compare `on` f)

{- | 'O.mergeBy' of 'two_stage_compare'. -}
merge_by_two_stage :: Ord b => (a -> b) -> Compare_F c -> (a -> c) -> [a] -> [a] -> [a]
merge_by_two_stage f cmp g = O.mergeBy (two_stage_compare (compare `on` f) (cmp `on` g))

{- | Alias for 'O.merge' -}
merge :: Ord a => [a] -> [a] -> [a]
merge = O.merge

{- | Merge list of sorted lists given comparison function.  Note that this is not equal to 'O.mergeAll'.
-}
merge_set_by :: (a -> a -> Ordering) -> [[a]] -> [a]
merge_set_by f = foldr (merge_by f) []

{- | 'merge_set_by' of 'compare'.

>>> merge_set [[1,3,5,7,9],[2,4,6,8],[10]] == [1..10]
True
-}
merge_set :: Ord a => [[a]] -> [a]
merge_set = merge_set_by compare

{-| 'merge_by' variant that joins (resolves) equal elements.

>>> let left p _ = p
>>> let right _ q = q
>>> let cmp = compare `on` fst
>>> let p = zip [1,3,5] "abc"
>>> let q = zip [1,2,3] "ABC"
>>> [merge_by_resolve left cmp p q, merge_by_resolve right cmp p q]
[[(1,'a'),(2,'B'),(3,'b'),(5,'c')],[(1,'A'),(2,'B'),(3,'C'),(5,'c')]]

>>> merge_by_resolve (\x _ -> x) (compare `on` fst) [(0,'A'),(1,'B'),(4,'E')] (zip [1..] "bcd")
[(0,'A'),(1,'B'),(2,'c'),(3,'d'),(4,'E')]
-}
merge_by_resolve :: (a -> a -> a) -> Compare_F a -> [a] -> [a] -> [a]
merge_by_resolve jn cmp =
    let recur p q =
            case (p,q) of
              ([],_) -> q
              (_,[]) -> p
              (l:p',r:q') -> case cmp l r of
                               LT -> l : recur p' q
                               EQ -> jn l r : recur p' q'
                               GT -> r : recur p q'
    in recur

{- | Merge two sorted (ascending) sequences.
Where elements compare equal, select element from left input.

>>> asc_seq_left_biased_merge_by (compare `on` fst) [(0,'A'),(1,'B'),(4,'E')] (zip [1..] "bcd")
[(0,'A'),(1,'B'),(2,'c'),(3,'d'),(4,'E')]
-}
asc_seq_left_biased_merge_by :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
asc_seq_left_biased_merge_by = merge_by_resolve const

{- | Find the first two adjacent elements for which /f/ is True.

>>> find_adj (>) [1,2,3,3,2,1]
Just (3,2)

>>> find_adj (>=) [1,2,3,3,2,1]
Just (3,3)
-}
find_adj :: (a -> a -> Bool) -> [a] -> Maybe (a,a)
find_adj f xs =
    case xs of
      p:q:xs' -> if f p q then Just (p,q) else find_adj f (q:xs')
      _ -> Nothing

{- | 'find_adj' of '>='

>>> filter is_ascending (words "A AA AB ABB ABC ABA") == words "A AB ABC"
True
-}
is_ascending :: Ord a => [a] -> Bool
is_ascending = isNothing . find_adj (>=)

{- | 'find_adj' of '>'

>>> filter is_non_descending (words "A AA AB ABB ABC ABA")
["A","AA","AB","ABB","ABC"]
-}
is_non_descending :: Ord a => [a] -> Bool
is_non_descending = isNothing . find_adj (>)

{- | Variant of `elem` that operates on a sorted list, halting.
This is 'O.member'.

>>> 16 `elem_ordered` [1,3 ..]
False

> 16 `elem` [1,3 ..] == undefined
-}
elem_ordered :: Ord t => t -> [t] -> Bool
elem_ordered = O.member

{- | Variant of `elemIndex` that operates on a sorted list, halting.

>>> 16 `elemIndex_ordered` [1,3 ..]
Nothing

>>> 16 `elemIndex_ordered` [0,1,4,9,16,25,36,49,64,81,100]
Just 4
-}
elemIndex_ordered :: Ord t => t -> [t] -> Maybe Int
elemIndex_ordered e =
    let recur k l =
            case l of
              [] -> Nothing
              x:l' -> if e == x
                      then Just k
                      else if x > e
                           then Nothing
                           else recur (k + 1) l'
    in recur 0

{- | 'zipWith' variant equivalent to 'mapMaybe' (ie. 'catMaybes' of 'zipWith') -}
zip_with_maybe :: (a -> b -> Maybe c) -> [a] -> [b] -> [c]
zip_with_maybe f lhs = catMaybes . zipWith f lhs

{- | 'zipWith' variant that extends shorter side using given value. -}
zip_with_ext :: t -> u -> (t -> u -> v) -> [t] -> [u] -> [v]
zip_with_ext i j f p q =
  case (p,q) of
    ([],_) -> map (f i) q
    (_,[]) -> map (`f` j) p
    (x:p',y:q') -> f x y : zip_with_ext i j f p' q'

{- | 'zip_with_ext' of ','

>>> let f = zip_ext 'i' 'j'
>>> f "" ""
[]

>>> f "p" ""
[('p','j')]

>>> f "" "q"
[('i','q')]

>>> f "pp" "q"
[('p','q'),('p','j')]

>>> f "p" "qq"
[('p','q'),('i','q')]
-}
zip_ext :: t -> u -> [t] -> [u] -> [(t,u)]
zip_ext i j = zip_with_ext i j (,)

{- | Keep right variant of 'zipWith', where unused rhs values are returned.

>>> zip_with_kr (,) [1..3] ['a'..'e']
([(1,'a'),(2,'b'),(3,'c')],"de")
-}
zip_with_kr :: (a -> b -> c) -> [a] -> [b] -> ([c],[b])
zip_with_kr f =
    let go r p q =
            case (p,q) of
              (i:p',j:q') -> go (f i j : r) p' q'
              _ -> (reverse r,q)
    in go []

{- | A 'zipWith' variant that always consumes an element from the left
hand side (lhs), but only consumes an element from the right hand
side (rhs) if the zip function is 'Right' and not if 'Left'.
There's also a secondary function to continue if the rhs ends
before the lhs.
-}
zip_with_perhaps_rhs :: (a -> b -> Either c c) -> (a -> c) -> [a] -> [b] -> [c]
zip_with_perhaps_rhs f g lhs rhs =
    case (lhs,rhs) of
      ([],_) -> []
      (_,[]) -> map g lhs
      (p:lhs',q:rhs') -> case f p q of
                           Left r -> r : zip_with_perhaps_rhs f g lhs' rhs
                           Right r -> r : zip_with_perhaps_rhs f g lhs' rhs'

{- | Zip a list with a list of lists.
Ordinarily the list has at least as many elements as there are elements at the list of lists.
There is also a Traversable form of this called 'adopt_shape_2_zip_stream'.

>>> zip_list_with_list_of_list [1 ..] ["a", "list", "of", "strings"]
[[(1,'a')],[(2,'l'),(3,'i'),(4,'s'),(5,'t')],[(6,'o'),(7,'f')],[(8,'s'),(9,'t'),(10,'r'),(11,'i'),(12,'n'),(13,'g'),(14,'s')]]

>>> zip_list_with_list_of_list [1 .. 9] ["a", "list", "of", "strings"]
[[(1,'a')],[(2,'l'),(3,'i'),(4,'s'),(5,'t')],[(6,'o'),(7,'f')],[(8,'s'),(9,'t')]]
-}
zip_list_with_list_of_list :: [p] -> [[q]] -> [[(p, q)]]
zip_list_with_list_of_list s l =
  case l of
    [] -> []
    e:l' ->
      let n = length e
      in zip (take n s) e : zip_list_with_list_of_list (drop n s) l'

{- | Fill gaps in a sorted association list, range is inclusive at both ends.

>>> fill_gaps_ascending' 'x' (1,9) (zip [1,5,7] "abc")
[(1,'a'),(2,'x'),(3,'x'),(4,'x'),(5,'b'),(6,'x'),(7,'c'),(8,'x'),(9,'x')]
-}
fill_gaps_ascending :: (Enum n, Ord n) => t -> (n,n) -> [(n,t)] -> [(n,t)]
fill_gaps_ascending def_e (l,r) =
    let f i (j,e) = if j > i then Left (i,def_e) else Right (j,e)
        g i = (i,def_e)
    in zip_with_perhaps_rhs f g [l .. r]

{- | Direct definition. -}
fill_gaps_ascending' :: (Num n,Enum n, Ord n) => t -> (n,n) -> [(n,t)] -> [(n,t)]
fill_gaps_ascending' def (l,r) =
    let recur n x =
            if n > r
            then []
            else case x of
                   [] -> zip [n .. r] (repeat def)
                   (m,e):x' -> if n < m
                               then (n,def) : recur (n + 1) x
                               else (m,e) : recur (n + 1) x'
    in recur l

{- | Variant with default value for empty input list case. -}
minimumBy_or :: t -> (t -> t -> Ordering) -> [t] -> t
minimumBy_or p f q = if null q then p else minimumBy f q

{- | 'minimum' and 'maximum' in one pass.

>>> minmax "minmax"
('a','x')
-}
minmax :: Ord t => [t] -> (t,t)
minmax inp =
    case inp of
      [] -> error "minmax: null"
      x:xs -> let mm p (l,r) = (min p l,max p r) in foldr mm (x,x) xs

{- | Append /k/ to the right of /l/ until result has /n/ places.
Truncates long input lists.

>>> map (pad_right '0' 2 . return) ['0' .. '9']
["00","10","20","30","40","50","60","70","80","90"]

>>> pad_right '0' 12 "1101"
"110100000000"

>>> map (pad_right ' '3) ["S","E-L"]
["S  ","E-L"]

>>> pad_right '!' 3 "truncate"
"tru"
-}
pad_right :: a -> Int -> [a] -> [a]
pad_right k n l = take n (l ++ repeat k)

{- | Variant that errors if the input list has more than /n/ places.

>>> map (pad_right_err '!' 3) ["x","xy","xyz"]
["x!!","xy!","xyz"]

> pad_right_err '!' 3 "xyz!" -- undefined
-}
pad_right_err :: t -> Int -> [t] -> [t]
pad_right_err k n l = if length l > n then error "pad_right_err?" else pad_right k n l

{- | Variant that will not truncate long inputs.

>>> pad_right_no_truncate '!' 3 "truncate"
"truncate"
-}
pad_right_no_truncate :: a -> Int -> [a] -> [a]
pad_right_no_truncate k n l = if length l > n then l else pad_right k n l

{- | Append /k/ to the left of /l/ until result has /n/ places.

>>> map (pad_left '0' 2 . return) ['0' .. '9']
["00","01","02","03","04","05","06","07","08","09"]
-}
pad_left :: a -> Int -> [a] -> [a]
pad_left k n l = replicate (n - length l) k ++ l

-- * Embedding

{- | Locate first (leftmost) embedding of /q/ in /p/.
Return partial indices for failure at 'Left'.

>>> embedding ("embedding","ming")
Right [1,6,7,8]

>>> embedding ("embedding","mind")
Left [1,6,7]
-}
embedding :: Eq t => ([t],[t]) -> Either [Int] [Int]
embedding =
    let recur n r (p,q) =
            case (p,q) of
              (_,[]) -> Right (reverse r)
              ([],_) -> Left (reverse r)
              (x:p',y:q') ->
                  let n' = n + 1
                      r' = if x == y then n : r else r
                  in recur n' r' (p',if x == y then q' else q)
    in recur 0 []

{- | 'fromRight' of 'embedding' -}
embedding_err :: Eq t => ([t],[t]) -> [Int]
embedding_err = either (error "embedding_err") id . embedding

{- | Does /q/ occur in sequence, though not necessarily adjacently, in /p/.

>>> is_embedding [1 .. 9] [1,3,7]
True

>>> is_embedding "embedding" "ming"
True

>>> is_embedding "embedding" "mind"
False
-}
is_embedding :: Eq t => [t] -> [t] -> Bool
is_embedding p q = T.is_right (embedding (p,q))

-- * Un-list

{- | Unpack one element list. -}
unlist1 :: [t] -> Maybe t
unlist1 l =
    case l of
      [e] -> Just e
      _ -> Nothing

{- | Erroring variant. -}
unlist1_err :: [t] -> t
unlist1_err = fromMaybe (error "unlist1") . unlist1

-- * Tree

{- | Given an 'Ordering' predicate where 'LT' opens a group, 'GT'
closes a group, and 'EQ' continues current group, construct tree
from list.

>>> let l = "a {b {c d} e f} g h i"
>>> let t = group_tree ((==) '{',(==) '}') l
>>> catMaybes (Tree.flatten t) == l
True

> let d = putStrLn . Tree.drawTree . fmap show
> d (group_tree ((==) '(',(==) ')') "a(b(cd)ef)ghi")
-}
group_tree :: (a -> Bool,a -> Bool) -> [a] -> Tree.Tree (Maybe a)
group_tree (open_f,close_f) =
    let unit e = Tree.Node (Just e) []
        nil = Tree.Node Nothing []
        insert_e (Tree.Node t l) e = Tree.Node t (e:l)
        reverse_n (Tree.Node t l) = Tree.Node t (reverse l)
        do_push (r,z) e =
            case z of
              h:z' -> (r,insert_e h (unit e) : z')
              [] -> (unit e : r,[])
        do_open (r,z) = (r,nil:z)
        do_close (r,z) =
            case z of
              h0:h1:z' -> (r,insert_e h1 (reverse_n h0) : z')
              h:z' -> (reverse_n h : r,z')
              [] -> (r,z)
        go st x =
            case x of
              [] -> Tree.Node Nothing (reverse (fst st))
              e:x' -> if open_f e
                      then go (do_push (do_open st) e) x'
                      else if close_f e
                           then go (do_close (do_push st e)) x'
                           else go (do_push st e) x'
    in go ([],[])

-- * Indexing

{- | Remove element at index.

>>> map (remove_ix 5) ["remove","removed"]
["remov","removd"]

> remove_ix 5 "short" -- error
-}
remove_ix :: Int -> [a] -> [a]
remove_ix k l = let (p,q) = splitAt k l in p ++ tail_err q

{- | Delete element at ix from list (c.f. remove_ix, this has a more specific error if index does not exist).

>>> delete_at 3 "deleted"
"delted"

> delete_at 8 "deleted" -- error
-}
delete_at :: (Eq t, Num t) => t -> [a] -> [a]
delete_at ix l =
  case (ix,l) of
    (_,[]) -> error "delete_at: index does not exist"
    (0,_:l') -> l'
    (_,e:l') -> e : delete_at (ix - 1) l'

{- | Select or remove elements at set of indices. -}
operate_ixs :: Bool -> [Int] -> [a] -> [a]
operate_ixs mode k =
    let sel = if mode then notElem else elem
        f (n,e) = if n `sel` k then Nothing else Just e
    in mapMaybe f . zip [0..]

{- | Select elements at set of indices.

>>> select_ixs [1,3] "select"
"ee"
-}
select_ixs :: [Int] -> [a] -> [a]
select_ixs = operate_ixs True

{- | Remove elements at set of indices.

>>> remove_ixs [1,3,5] "remove"
"rmv"
-}
remove_ixs :: [Int] -> [a] -> [a]
remove_ixs = operate_ixs False

{- | Replace element at /i/ in /p/ by application of /f/.

>>> replace_ix negate 1 [1..3]
[1,-2,3]
-}
replace_ix :: (a -> a) -> Int -> [a] -> [a]
replace_ix f i p =
    let (q,r) = splitAt i p
        (s,t) = headTail r
    in q ++ (f s : t)

{- | List equality, ignoring indicated indices.

>>> list_eq_ignoring_indices [3,5] "abcdefg" "abc.e.g"
True
-}
list_eq_ignoring_indices :: (Eq t,Integral i) => [i] -> [t] -> [t] -> Bool
list_eq_ignoring_indices x =
  let f n p q =
        case (p,q) of
          ([],[]) -> True
          ([],_) -> False
          (_,[]) -> False
          (p1:p',q1:q') -> (n `elem` x || p1 == q1) &&
                           f (n + 1) p' q'
  in f 0

{- | Edit list to have /v/ at indices /k/.
Replacement assoc-list must be ascending.
All replacements must be in range.

>>> list_set_indices [(2,'C'),(4,'E')] "abcdefg"
"abCdEfg"

>>> list_set_indices [] "abcdefg"
"abcdefg"

> list_set_indices [(9,'I')] "abcdefg" == undefined
-}
list_set_indices :: (Eq ix, Num ix) => [(ix,t)] -> [t] -> [t]
list_set_indices =
  let f n r l =
        case (r,l) of
          ([],_) -> l
          (_,[]) -> error "list_set_indices: out of range?"
          ((k,v):r',l0:l') -> if n == k
                              then v : f (n + 1) r' l'
                              else l0 : f (n + 1) r l'
  in f 0

{- | Variant of 'list_set_indices' with one replacement. -}
list_set_ix :: (Eq t, Num t) => t -> a -> [a] -> [a]
list_set_ix k v = list_set_indices [(k,v)]

{- | Cyclic indexing function.

>>> map (at_cyclic "cycle") [0..9]
"cyclecycle"
-}
at_cyclic :: [a] -> Int -> a
at_cyclic l n =
    let m = Map.fromList (zip [0..] l)
        k = Map.size m
        n' = n `mod` k
    in fromMaybe (error "cyc_at") (Map.lookup n' m)

{- | Index list from the end, assuming the list is longer than n + 1.

>>> atFromEnd [1 .. 30] 0
30

>>> atFromEnd [1..100] 15
85
-}
atFromEnd :: [t] -> Int -> t
atFromEnd lst n =
  let loop xs ys = last (zipWith const xs ys)
  in loop lst (drop n lst)

{- | Merge taking first element from left, allows inifite lists and lists that terminate.

>>> take 17 (merge_left_first [0..] [-1, -2 ..])
[0,-1,1,-2,2,-3,3,-4,4,-5,5,-6,6,-7,7,-8,8]

>>> take 13 (merge_left_first [1, 3 ..] [2, 4 .. 10])
[1,2,3,4,5,6,7,8,9,10,11,13,15]

>>> take 13 (merge_left_first [1, 3 .. 11] [2, 4 ..])
[1,2,3,4,5,6,7,8,9,10,11,12,14]
-}
merge_left_first :: [a] -> [a] -> [a]
merge_left_first p q =
  case (p, q) of
    ([], _) -> q
    (x:xs, _) -> x : merge_left_first q xs

{- | All pairs of p and q, allows inifite lists and lists that terminate.  (Norman Ramsey)
ml = merge-left

>>> take 11 (all_pairs_ml [1..] [1..])
[(1,1),(1,2),(2,2),(2,1),(2,3),(1,3),(3,3),(3,1),(3,2),(1,4),(3,4)]

>>> all_pairs_ml "ab" "cde"
[('a','c'),('a','d'),('b','d'),('b','c'),('b','e'),('a','e')]
-}
all_pairs_ml :: [a] -> [b] -> [(a, b)]
all_pairs_ml p q =
  case (p, q) of
    (_, []) -> []
    ([], _) -> []
    (a:as, b:bs) ->
      (a, b) : (([(a, b') | b' <- bs] `merge_left_first`
                 [(a', b) | a' <- as]) `merge_left_first`
                 all_pairs_ml as bs)

{- | All possible pairs of elements (/x/,/y/) where /x/ is from /p/ and /y/ from /q/.
Enumerated in the ordinary sequence.
lc = list-comprehension

>>> all_pairs_lc "ab" "cde"
[('a','c'),('a','d'),('a','e'),('b','c'),('b','d'),('b','e')]
-}
all_pairs_lc :: [t] -> [u] -> [(t,u)]
all_pairs_lc p q = [(x,y) | x <- p, y <- q]
