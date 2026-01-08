{- | <http://afriggeri.github.io/Ryb/> &
  <http://web.siat.ac.cn/~baoquan/papers/InfoVis_Paint.pdf>

_Paint Inspired Color Mixing and Compositing for Visualization_,
Nathan Gossett and Baoquan Chen, 2004
-}
module Music.Theory.Colour.Ryb where

import Data.Fixed {- base -}
import Data.List {- base -}

import qualified Data.Colour.SRGB as Srgb {- colour -}

import qualified Music.Theory.Tuple as Tuple {- hmt-base -}

type T3 t = Tuple.T3 t
type R = Double
type Ryb = T3 R
type Rgb = T3 R
type U8 = Int
type Rgb_U8 = T3 U8

ryb_clr :: [(String, [R])]
ryb_clr =
  [ ("white", [1, 1, 1])
  , ("red", [1, 0, 0])
  , ("yellow", [1, 1, 0])
  , ("blue", [0.163, 0.373, 0.6])
  , ("violet", [0.5, 0, 0.5])
  , ("green", [0, 0.66, 0.2])
  , ("orange", [1, 0.5, 0])
  , ("black", [0.2, 0.094, 0.0])
  ]

ryb_clr_ix :: String -> Int -> R
ryb_clr_ix nm ix = maybe (error "ryb_clr_ix") (!! ix) (lookup nm ryb_clr)

unit_to_u8 :: R -> U8
unit_to_u8 = floor . (* 255.0)

t3_unit_to_u8 :: T3 R -> T3 U8
t3_unit_to_u8 (p, q, r) = (unit_to_u8 p, unit_to_u8 q, unit_to_u8 r)

t3_round :: (RealFrac n, Integral i) => T3 n -> T3 i
t3_round = Tuple.t3_map round

rgb_to_u8 :: Rgb -> Rgb_U8
rgb_to_u8 = t3_unit_to_u8

{- | Ryb to Rgb

>>> map (rgb_to_u8 . ryb_to_rgb) [(0,0,0),(1,1,1),(1,0,0),(0,1,0),(0,0,1)]
[(255,255,255),(51,23,0),(255,0,0),(255,255,0),(41,95,153)]

>>> ryb_to_rgb (0,1,1) -- green
(0.0,0.66,0.2)

>>> ryb_to_rgb (0,0.5,1) -- cyan = blue+green = (0,1,2)
(8.15e-2,0.5165,0.4)
-}
ryb_to_rgb :: Ryb -> Rgb
ryb_to_rgb (r, y, b) =
  let f ix =
        ryb_clr_ix "white" ix * (1 - r) * (1 - b) * (1 - y)
          + ryb_clr_ix "red" ix * r * (1 - b) * (1 - y)
          + ryb_clr_ix "blue" ix * (1 - r) * b * (1 - y)
          + ryb_clr_ix "violet" ix * r * b * (1 - y)
          + ryb_clr_ix "yellow" ix * (1 - r) * (1 - b) * y
          + ryb_clr_ix "orange" ix * r * (1 - b) * y
          + ryb_clr_ix "green" ix * (1 - r) * b * y
          + ryb_clr_ix "black" ix * r * b * y
  in Tuple.t3_from_list (map f [0 .. 2])

int_to_r :: Int -> R
int_to_r = fromIntegral

{- | N triples

>>> map (^ 3) [2 .. 8]
[8,27,64,125,216,343,512]

>>> map n_triples [8,27,64,125,216,343,512]
[(2,8),(3,27),(4,64),(5,125),(6,216),(7,343),(8,512)]
-}
n_triples :: Int -> (Int, Int)
n_triples k =
  let b = ceiling (int_to_r k ** (1 / 3))
      n = b ^ (3::Int)
  in (b, n)

n_triples_r :: Int -> (R, R)
n_triples_r = Tuple.t2_map int_to_r . n_triples

{- | Gen triples (integers)

>>> gen_triples_i 8
[(0,0,0),(0,0,1),(0,1,0),(0,1,1),(1,0,0),(1,0,1),(1,1,0),(1,1,1)]

>>> gen_triples_i 27
[(0,0,0),(0,0,1),(0,0,2),(0,1,0),(0,1,1),(0,1,2),(0,2,0),(0,2,1),(0,2,2),(1,0,0),(1,0,1),(1,0,2),(1,1,0),(1,1,1),(1,1,2),(1,2,0),(1,2,1),(1,2,2),(2,0,0),(2,0,1),(2,0,2),(2,1,0),(2,1,1),(2,1,2),(2,2,0),(2,2,1),(2,2,2)]
-}
gen_triples_i :: Int -> [T3 Int]
gen_triples_i k =
  let (base, base_n) = n_triples_r k
      (%) = mod'
      f n =
        ( floor (n / (base * base))
        , floor ((n / base) % base)
        , floor (n % base)
        )
  in map f [0 .. base_n - 1]

{- | Gen triples

>>> gen_triples 8
[(0.0,0.0,0.0),(0.0,0.0,1.0),(0.0,1.0,0.0),(0.0,1.0,1.0),(1.0,0.0,0.0),(1.0,0.0,1.0),(1.0,1.0,0.0),(1.0,1.0,1.0)]

>>> gen_triples 27
[(0.0,0.0,0.0),(0.0,0.0,0.5),(0.0,0.0,1.0),(0.0,0.5,0.0),(0.0,0.5,0.5),(0.0,0.5,1.0),(0.0,1.0,0.0),(0.0,1.0,0.5),(0.0,1.0,1.0),(0.5,0.0,0.0),(0.5,0.0,0.5),(0.5,0.0,1.0),(0.5,0.5,0.0),(0.5,0.5,0.5),(0.5,0.5,1.0),(0.5,1.0,0.0),(0.5,1.0,0.5),(0.5,1.0,1.0),(1.0,0.0,0.0),(1.0,0.0,0.5),(1.0,0.0,1.0),(1.0,0.5,0.0),(1.0,0.5,0.5),(1.0,0.5,1.0),(1.0,1.0,0.0),(1.0,1.0,0.5),(1.0,1.0,1.0)]
-}
gen_triples :: Int -> [Ryb]
gen_triples k =
  let (base, _) = n_triples_r k
      f = Tuple.t3_map (\x -> fromIntegral x / (base - 1))
  in map f (gen_triples_i k)

{- | Distance function, square of Euclidian distance.

>>> map (distance [0,1,0]) [[1,1,0],[0,1,0],[1,1,1],[0.5,0.5,0.5]]
[1.0,0.0,2.0,0.75]
-}
distance :: Floating t => [t] -> [t] -> t
distance p1 =
  let f x1 x2 = (x2 - x1) ** 2
  in sum . zipWith f p1

{- | Distance function at T3

>>> map (distance_t3 (0,1,0)) [(1,1,0),(0,1,0),(1,1,1),(0.5,0.5,0.5)]
[1.0,0.0,2.0,0.75]
-}
distance_t3 :: Floating t => T3 t -> T3 t -> t
distance_t3 (p1, p2, p3) (q1, q2, q3) =
  let f x1 x2 = (x2 - x1) ** 2
  in f p1 q1 + f p2 q2 + f p3 q3

-- | Sum of distance_t3 from x to each item in l.
distance_t3_set :: Floating t => [T3 t] -> T3 t -> t
distance_t3_set l x = sum (map (distance_t3 x) l)

{- | Answer the most distant item in l from x,
according to the distance function f and the reduction function g,
and also the remainder of l.

>>> fst (most_distant_item distance_t3 minimum [(0,0,0)] (gen_triples 8))
(1.0,1.0,1.0)
-}
most_distant_item :: (Eq t, Ord a) => (t -> t -> a) -> ([a] -> a) -> [t] -> [t] -> (t, [t])
most_distant_item f g x l =
  let d = map (\y -> g (map (f y) x)) l
      z = maximum d
  in case find ((== z) . fst) (zip d l) of
      Just (_, e) -> (e, delete e l)
      Nothing -> error "most_distant_item?"

farthest_first_traversal :: (Eq t, Ord a) => (t -> t -> a) -> ([a] -> a) -> [t] -> [t]
farthest_first_traversal f g l =
  let step lhs rhs =
        case rhs of
          [] -> []
          _ -> let (e, rhs') = most_distant_item f g lhs rhs
               in e : step (e : lhs) rhs'
  in case l of
       e : l' -> e : step [e] l'
       _ -> error "farthest_first_traversal"

{- | Distance sort

>>> map t3_round (distance_sort (gen_triples 8))
[(0,0,0),(1,1,1),(0,0,1),(1,1,0),(0,1,0),(1,0,1),(0,1,1),(1,0,0)]

>>> map (rgb_to_u8 . ryb_to_rgb) (distance_sort (gen_triples 8))
[(255,255,255),(51,23,0),(41,95,153),(255,127,0),(255,255,0),(127,0,127),(0,168,51),(255,0,0)]

>>> let a = gen_triples 27
>>> let b = map (Tuple.t3_map (* 2)) a
>>> map t3_round (distance_sort b)
[(0,0,0),(2,2,2),(0,0,2),(2,2,0),(0,2,0),(2,0,2),(0,2,2),(2,0,0),(0,0,1),(2,2,1),(0,1,0),(2,1,2),(0,1,2),(2,1,0),(0,2,1),(2,0,1),(1,0,0),(1,2,2),(1,0,2),(1,2,0),(0,1,1),(2,1,1),(1,0,1),(1,2,1),(1,1,0),(1,1,2),(1,1,1)]

>>> let a = gen_triples 64
>>> let b = map (Tuple.t3_map (* 3)) a
>>> map t3_round (distance_sort b)
[(0,0,0),(3,3,3),(0,0,3),(3,3,0),(0,3,0),(3,0,3),(0,3,3),(3,0,0),(0,0,1),(3,3,2),(0,0,2),(3,3,1),(0,1,0),(3,2,3),(0,1,3),(3,2,0),(0,2,0),(3,1,3),(0,2,3),(3,1,0),(0,3,1),(3,0,2),(0,3,2),(3,0,1),(1,0,0),(2,3,3),(1,0,3),(2,3,0),(1,3,0),(2,0,3),(1,3,3),(2,0,0),(0,1,1),(3,2,2),(0,1,2),(3,2,1),(0,2,1),(3,1,2),(0,2,2),(3,1,1),(1,0,1),(2,3,2),(1,0,2),(2,3,1),(1,1,0),(2,2,3),(1,1,3),(2,2,0),(1,2,0),(2,1,3),(1,2,3),(2,1,0),(1,3,1),(2,0,2),(1,3,2),(2,0,1),(1,1,1),(2,2,2),(1,1,2),(2,2,1),(1,2,1),(2,1,2),(1,2,2),(2,1,1)]
-}
distance_sort :: (Ord t, Floating t) => [(t, t, t)] -> [(t, t, t)]
distance_sort = farthest_first_traversal distance_t3 sum

{- | Ryb colour gen

>>> ryb_colour_gen 8 == [(0,0,0),(1,1,1),(0,0,1),(1,1,0),(0,1,0),(1,0,1),(0,1,1),(1,0,0)]
True
-}
ryb_colour_gen :: Int -> [Ryb]
ryb_colour_gen = distance_sort . gen_triples

-- | ryb_to_rgb of ryb_colour_gen
rgb_colour_gen :: Int -> [Rgb]
rgb_colour_gen = map ryb_to_rgb . ryb_colour_gen

{- | rgb_to_u8 of rgb_colour_gen

>>> rgb_u8_colour_gen 27
[(255,255,255),(51,23,0),(41,95,153),(255,127,0),(255,255,0),(127,0,127),(0,168,51),(255,0,0),(148,175,204),(153,75,0),(255,255,127),(89,11,63),(20,131,102),(255,63,0),(127,211,25),(191,0,63),(255,127,127),(25,96,25),(84,47,140),(255,191,0),(137,193,114),(172,37,31),(169,87,133),(140,143,12),(255,159,63),(55,71,82),(155,115,73)]
-}
rgb_u8_colour_gen :: Int -> [Rgb_U8]
rgb_u8_colour_gen = map rgb_to_u8 . rgb_colour_gen

colour_gen :: Int -> [Srgb.Colour R]
colour_gen = map (\(r, g, b) -> Srgb.sRGB r g b) . rgb_colour_gen
