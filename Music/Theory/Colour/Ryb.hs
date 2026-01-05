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

type R = Double
type Ryb = (R, R, R)
type Rgb = (R, R, R)
type U8 = Int
type Rgb_U8 = (U8, U8, U8)

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

t3_unit_to_u8 :: (R, R, R) -> (U8, U8, U8)
t3_unit_to_u8 (p, q, r) = (unit_to_u8 p, unit_to_u8 q, unit_to_u8 r)

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

{- | Euclidian distance

>>> map (euclidian_distance [0,1,0]) [[1,1,0],[0,1,0],[1,1,1],[0.5,0.5,0.5]]
[1.0,0.0,2.0,0.75]
-}
euclidian_distance :: Floating t => [t] -> [t] -> t
euclidian_distance p1 =
  let f x1 x2 = (x2 - x1) ** 2
  in sum . zipWith f p1

euclidian_distance_t3 :: Floating t => (t, t, t) -> (t, t, t) -> t
euclidian_distance_t3 (p1, p2, p3) (q1, q2, q3) =
  let f x1 x2 = (x2 - x1) ** 2
  in f p1 q1 + f p2 q2 + f p3 q3

euclidian_distance_t3_set :: Floating t => [(t, t, t)] -> (t, t, t) -> t
euclidian_distance_t3_set l x = sum (map (euclidian_distance_t3 x) l)

int_to_r :: Int -> R
int_to_r = fromIntegral

{- | N triples

>>> map n_triples [8,27,64,125,216,343,512]
[(2.0,8.0),(3.0,27.0),(4.0,64.0),(5.0,125.0),(6.0,216.0),(7.0,343.0),(8.0,512.0)]
-}
n_triples :: Int -> (R, R)
n_triples k =
  let fceil = int_to_r . ceiling
      b = fceil (int_to_r k ** (1 / 3))
      n = (b ** 3)
  in (b, n)

{- | Gen triples

>>> gen_triples 8 == [(0,0,0),(0,0,1),(0,1,0),(0,1,1),(1,0,0),(1,0,1),(1,1,0),(1,1,1)]
True
-}
gen_triples :: Int -> [Ryb]
gen_triples k =
  let ffloor = int_to_r . floor
      (base, base_n) = n_triples k
      (%) = mod'
      f n =
        ( ffloor (n / (base * base)) / (base - 1)
        , ffloor ((n / base) % base) / (base - 1)
        , ffloor (n % base) / (base - 1)
        )
  in map f [0 .. base_n - 1]

-- > most_distant_set [(0,0,0)] (gen_triples 8)
most_distant_set :: (Ord t, Floating t) => [(t, t, t)] -> [(t, t, t)] -> ((t, t, t), [(t, t, t)])
most_distant_set x l =
  let d = map (euclidian_distance_t3_set x) l
      z = maximum d
  in case find ((== z) . fst) (zip d l) of
      Just (_, e) -> (e, delete e l)
      Nothing -> error "most_distant_set"

distance_step :: (Ord t, Floating t) => [(t, t, t)] -> [(t, t, t)] -> [(t, t, t)]
distance_step lhs rhs =
  case rhs of
    [] -> []
    _ ->
      let (e, rhs') = most_distant_set lhs rhs
      in e : distance_step (e : lhs) rhs'

{- | Distance sort

>>> map (rgb_to_u8 . ryb_to_rgb) (distance_sort (gen_triples 8))
[(255,255,255),(51,23,0),(41,95,153),(255,127,0),(255,255,0),(127,0,127),(0,168,51),(255,0,0)]
-}
distance_sort :: (Ord t, Floating t) => [(t, t, t)] -> [(t, t, t)]
distance_sort l =
  case l of
    e : l' -> e : distance_step [e] l'
    _ -> error "distance_sort"

{- | Ryb colour gen

>>> ryb_colour_gen 8 == [(0,0,0),(1,1,1),(0,0,1),(1,1,0),(0,1,0),(1,0,1),(0,1,1),(1,0,0)]
True
-}
ryb_colour_gen :: Int -> [Ryb]
ryb_colour_gen = distance_sort . gen_triples

rgb_colour_gen :: Int -> [Rgb]
rgb_colour_gen = map ryb_to_rgb . ryb_colour_gen

-- > rgb_u8_colour_gen 27
rgb_u8_colour_gen :: Int -> [Rgb_U8]
rgb_u8_colour_gen = map rgb_to_u8 . rgb_colour_gen

colour_gen :: Int -> [Srgb.Colour R]
colour_gen = map (\(r, g, b) -> Srgb.sRGB r g b) . rgb_colour_gen
