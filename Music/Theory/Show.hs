-- | Show functions.
module Music.Theory.Show where

import Data.Char {- base -}
import Data.Ratio {- base -}
import Numeric {- base -}

import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Math as Math {- hmt-base -}
import qualified Music.Theory.Math.Convert as Math.Convert {- hmt-base -}

-- * Difference

{- | Show positive and negative values always with sign, maybe show zero, maybe right justify.

>>> map (num_diff_str_opt (True,2)) [-2,-1,0,1,2]
["-2","-1"," 0","+1","+2"]
-}
num_diff_str_opt :: (Ord a, Num a, Show a) => (Bool,Int) -> a -> String
num_diff_str_opt (wr_0,k) n =
  let r = case compare n 0 of
            LT -> '-' : show (abs n)
            EQ -> if wr_0 then "0" else ""
            GT -> '+' : show n
  in if k > 0 then List.pad_left ' ' k r else r

{- | Show /only/ positive and negative values, always with sign.

>>> map num_diff_str [-2,-1,0,1,2]
["-2","-1","","+1","+2"]

>>> map show [-2,-1,0,1,2]
["-2","-1","0","1","2"]
-}
num_diff_str :: (Num a, Ord a, Show a) => a -> String
num_diff_str = num_diff_str_opt (False,0)

-- * Rational

{- | Pretty printer for 'Rational' using @/@.
If /conciseInteger/ is True eliding denominators of @1@.

>>> map (rational_pp_opt True) [1,3/2,5/4,2]
["1","3/2","5/4","2"]
-}
rational_pp_opt :: (Show a,Integral a) => Bool -> Ratio a -> String
rational_pp_opt conciseInteger r =
    let n = numerator r
        d = denominator r
    in if d == 1 && conciseInteger
       then show n
       else concat [show n,"/",show d]

rational_pp :: (Show a,Integral a) => Ratio a -> String
rational_pp = rational_pp_opt False

{- | Pretty print ratio as @:@ separated integers.
If /conciseInteger/ is True elide unit denominator.

>>> map (ratio_pp_opt True) [1,3/2,2]
["1","3:2","2"]
-}
ratio_pp_opt :: Bool -> Rational -> String
ratio_pp_opt nil r =
  let f :: (Integer,Integer) -> String
      f (n,d) = concat [show n,":",show d]
  in case Math.rational_nd r of
       (n,1) -> if nil then show n else f (n,1)
       x -> f x

{- | Pretty print ratio as @:@ separated integers.

>>> map ratio_pp [1,3/2,2]
["1:1","3:2","2:1"]
-}
ratio_pp :: Rational -> String
ratio_pp = ratio_pp_opt False

{- | Show rational to /n/ decimal places.

>>> let r = approxRational pi 1e-100
>>> r == 884279719003555 / 281474976710656
True

>>> show_rational_decimal 12 (884279719003555 / 281474976710656)
"3.141592653590"

>>> show_rational_decimal 3 (-100)
"-100.000"
-}
show_rational_decimal :: Int -> Rational -> String
show_rational_decimal n = double_pp n . fromRational

-- * Real

{- | Show /r/ as float to /k/ places.

>>> real_pp 4 (1/3 :: Rational)
"0.3333"

>>> map (real_pp 4) [1,1.1,1.12,1.123,1.1234,1/0,sqrt (-1)]
["1.0000","1.1000","1.1200","1.1230","1.1234","Infinity","-Infinity"]
-}
real_pp :: Real t => Int -> t -> String
real_pp k = realfloat_pp k . Math.Convert.real_to_double

{- | Print as exact integer if whole to indicated precision.

>>> map (real_pp_exact 4) [1.0, 1.0010, 1.00001]
["1","1.0010","1"]
-}
real_pp_exact :: RealFrac t => Int -> t -> String
real_pp_exact k r =
  if Math.whole_to_precision k r
  then show (truncate r)
  else real_pp k r

{- | Variant that writes `∞` for Infinity.

> putStrLn $ unwords $ map (real_pp_unicode 4) [1/0,-1/0]
-}
real_pp_unicode :: Real t => Int -> t -> [Char]
real_pp_unicode k r =
  case real_pp k r of
    "Infinity" -> "∞"
    "-Infinity" -> "-∞"
    s -> s

{- | Prints /n/ as integral or to at most /k/ decimal places. Does not print -0.

>>> real_pp_trunc 4 (1/3 :: Rational)
"0.3333"

>>> map (real_pp_trunc 4) [1,1.1,1.12,1.123,1.1234]
["1","1.1","1.12","1.123","1.1234"]

>>> map (real_pp_trunc 4) [1.00009,1.00001]
["1.0001","1"]

>>> map (real_pp_trunc 2) [59.999,60.001,-0.00,-0.001]
["60","60","0","0"]
-}
real_pp_trunc :: Real t => Int -> t -> String
real_pp_trunc k n =
  case break (== '.') (real_pp k n) of
    (i,[]) -> i
    (i,j) -> case List.drop_while_end (== '0') j of
               "." -> if i == "-0" then "0" else i
               z -> i ++ z

{- | Variant of 'showFFloat'.
The 'Show' instance for floats resorts to exponential notation very readily.
k is the precision given as number of decimal places.

>>> [show 0.01,realfloat_pp 2 0.01]
["1.0e-2","0.01"]

>>> map (realfloat_pp 4) [1,1.1,1.12,1.123,1.1234,1/0,sqrt (-1)]
["1.0000","1.1000","1.1200","1.1230","1.1234","Infinity","NaN"]

>>> realfloat_pp 3 pi
"3.142"
-}
realfloat_pp :: RealFloat a => Int -> a -> String
realfloat_pp k n = showFFloat (Just k) n ""

-- | Type specialised 'realfloat_pp'.
float_pp :: Int -> Float -> String
float_pp = realfloat_pp

{- | Type specialised 'realfloat_pp'.

>>> double_pp 4 0
"0.0000"
-}
double_pp :: Int -> Double -> String
double_pp = realfloat_pp

-- * Binary

{- | Read binary integer.

>>> unwords (map (show_bin Nothing) [0 .. 7])
"0 1 10 11 100 101 110 111"

>>> unwords (map (show_bin (Just 3)) [0 .. 7])
"000 001 010 011 100 101 110 111"
-}
show_bin :: (Integral i,Show i) => Maybe Int -> i -> String
show_bin k n = maybe id (List.pad_left '0') k (showIntAtBase 2 intToDigit n "")
