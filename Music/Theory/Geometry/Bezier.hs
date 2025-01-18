-- | Bezier functions.
module Music.Theory.Geometry.Bezier where

import Music.Theory.Geometry.Vector {- hmt-base -}

import qualified Music.Theory.Geometry.Matrix as Matrix {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}

-- | Given quadratic parameters (q1,q2,q3) generate cubic parameters (c1,c2,c3,c4)
bezier_quadratic_to_cubic :: Fractional n => V3 (V2 n) -> V4 (V2 n)
bezier_quadratic_to_cubic (q1, q2, q3) =
  let c1 = q1
      c2 = v2_add q1 (v2_scale (2 / 3) (v2_sub q2 q1))
      c3 = v2_add q3 (v2_scale (2 / 3) (v2_sub q2 q3))
      c4 = q3
  in (c1, c2, c3, c4)

{- | Calculate B(i,n) at t.

>>> bernstein_base (3, 4) 0.5
0.25

>>> let f = map (\t -> map (\b -> bernstein_base b t) [(0,3),(1,3),(2,3),(3,3)])
>>> f [0,0.5,1] == [[1,0,0,0],[1/8,6/16,6/16,1/8],[0,0,0,1]]
True

> import Sound.Sc3.Plot
> f = map (\b -> map (\t -> bernstein_base b t) [0,0.01 .. 1])
> b k = let i = k - 1 in (zip [0 .. i] (repeat i))
> plot_p1_ln (f (b 5))
> plot_p1_ln (f (b 36))
-}
bernstein_base :: (Integral i, Num n) => (i, i) -> n -> n
bernstein_base (i, n) t =
  let f x = if x <= 1 then 1 else x * f (x - 1) -- f = factorial
      c x y = (f x `div` f (x - y)) `div` (f y) -- c = nk-combinations (binomial coefficient)
      r = fromIntegral
  in r (n `c` i) * (t ^ i) * ((1 - t) ^ (n - i))

{- | Calculate bezier curve at /t/ for sequence /p/.  /t/ is in (0, 1).

> import Sound.Sc3.Plot
> let p = [16,115,212,312,273]
> let q = [106,182,0,75,151]
> let f x = map (bezier_curve x) [0,0.01 .. 1]
> plot_p1_ln [f p, f q]
-}
bezier_curve :: Num a => [a] -> a -> a
bezier_curve p t =
  let n = length p - 1
  in sum (zipWith (*) p (map (\i -> bernstein_base (i, n) t) [0 .. n]))

{- | 'bezier_curve' at 'V2'.

> import Sound.Sc3.Plot
> let f z = map (bezier_curve_v2 z) [0,0.01 .. 1]
> let p = [(16,106),(115,182),(212,0),(312,75),(273,151)]
> plot_p2_ln [p,f p]

> let q = [(0,0),(1,1),(2,-1),(3,0),(5,2),(6,-1),(7,3)]
> plot_p2_ln [q,f (take 4 q) ++ f (drop 3 q)]
-}
bezier_curve_v2 :: Num a => [V2 a] -> a -> V2 a
bezier_curve_v2 p t = let (x, y) = unzip p in (bezier_curve x t, bezier_curve y t)

{- | Cubic Bernstein bases, ie. B(0,3) B(1,3) B(2,3) and B(3,3)

>>> map bezier4_bases [0,0.5,1] == [(1,0,0,0),(1/8,6/16,6/16,1/8),(0,0,0,1)]
True

>>> import Data.Ratio
>>> map bezier4_bases [1%4,1%3]
[(27 % 64,27 % 64,9 % 64,1 % 64),(8 % 27,4 % 9,2 % 9,1 % 27)]

> import Sound.Sc3.Plot
> (p,q,r,s) = Data.List.unzip4 (map bezier4_bases [0,0.01 .. 1])
> plot_p1_ln [p,q,r,s]
-}
bezier4_bases :: Num n => n -> V4 n
bezier4_bases t =
  let sqr x = x * x
      cub x = x * x * x
  in ( cub (1 - t) -- B(0,3)
     , 3 * sqr (1 - t) * t -- B(1,3)
     , 3 * (1 - t) * sqr t -- B(2,3)
     , cub t -- B(3,3)
     )

bezier4 :: Num n => V4 n -> n -> n
bezier4 p t = v4_foldl (+) (v4_mul p (bezier4_bases t))

bezier4_v2 :: Num n => V4 (V2 n) -> n -> V2 n
bezier4_v2 ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) mu =
  (bezier4 (x1, x2, x3, x4) mu
  ,bezier4 (y1, y2, y3, y4) mu)

bezier4_v3 :: Num n => V4 (V3 n) -> n -> V3 n
bezier4_v3 ((x1, y1, z1), (x2, y2, z2), (x3, y3, z3), (x4, y4, z4)) mu =
  ( bezier4 (x1, x2, x3, x4) mu
  , bezier4 (y1, y2, y3, y4) mu
  , bezier4 (z1, z2, z3, z4) mu
  )

-- | 4x4 matrix that, applied to (1,t,t*t,t*t*t) gives the bezier4 bases.
bezier4_m44 :: Num n => Matrix.M44 n
bezier4_m44 = ((1, -3, 3, -1), (0, 3, -6, 3), (0, 0, 3, -3), (0, 0, 0, 1))

{- | Bezier bases, matrix form.

map bezier4_bases_mtx [0,1/2,1] == map bezier4_bases [0,1/2,1]
zipWith v4_sub (map bezier4_bases_mtx [1/4,1/3]) (map bezier4_bases [1/4,1/3])
-}
bezier4_bases_mtx :: Num a => a -> V4 a
bezier4_bases_mtx t = Matrix.m44_apply bezier4_m44 (1, t, t * t, t * t * t)

{- | Matrix form of 'bezier4'.

> let mk f = map (f (0,180,0,60)) [0,0.01 .. 1]
> plot_p1_ln [mk bezier4,mk bezier4_mtx]
-}
bezier4_mtx :: Num n => V4 n -> n -> n
bezier4_mtx p t = v4_foldl (+) (v4_mul p (bezier4_bases_mtx t))

-- | Generic variant, given a scaling function, ie. 'pt_scale' or 'p3_scale'.
bezier4_generic :: (Num a, Num t) => (a -> t -> t) -> t -> t -> t -> t -> a -> t
bezier4_generic f p0 p1 p2 p3 t =
  let u = 1 - t
      tt = t * t
      ttt = tt * t
      uu = u * u
      uuu = uu * u
  in (uuu `f` p0) + ((3 * uu * t) `f` p1) + ((3 * u * tt) `f` p2) + (ttt `f` p3)

-- | Three-point (quadratic) bezier curve interpolation.  The index /mu/ is in the range zero to one.
bezier3_v2_direct :: Num n => V2 n -> V2 n -> V2 n -> n -> V2 n
bezier3_v2_direct (x1, y1) (x2, y2) (x3, y3) mu =
  let a = mu * mu
      b = 1 - mu
      c = b * b
      x = x1 * c + 2 * x2 * b * mu + x3 * a
      y = y1 * c + 2 * y2 * b * mu + y3 * a
  in (x, y)

-- | Four-point (cubic) bezier curve interpolation.  The index /mu/ is in the range zero to one.
bezier4_v2_direct :: Num n => V2 n -> V2 n -> V2 n -> V2 n -> n -> V2 n
bezier4_v2_direct (x1, y1) (x2, y2) (x3, y3) (x4, y4) mu =
  let a = 1 - mu
      b = a * a * a
      c = mu * mu * mu
      x = b * x1 + 3 * mu * a * a * x2 + 3 * mu * mu * a * x3 + c * x4
      y = b * y1 + 3 * mu * a * a * y2 + 3 * mu * mu * a * y3 + c * y4
  in (x, y)

-- | Tuple variant of 'bezier3_v2_direct'
bezier_quadratic_v2 :: Num n => V3 (V2 n) -> n -> V2 n
bezier_quadratic_v2 (p1, p2, p3) = bezier3_v2_direct p1 p2 p3

unwrap_mu :: Fractional a => a -> a -> a -> a
unwrap_mu x0 x3 mu = let d = x3 - x0 in (mu - x0) / d

next_mu :: Fractional a => a -> a -> a
next_mu x0 x3 = x0 + ((x3 - x0) / 2)

{- | Requires that x is monotonic (increasing), so that a simple
binary search will work.  Initial /mu/ allows fast forwarding to
last result for left to right lookup (unused in variants here).
-}
bezier4_y_mt' :: (Ord a, Fractional a) => a -> V2 a -> V2 a -> V2 a -> V2 a -> a -> a -> (a, a)
bezier4_y_mt' dx (x0, y0) (x1, y1) (x2, y2) (x3, y3) i_mu x =
  let f = bezier4_v2 ((x0, y0), (x1, y1), (x2, y2), (x3, y3))
      recur l r mu =
        let (x', y') = f (unwrap_mu x0 x3 mu)
        in if abs (x - x') <= dx
            then (mu, y')
            else
              let (l', r') = if x' < x then (mu, r) else (l, mu)
                  mu' = next_mu l' r'
              in recur l' r' mu'
  in recur x0 x3 i_mu

{- | Variant with initial /mu/ at mid-point.

> import Sound.Sc3.Plot

> let f = bezier4_y_mt 0.0001 (0,0) (0.1,0.3) (0.5,0.2) (1,0.5)
> plot_p1_ln [map (snd . f) [0.0,0.01 .. 1.0]]

> let f = bezier4_y_mt 0.0001 (0,0) (0.4,1.3) (0.6,1.3) (1,0)
> plot_p1_ln [map (snd . f) [0.0,0.01 .. 1.0]]

> let f = bezier4_y_mt 0.0001 (1,0) (1.4,-1.3) (1.6,-1.3) (2,0)
> plot_p1_ln [map (snd . f) [1.0,1.01 .. 2.0]]
-}
bezier4_y_mt :: (Ord a, Fractional a) => a -> V2 a -> V2 a -> V2 a -> V2 a -> a -> (a, a)
bezier4_y_mt dx p0 c1 c2 p3 x =
  let (x0, _) = p0
      (x3, _) = p3
      mu = x0 + ((x3 - x0) / 2)
  in bezier4_y_mt' dx p0 c1 c2 p3 mu x

-- | Variant that scans a list [p0,c1,c2,p3,c4,c5,p6...] to resolve /x/.
bezier4_seq_y' :: (Ord a, Fractional a) => a -> [V2 a] -> a -> Maybe (a, a)
bezier4_seq_y' dx pt_l x =
  case pt_l of
    p0 : c1 : c2 : p3 : pt_l' ->
      if x >= v2_x p0 && x <= v2_x p3
        then Just (bezier4_y_mt dx p0 c1 c2 p3 x)
        else bezier4_seq_y' dx (p3 : pt_l') x
    _ -> Nothing

-- | Variant giving only /y/ and zero for out of range /x/.
bezier4_seq_y0 :: (Ord a, Fractional a) => a -> [V2 a] -> a -> a
bezier4_seq_y0 dx pt = maybe 0 snd . bezier4_seq_y' dx pt

{- | Variant to generate a /wavetable/, ie. /n/ equally spaced /x/ across range of /pt/.

> let pt = [(0,0),(0.4,-1.3),(0.6,-1.3),(1,0),(1.4,1.3),(1.6,1.3),(2,0)]
> plot_p1_ln [bezier4_seq_wt 1e-4 pt 1024]
-}
bezier4_seq_wt :: (Ord a, Integral i, Fractional a, Enum a) => a -> [V2 a] -> i -> [a]
bezier4_seq_wt dx pt n =
  let (l, _) = List.head_err pt
      (r, _) = List.last_err pt
      l' = l + ((r - l) / fromIntegral n)
      ix = [l, l' .. r]
  in map (bezier4_seq_y0 dx pt) ix
