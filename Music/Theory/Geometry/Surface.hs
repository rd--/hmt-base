-- | Surface functions, V2->V3
module Music.Theory.Geometry.Surface where

import Music.Theory.Math (sech) {- hmt-base -}
import Music.Theory.Geometry.Vector (V2, V3) {- hmt-base -}

-- | Quieten compiler
(^.) :: Num t => t -> Int -> t
p ^. q = p ^ q

-- | V2 -> V3
type Surface t = V2 t -> V3 t

-- | Möbius strip, 0 <= u < 2 pi, -1 <= v < 1
moebius :: Floating t => t -> Surface t
moebius r (u, v) =
  let a = r + ((v / 2) * cos (u / 2))
      x = a * cos u
      y = a * sin u
      z = (v / 2) * sin (u / 2)
  in (x, y, z)

-- | 0 <= u < pi, 0 <= v < 2pi
klein :: Floating t => Surface t
klein (u, v) =
  let x = (-2 / 15) * cos (u) * (3 * cos (v) - 30 * sin (u) + 90 * cos (u) ^. 4 * sin (u) - 60 * cos (u) ^. 6 * sin (u) + 5 * cos (u) * cos (v) * sin (u))
      y = (-1 / 15) * sin (u) * (3 * cos (v) - 3 * cos (u) ^. 2 * cos (v) - 48 * cos (u) ^. 4 * cos (v) + 48 * cos (u) ^. 6 * cos (v) - 60 * sin (u) + 5 * cos (u) * cos (v) * sin (u) - 5 * cos (u) ^. 3 * cos (v) * sin (u) - 80 * cos (u) ^. 5 * cos (v) * sin (u) + 80 * cos (u) ^. 7 * cos (v) * sin (u))
      z = (2 / 15) * (3 + 5 * cos (u) * sin (u)) * sin (v)
  in (x, y, z)

roman :: Floating t => t -> Surface t
roman t (u, v) =
  let sq n = n * n
      x = 1 / 2 * t * sin (2 * u) * sq (sin v)
      y = 1 / 2 * t * sin u * cos (2 * v)
      z = 1 / 2 * t * cos u * sin (2 * v)
  in (x, y, z)

boys_apery :: Floating t => Surface t
boys_apery (u, v) =
  let sq n = n * n
      d = 2 - sqrt 2 * sin (3 * u) * sin (2 * v)
      x = (sqrt 2 * sq (cos v) * cos (2 * u) + cos u * sin (2 * v)) / d
      y = (sqrt 2 * sq (cos v) * sin (2 * u) + sin u * sin (2 * v)) / d
      z = 2 * sq (cos v) / d
  in (x, y, z)

boys_roman :: Floating t => t -> Surface t
boys_roman t (u, v) =
  let sq n = n * n
      d = (2 - t * sqrt 2 * sin (3 * u) * sin (2 * v))
      x = (sqrt 2 * cos (2 * u) * sq (cos v) + cos u * sin (2 * v)) / d
      y = (sqrt 2 * sin (2 * u) * sq (cos v) - sin u * sin (2 * v)) / d
      z = 3 * sq (cos v) / d
  in (x, y, z)

-- | T. Kuen, c.1884
kuen :: Floating t => Surface t
kuen (u, v) =
  let sq n = n * n
      d = 1 + sq u * sq (sin v)
      x = (2 * (cos u + u * sin u) * sin v) / d
      y = (2 * (sin u - u * cos u) * sin v) / d
      z = log (tan (v / 2)) + ((2 * cos v) / d)
  in (x, y, z)

-- 0.5 <= r <= 1 ; 0 <= p <= 2 pi
verrill :: Floating t => Surface t
verrill (r, p) =
  let x = -2 * r * cos p + 2 * cos p / r - 2 * r ^. 3 * cos (3 * p) / 3
      y = 6 * r * sin p - 2 * sin p / r - 2 * r ^. 3 * sin (3 * p) / 3
      z = 4 * log r
  in (x, y, z)

catalan :: Floating t => Surface t
catalan (r, t0) =
  let x = 1 - (cos t0 * cosh r)
      y = 4 * sin (t0 / 2) * sinh (r / 2)
      z = t0 - cosh r * sin t0
  in (x, y, z)

-- | 0 <= r, 0 <= t <= 2 pi
bour :: Floating t => t -> Surface t
bour n (r, t) =
  let x = r ** (n - 1) * cos ((n - 1) * t) / (2 * (n - 1)) - r ** (n + 1) * cos ((n + 1) * t) / (2 * (n + 1))
      y = r ** (n - 1) * sin ((n - 1) * t) / (2 * (n - 1)) + r ** (n + 1) * sin ((n + 1) * t) / (2 * (n + 1))
      z = r ** n * cos (n * t) / n
  in (x, y, z)

-- | Alfred Enneper, c.1863. -2 <= u <= 2, -2 <= v <= 2
enneper :: Floating t => Surface t
enneper (u, v) =
  let x = u - ((1 / 3) * (u ^. 3)) + (u * (v ^. 2))
      y = (0 - v) - ((u ^. 2) * v) + ((1 / 3) * (v ^. 3))
      z = (u ^. 2) - (v ^. 2)
  in (x, y, z)

enneper' :: Floating t => Surface t
enneper' (u, v) =
  let x = (1 / 3) * u * (1 - ((1 / 3) * (u ^. 2)) + (v ^. 2))
      y = (1 / 3) * v * (1 - ((1 / 3) * (v ^. 2)) + (u ^. 2))
      z = (1 / 3) * ((u ^. 2) - (v ^. 2))
  in (x, y, z)

-- 0 <= u <= pi, 0 <= v <= pi
lemnescate :: Floating t => Surface t
lemnescate (u, v) =
  let sq n = n * n
      x = cos (v) * sqrt (abs (sin (2 * u))) * cos u
      y = cos (v) * sqrt (abs (sin (2 * u))) * sin u
      z = sq x - sq y + 2 * x * y * sq (tan v)
  in (x, y, z)

-- | -infinity <= u & v <= infinity
henneburg :: Floating t => Surface t
henneburg (u, v) =
  let x = 2 * sinh (u) * cos (v) - 2 * sinh (3 * u) * cos (3 * v) / 3
      y = 2 * sinh (u) * sin (v) + 2 * sinh (3 * u) * sin (3 * v) / 3
      z = 2 * cosh (2 * u) * cos (2 * v)
  in (x, y, z)

-- | Leonhard Euler, c.1740. 0 <= u <= 2pi ; -infinity <= v <= infinity
catenoid :: Floating t => t -> Surface t
catenoid c (u, v) =
  let x = c * cosh (v / c) * cos u
      y = c * cosh (v / c) * sin u
      z = v
  in (x, y, z)

-- | J. Meusnier, c.1775. -infinity <= u & v <= infinity
helicoid :: Floating t => t -> Surface t
helicoid c (u, v) =
  let x = c * v * cos u
      y = c * v * sin u
      z = u
  in (x, y, z)

richmond :: Fractional n => n -> V2 n -> V3 n
richmond c (u, v) =
  let x = c * (-3 * u - u ^. 5 + 2 * u ^. 3 * v ^. 2 + 3 * u * v ^. 4) / (6 * (u * u + v * v))
      y = c * (-3 * v - 3 * u ^. 4 * v - 2 * u ^. 2 * v ^. 3 + v ^. 5) / (6 * (u * u + v * v))
      z = c * u
  in (x, y, z)

richmond_ :: Floating t => Surface t
richmond_ (u, v) =
  let x = (1 / 3) * u ^. 3 - u * v ^. 2 + (u / (u ^. 2 + v ^. 2))
      y = (1 / 3) * v ^. 2 - u ^. 2 * v - (v / (u ^. 2 + v ^. 2))
      z = 2 * u
  in (x, y, z)

scherk :: Floating t => t -> Surface t
scherk c (u, v) =
  let x = u
      y = v
      z = log (cos (c * u) / cos (c * v)) / c
  in (x, y, z)

dini :: Floating t => Surface t
dini (u, v) =
  let x = cos u * sin v
      y = sin u * sin v
      z = cos v + log (tan (v / 2)) + 0.2 * u
  in (x, y, z)

conchoid :: Floating t => Surface t
conchoid (u, v) =
  let k1 = 1.2
      k2 = 1.2
      a = 1.5
      x = k1 ** u * (1 + cos v) * cos u
      y = k1 ** u * (1 + cos v) * sin u
      z = k1 ** u * sin v - a * k2 ** u
  in (x, y, z)

breather :: Floating t => Surface t
breather (u, v) =
  let k = sqrt (0.84)
      g = (0.4 * ((k * cosh (0.4 * u)) ^. 2 + (0.4 * sin (k * v)) ^. 2))
      x = (2 * k * cosh (0.4 * u) * (-(k * cos (v) * cos (k * v)) - sin (v) * sin (k * v))) / g
      y = (2 * k * cosh (0.4 * u) * (-(k * sin (v) * cos (k * v)) + cos (v) * sin (k * v))) / g
      z = -u + (2 * 0.84 * cosh (0.4 * u) * sinh (0.4 * u)) / g
  in (x, y, z)

whitney :: Floating t => Surface t
whitney (u, v) =
  let x = u * v
      y = u
      z = v ^. 2
  in (x, y, z)

cross_cap :: Floating t => Surface t
cross_cap (u, v) =
  let x = (1 + cos (v)) * cos (u)
      y = (1 + cos (v)) * sin (u)
      z = -tanh ((2 / 3) * (u - pi)) * sin (v)
  in (x, y, z)

trefoil :: Floating t => Surface t
trefoil (u, v) =
  let k = 12
      x = (k * (1 + 0.25 * sin (3 * v)) + cos (u)) * cos (2 * v)
      y = (k * (1 + 0.25 * sin (3 * v)) + cos (u)) * sin (2 * v)
      z = sin (u) + (k / 2) * cos (3 * v)
  in (x, y, z)

pseudosphere_A :: Floating t => Surface t
pseudosphere_A (u, v) = (sech u * cos v, sech u * sin v, u - tanh u)

{- | Plueckers Conoid

<https://en.wikipedia.org/wiki/Pl%C3%BCcker%27s_conoid>
<https://mathworld.wolfram.com/PlueckersConoid.html>
-}
plueckersConoid :: Floating t => t -> Surface t
plueckersConoid n (u, v) =
  let x = v * cos(u)
      y = v * sin(u)
      z = sin(n * u)
  in (x, y, z)
