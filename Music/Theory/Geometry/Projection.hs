-- | Projections
module Music.Theory.Geometry.Projection where

import Music.Theory.Geometry.Functions {- hmt-base -}
import Music.Theory.Geometry.Matrix {- hmt-base -}
import Music.Theory.Geometry.Vector {- hmt-base -}

-- * Prj

-- | Project V3 to V2.
type Prj n = V3 n -> V2 n

{- | Select X & Y, X & Z, Y & Z, also reverse.

>>> let p = (1,2,3) in map (\f -> f p) [prj_xy,prj_xz,prj_yz]
[(1,2),(1,3),(2,3)]
-}
prj_xy, prj_xz, prj_yz :: Prj n
prj_xy (x, y, _) = (x, y)
prj_xz (x, _, z) = (x, z)
prj_yz (_, y, z) = (y, z)

-- | Select Y & X, Z & X, Z & Y.
prj_yx, prj_zx, prj_zy :: Prj n
prj_yx (x, y, _) = (y, x)
prj_zx (x, _, z) = (z, x)
prj_zy (_, y, z) = (z, y)

{- | Stereographic

>>> prj_stereographic (0,0,-1)
(0.0,0.0)
-}
prj_stereographic :: Fractional n => V3 n -> V2 n
prj_stereographic (x, y, z) = (x / (1 - z), y / (1 - z))

{- | Stereographic (Inv)

>>> prj_stereographic_inv (0,0)
(0.0,0.0,-1.0)
-}
prj_stereographic_inv :: Fractional n => V2 n -> V3 n
prj_stereographic_inv (x, y) = let i = 1 + x * x + y * y in ((2 * x) / i, (2 * y) / i, (i - 2) / i)

{- | M33 variants.

>>> let p = (1,2,3) in map (\m -> m33_apply m p) [prj_xy_m33,prj_xz_m33,prj_yz_m33]
[(1,2,0),(1,3,0),(2,3,0)]
-}
prj_xy_m33, prj_xz_m33, prj_yz_m33 :: Num n => M33 n
prj_xy_m33 = ((1, 0, 0), (0, 1, 0), (0, 0, 0))
prj_xz_m33 = ((1, 0, 0), (0, 0, 1), (0, 0, 0))
prj_yz_m33 = ((0, 1, 0), (0, 0, 1), (0, 0, 0))

-- * Prj3

-- | (α,β,×), α and β are in radians. α=z β=x ×=(x,y,z)
type Prj3 n = (n, n, (n, n, n))

-- | Prj3 to row order 3×3 matrix.
prj3_to_m33 :: Floating r => Prj3 r -> M33 r
prj3_to_m33 (alpha, beta, (x, y, z)) =
  ( (x * negate (cos beta), 0, z * cos alpha)
  , (x * sin beta, y, z * sin alpha)
  , (0, 0, 0)
  )

-- | 'prj_xy' of 'm33_apply' of 'prj3_to_m33'
prj3_apply :: Floating n => Prj3 n -> V3 n -> V2 n
prj3_apply p = prj_xy . m33_apply (prj3_to_m33 p)

-- | (α=β=0,×=1)
identity_prj3 :: Floating n => Prj3 n
identity_prj3 = (0, 0, (1, 1, 1))

{- | (α=β=π÷6,×=1) ; ISO 5456-3 ; sin(pi/6)=1/2

>>> f = prj3_apply isometric_prj3
>>> map (v2_distance (0,0) . f) [(1,0,0),(1,1,0),(1,1,1)] == [1,sqrt 3,2]
True
-}
isometric_prj3 :: Floating n => Prj3 n
isometric_prj3 = (pi / 6, pi / 6, (1, 1, 1))

-- | (α=β=atan(1/2),×=1) ; α=β~=27°
isometric_tile_prj3 :: Floating n => Prj3 n
isometric_tile_prj3 = let x = atan (1 / 2) in (x, x, (1, 1, 1))

{- | (α=π÷4,β=0,×=1÷2) ; a=atan(2) | a=sin(pi/4)==1/sqrt(2)

>>> let a = atan 2 -- pi / 4
>>> let m = ((-1,0,cos(a)/2),(0,1,sin(a)/2),(0,0,0))
>>> prj3_to_m33 (cabinet_oblique_prj3 a) == m
True
-}
cabinet_oblique_prj3 :: Floating n => n -> Prj3 n
cabinet_oblique_prj3 a = (a, 0, (1, 1, 1 / 2))

-- | (α=π÷4,β=0,×=1)
cavalier_oblique_prj3 :: Floating n => Prj3 n
cavalier_oblique_prj3 = (pi / 4, 0, (1, 1, 1))

-- | (α=β=π÷4,×=1), also /military/ projection.
planometric_prj3 :: Floating n => Prj3 n
planometric_prj3 = (pi / 4, pi / 4, (1, 1, 1))

-- | (α=42°,β=7°,×=1÷2) ; NEN 2536
dimetric_42_7_prj3 :: Floating n => Prj3 n
dimetric_42_7_prj3 = (degrees_to_radians 42, degrees_to_radians 7, (1, 1, 1 / 2))

-- | (α=β=36°50',×=1,3÷4,1)
dimetric_37_37_prj3 :: Floating n => Prj3 n
dimetric_37_37_prj3 = let f = degrees_minutes_to_radians in (f (36, 50), f (36, 50), (1, 3 / 4, 1))

-- | (α=36°50',β=16°20',×=1,1,3÷4)
dimetric_37_16_prj3 :: Floating n => Prj3 n
dimetric_37_16_prj3 = let f = degrees_minutes_to_radians in (f (36, 50), f (16, 20), (1, 1, 3 / 4))

-- | (α=54°16',β=23°16',×=1,2÷3,7÷8)
trimetric_54_23_prj3 :: Floating n => Prj3 n
trimetric_54_23_prj3 = let f = degrees_minutes_to_radians in (f (54, 16), f (23, 16), (1, 2 / 3, 7 / 8))

-- | (α=24°46',β=17°,×=7÷8,1,3÷4)
trimetric_25_17_prj3 :: Floating n => Prj3 n
trimetric_25_17_prj3 = let f = degrees_minutes_to_radians in (f (24, 46), f (17, 0), (7 / 8, 1, 3 / 4))

-- | (α=23°16',β=12°28',×=7÷8,1,2÷3)
trimetric_23_12_prj3 :: Floating n => Prj3 n
trimetric_23_12_prj3 = let f = degrees_minutes_to_radians in (f (23, 16), f (12, 28), (7 / 8, 1, 2 / 3))

-- | (α=48°14',β=24°46',×=1,3÷4,7÷8)
trimetric_48_25_prj3 :: Floating n => Prj3 n
trimetric_48_25_prj3 = let f = degrees_minutes_to_radians in (f (48, 14), f (24, 46), (1, 3 / 4, 7 / 8))

{- | (α=π÷6,β=0,×=1÷2)

>>> prj3_to_m33 chinese_prj3
((-1.0,0.0,0.43301270189221935),(0.0,1.0,0.24999999999999997),(0.0,0.0,0.0))
-}
chinese_prj3 :: Floating n => Prj3 n
chinese_prj3 = (pi / 6, 0, (1, 1, 1 / 2))

-- * V4-V3

-- | Projections from V4 (x,y,z,w) to V3.
prj_xyz, prj_xyw, prj_xzw, prj_yzw :: V4 t -> V3 t
prj_xyz (x, y, z, _w) = (x, y, z)
prj_xyw (x, y, _z, w) = (x, y, w)
prj_xzw (x, _y, z, w) = (x, z, w)
prj_yzw (_x, y, z, w) = (y, z, w)

-- * Perspective

-- | c=camera-location t=camera-orientation
p_camera :: (Floating t) => V3 t -> V3 t -> V3 t -> V3 t
p_camera (cx, cy, cz) (tx, ty, tz) (ax, ay, az) =
  let (x, y, z) = (ax - cx, ay - cy, az - cz)
      (c_x, c_y, c_z) = (cos tx, cos ty, cos tz)
      (s_x, s_y, s_z) = (sin tx, sin ty, sin tz)
  in ( c_y * (s_z * y + c_z * x) - s_y * z
     , s_x * (c_y * z + s_y * (s_z * y + c_z * x)) + c_x * (c_z * y - s_z * x)
     , c_x * (c_y * z + s_y * (s_z * y + c_z * x)) - s_x * (c_z * y - s_z * x)
     )

-- | e=image-plane-location
p_plane :: Fractional t => V3 t -> V3 t -> V2 t
p_plane (ex, ey, ez) (dx, dy, dz) = ((ez / dz) * dx + ex, (ez / dz) * dy + ey)

-- | c=camera-location t=camera-orientation e=image-plane-location
perspective :: Floating t => V3 t -> V3 t -> V3 t -> V3 t -> V2 t
perspective c t e a = p_plane e (p_camera c t a)

{-
Ingrid Carlbom, Joseph Paciorek (1978).
"Planar Geometric Projections and Viewing Transformations".
ACM Computing Surveys. 10 (4): 465-502.
doi:10.1145/356744.356750.
[PDF](http://www.cs.uns.edu.ar/cg/clasespdf/p465carlbom.pdf)
-}
