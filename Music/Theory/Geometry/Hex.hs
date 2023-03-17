-- | <https://www.redblobgames.com/grids/hexagons/implementation.html>
module Music.Theory.Geometry.Hex where

import Music.Theory.Geometry.Matrix {- hmt-base -}
import Music.Theory.Geometry.Vector {- hmt-base -}
import qualified Music.Theory.Geometry.Functions as Geometry {- hmt-base -}

{- | Hex (q,r) co-ordinate to (q,r,s) co-ordinate.

> map hex_qrs [(0, 0), (7,-2)] == [(0,0,0),(7,-2,-5)]
-}
hex_qrs :: Num n => V2 n -> V3 n
hex_qrs (q,r) = (q,r,-q - r)

-- | Hex (q,r,s) co-ordinate to (q,r) co-ordinate.
hex_qr :: Num n => V3 n -> V2 n
hex_qr (q,r,_) = (q,r)

{- | Given unit size and initial angle return axis vectors for hex_qr co-ordinates.
(sqrt 3) is twice the inradius of a hexagon with unit size length and unit circumradius.

> hex_axis_vectors (sqrt 3) 0
-}
hex_axis_vectors :: Floating n => n -> n -> V2 (V2 n)
hex_axis_vectors u a = (v2_rotate a (u,0),v2_rotate (a + (pi / 3)) (u,0))

-- | Given unit size and initial angle calculate translation vector for (q,r) index to pixel.
hex_m22 :: Floating t => t -> t -> M22 t
hex_m22 u = m22_transpose . hex_axis_vectors u

-- | 'm22_inverse' of 'hex_m22'
hex_m22_inv :: Floating t => t -> t -> M22 t
hex_m22_inv u = m22_inverse . hex_m22 u

-- | Given origin, scaling factor and and matrix translate a (q,r) hex co-ordinate to pixel co-ordinate.
hex_to_pixel :: Num n => V2 n -> n -> M22 n -> V2 Int -> V2 n
hex_to_pixel o u m (q,r) =
  let c = m22_apply m (fromIntegral q,fromIntegral r)
  in v2_add o (v2_scale u c)

-- | (x,y) pixel co-ordinate to fractional (q,r) hex co-ordinate.
pixel_to_fhex :: Fractional n => V2 n -> n -> M22 n -> V2 n -> V2 n
pixel_to_fhex o u m c =
  let p = v2_scale (1 / u) (v2_sub c o)
  in m22_apply m p

-- | Fraction (q,r,s) co-ordinate rounded to Int (q,r,s) co-ordinate.
fhex_round :: RealFrac n => V3 n -> V3 Int
fhex_round (qf,rf,sf) =
  let q = round qf
      r = round rf
      s = round sf
      q_diff = abs (fromIntegral q - qf)
      r_diff = abs (fromIntegral r - rf)
      s_diff = abs (fromIntegral s - sf)
  in if (q_diff > r_diff && q_diff > s_diff)
     then (-r - s,r,s)
     else if (r_diff > s_diff)
          then (q,-q - s,s)
          else (q,r,-q - r)

-- * Standard grids

pointy_mtx :: (M22 Double, M22 Double)
pointy_mtx =
  let m = hex_m22 (sqrt 3) 0
  in (m,m22_inverse m)

flat_mtx :: (M22 Double, M22 Double)
flat_mtx =
  let m = hex_m22 (sqrt 3) (pi / 6)
  in (m,m22_inverse m)

-- * Mz-U648

{- | Microzone U648 angle

<https://www.starrlabs.com/product/microzone-u648/>

The orientation is arranged so that (q,r) and (q+7,r-2) are aligned (on the x-axis).
This is the usual Bosanquet/Wilson arrangement.

> round (Geometry.radians_to_degrees mz_u648_angle) == 16
-}
mz_u648_angle :: Double
mz_u648_angle =
  let b = 5
      a = 2
      gamma = 2 * pi / 3
      c = Geometry.triangle_side_sas a gamma b
  in Geometry.triangle_angle_alpha a b c

{- | Hex M22 form of mz_u648_angle.

px = map (hex_to_pixel (0,0) 1 mz_u648_mtx) [(p,q) | p <- [0..4], q <- [0..4]]
qr = map (pixel_to_fhex (0,0) 1 (m22_inverse mz_u648_mtx)) px
map (hex_qr . fhex_round . hex_qrs) qr
-}
mz_u648_mtx :: Double -> M22 Double
mz_u648_mtx = flip hex_m22 mz_u648_angle

-- * Face

{- | Ccw coordinate sequence for hexagonal face.
     a = angle, u = unit (diameter of hex), c = center coordinate

> map (v2_map round) (hex_face 0 100 (50,50)) == [(93,75),(50,100),(7,75),(7,25),(50,0),(93,25)]
-}
hex_face :: (RealFloat n,Enum n) => n -> n -> V2 n -> [V2 n]
hex_face a u c =
  let p = map (\x -> Geometry.polar_to_rectangular (u * 0.5,a + x)) (take 6 [pi/6, pi/2 ..])
  in map (v2_add c) p
