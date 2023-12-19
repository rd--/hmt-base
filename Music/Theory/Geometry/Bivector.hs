-- | 2/3-Bivectors and 3-Rotors.
module Music.Theory.Geometry.Bivector where

import Music.Theory.Geometry.Matrix {- hmt-base -}
import Music.Theory.Geometry.Vector {- hmt-base -}

import Music.Theory.Math (sqr {- hmt-base -})

-- * 2-Bivector (Bv2)

-- | 2-Bivector (b01)
type Bv2 n = n

-- | Outer product, a ∧ b.
bv2_outer_product :: Num n => V2 n -> V2 n -> Bv2 n
bv2_outer_product (x1, y1) (x2, y2) =
  (x1 * y2) - (y1 * x2)

-- * 3-Bivector (Bv3)

-- | 3-Bivector (b01, b02, b12)
type Bv3 n = (n, n, n)

-- | Outer product, a ∧ b (also called exterior, or wedge product)
bv3_outer_product :: Num n => V3 n -> V3 n -> Bv3 n
bv3_outer_product (x1, y1, z1) (x2, y2, z2) =
  ( (x1 * y2) - (y1 * x2) -- xy
  , (x1 * z2) - (z1 * x2) -- xz
  , (y1 * z2) - (z1 * y2) -- yz
  )

-- * 3-Rotor (Rt3) (https://marctenbosch.com/quaternions/)

-- | (a, b01, b02, b12) = (a, bxy, bxz, byz)
type Rt3 n = (n, n, n, n)

rt3_bv3 :: n -> Bv3 n -> Rt3 n
rt3_bv3 a (b01, b02, b12) = (a, b01, b02, b12)

-- | The rotor that rotates one vector to another. (Usual trick to get the half angle)
rt3_rotation :: Floating n => V3 n -> V3 n -> Rt3 n
rt3_rotation from to = rt3_normalize (rt3_bv3 (1 + v3_dot to from) (bv3_outer_product to from))

{- | Rotor from plane & angle, plane must be normalized.

The left side of the products have b a, not a b.
-}
rt3_plane :: Floating n => Bv3 n -> n -> Rt3 n
rt3_plane (b01, b02, b12) angleRadian =
  let m = negate (sin (angleRadian / 2))
      a = cos (angleRadian / 2)
  in (a, m * b01, m * b02, m * b12)

-- | Rotor product (non-optimised).
rt3_product :: Num n => Rt3 n -> Rt3 n -> Rt3 n
rt3_product (p_a, p_b01, p_b02, p_b12) (q_a, q_b01, q_b02, q_b12) =
  ( p_a * q_a - p_b01 * q_b01 - p_b02 * q_b02 - p_b12 * q_b12
  , p_b01 * q_a + p_a * q_b01 + p_b12 * q_b02 - p_b02 * q_b12
  , p_b02 * q_a + p_a * q_b02 - p_b12 * q_b01 + p_b01 * q_b12
  , p_b12 * q_a + p_a * q_b12 + p_b02 * q_b01 - p_b01 * q_b02
  )

-- | R x R* (non-optimized). Trivector part of the result is always zero.
rt3_rotate_v3 :: Num n => Rt3 n -> V3 n -> V3 n
rt3_rotate_v3 (a, b01, b02, b12) (x, y, z) =
  let (q_x, q_y, q_z) =
        ( a * x + y * b01 + z * b02
        , a * y - x * b01 + z * b12
        , a * z - x * b02 - y * b12
        )
      q012 = x * b12 - y * b02 + z * b01
  in ( a * q_x + q_y * b01 + q_z * b02 + q012 * b12
     , a * q_y - q_x * b01 - q012 * b02 + q_z * b12
     , a * q_z + q012 * b01 - q_x * b02 - q_y * b12
     )

-- | Rotate a rotor by another.
rt3_rotate :: Num n => Rt3 n -> Rt3 n -> Rt3 n
rt3_rotate p q = rt3_product (rt3_product p q) (rt3_reverse p)

-- | Equivalent to conjugate
rt3_reverse :: Num n => Rt3 n -> Rt3 n
rt3_reverse (a, b01, b02, b12) = (a, -b01, -b02, -b12)

-- | Length Squared
rt3_length_sq :: Num n => Rt3 n -> n
rt3_length_sq (a, b01, b02, b12) =
  sqr a + sqr b01 + sqr b02 + sqr b12

-- | Length
rt3_length :: Floating n => Rt3 n -> n
rt3_length = sqrt . rt3_length_sq

-- | Normalize
rt3_normalize :: Floating n => Rt3 n -> Rt3 n
rt3_normalize r =
  let (a, b01, b02, b12) = r
      l = rt3_length r
  in (a / l, b01 / l, b02 / l, b12 / l)

-- | Convert to matrix (non-optimized).
rt3_to_m33 :: Num n => Rt3 n -> M33 n
rt3_to_m33 r =
  ( rt3_rotate_v3 r (1, 0, 0)
  , rt3_rotate_v3 r (0, 1, 0)
  , rt3_rotate_v3 r (0, 0, 1)
  )

-- | Geometric product (for reference), produces twice the angle, negative direction.
rt3_geometric_product :: Num n => V3 n -> V3 n -> Rt3 n
rt3_geometric_product a b = rt3_bv3 (v3_dot a b) (bv3_outer_product a b)
