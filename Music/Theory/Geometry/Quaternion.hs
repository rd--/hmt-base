-- | Quaternions
module Music.Theory.Geometry.Quaternion where

import Music.Theory.Geometry.Matrix {- hmt-base -}
import Music.Theory.Geometry.Vector {- hmt-base -}

-- | Quaternion (0,1,2,3) (w,x,y,z) (a,b,c,d)
type Q t = V4 t

-- | Q unit
q_unit :: Num t => Q t
q_unit = (1, 0, 0, 0)

-- | Q zero
q_zero :: Num t => Q t
q_zero = (0, 0, 0, 0)

-- | Q multiplication (non-commutative)
q_mul :: Num t => Q t -> Q t -> Q t
q_mul (a1, b1, c1, d1) (a2, b2, c2, d2) =
  ( a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2
  , a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2
  , a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2
  , a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2
  )

-- | Q negation, ie. 'v4_negate'
q_neg :: Num t => Q t -> Q t
q_neg = v4_negate

-- | Q normalisation (ie. 'v4_normalize')
q_normalize :: Floating t => Q t -> Q t
q_normalize = v4_normalize

q_inverse :: Fractional t => Q t -> Q t
q_inverse (a, b, c, d) =
  let v = (a, -b, -c, -d)
  in v4_scale (1 / v4_dot v v) v

{- | Quaternion to rotation matrix (to be multiplied with on the /left/).

>>> let q = map euler_angles_to_quaternion [(pi/2,0,0),(0,pi/2,0),(0,0,-pi/2)]
>>> let m = map q_to_m33_left q
>>> map (v3_round . flip m33_apply (0,0,1)) m
[(0,0,1),(1,0,0),(0,1,0)]
-}
q_to_m33_left :: Num t => Q t -> M33 t
q_to_m33_left (a, b, c, d) =
  let aa = a * a
      bb = b * b
      cc = c * c
      dd = d * d
      ab = a * b
      ac = a * c
      ad = a * d
      bc = b * c
      bd = b * d
      cd = c * d
  in ( (aa + bb - cc - dd, 2 * (bc - ad), 2 * (bd + ac))
     , (2 * (bc + ad), aa - bb + cc - dd, 2 * (cd - ab))
     , (2 * (bd - ac), 2 * (cd + ab), aa - bb - cc + dd)
     )

-- | The /left/ action of a unit quaternion on a 3D vector.
q_act :: Num t => Q t -> V3 t -> V3 t
q_act q = m33_apply (q_to_m33_left q)

{- | The quaternion to encode rotation around an axis.
  Axis will be normalized.  Quaternions act on the /left/.
-}
q_rot :: Floating t => V3 t -> t -> Q t
q_rot axis angle = q_rot_normal (v3_normalize axis) angle

-- | The quaternion to encode rotation around an axis, which must be normal.
q_rot_normal :: Floating t => V3 t -> t -> Q t
q_rot_normal (x, y, z) angle =
  let a = 0.5 * angle
      c = cos a
      s = sin a
  in (c, x * s, y * s, z * s)

{- | (z=yaw,y=pitch,x=roll)

>>> let e2q = euler_angles_to_quaternion
>>> let q2e = quaternion_to_euler_angles
>>> q2e (e2q (pi/3,pi/4,pi/5)) `v3_approx_eq` (pi/3,pi/4,pi/5)
True

>>> let q = map e2q [(pi/2,0,0),(0,pi/2,0),(0,0,-pi/2)]
>>> map (v3_round . flip q_act (0,0,1)) q
[(0,0,1),(1,0,0),(0,1,0)]

>>> q2e (q_mul (e2q (pi/2,0,0)) (e2q (0,pi/2,0))) == (pi/2,pi/2,0)
True
-}
euler_angles_to_quaternion :: Floating t => V3 t -> V4 t
euler_angles_to_quaternion (z, y, x) =
  let cy = cos (z * 0.5)
      sy = sin (z * 0.5)
      cp = cos (y * 0.5)
      sp = sin (y * 0.5)
      cr = cos (x * 0.5)
      sr = sin (x * 0.5)
  in ( cr * cp * cy + sr * sp * sy
     , sr * cp * cy - cr * sp * sy
     , cr * sp * cy + sr * cp * sy
     , cr * cp * sy - sr * sp * cy
     )

-- | Inverse of 'euler_angles_to_quaternion'
quaternion_to_euler_angles :: RealFloat t => V4 t -> V3 t
quaternion_to_euler_angles (w, x, y, z) =
  let sinr_cosp = 2 * (w * x + y * z)
      cosr_cosp = 1 - 2 * (x * x + y * y)
      sinp = 2 * (w * y - z * x)
      siny_cosp = 2 * (w * z + x * y)
      cosy_cosp = 1 - 2 * (y * y + z * z)
  in ( atan2 siny_cosp cosy_cosp -- z=yaw
     , if abs sinp >= 1 then pi / 2 * signum sinp else asin sinp -- y=pitch
     , atan2 sinr_cosp cosr_cosp -- x=roll
     )
