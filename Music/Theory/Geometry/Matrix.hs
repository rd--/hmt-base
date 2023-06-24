-- | 2x2, 3x3 and 4x3 matrices.
module Music.Theory.Geometry.Matrix where

import Music.Theory.Geometry.Vector {- hmt-base -}

-- * M22

-- | 2×2 matrix.
type M22 n = V2 (V2 n)

-- | Transpose M22.
m22_transpose :: M22 t -> M22 t
m22_transpose ((a,b),(c,d)) = ((a,c),(b,d))

-- | Apply /f/ at each cell of /m/.
m22_map :: (t -> u) -> M22 t -> M22 u
m22_map f ((a,b),(c,d)) = ((f a,f b),(f c,f d))

m22_zip :: (t -> u -> v) -> M22 t -> M22 u -> M22 v
m22_zip fn ((a,b),(c,d)) ((i,j),(k,l)) = ((fn a i,fn b j),(fn c k,fn d l))

{- | 2×2 Matrix determinant

>>> m22_determinant ((1,4),(-1,9))
13
-}
m22_determinant :: Num t => M22 t -> t
m22_determinant ((a,b),(c,d)) = a * d - b * c

{- | Inverse matrix

>>> m22_inverse ((-1,3/2),(1,-1))
((2.0,3.0),(2.0,2.0))

> sage: matrix(2,2,[-1,3/2,1,-1]).inverse() == matrix(2,2,[2,3,2,2])
-}
m22_inverse :: Fractional t => M22 t -> M22 t
m22_inverse m =
  let ((a,b),(c,d)) = m
      mul = 1 / m22_determinant m
  in m22_map (* mul) ((d,-b),(-c,a))

{- | 2×2 Identity Matrix

>>> m22_mul ((0,-1/3),(1/3,0)) m22_id == ((0,-1/3),(1/3,0))
True
-}
m22_id :: Num t => M22 t
m22_id = ((1,0),(0,1))

{- | 2×2 matrix multiplication.

>>> m22_mul ((0,1),(0,0)) ((0,0),(1,0))
((1,0),(0,0))

>>> m22_mul ((0,0),(1,0)) ((0,1),(0,0))
((0,0),(0,1))
-}
m22_mul :: Num t => M22 t -> M22 t -> M22 t
m22_mul (a,b) (c,d) =
  let (i,j) = m22_transpose (c,d)
  in m22_zip v2_dot ((a,a),(b,b)) ((i,j),(i,j)) -- 00 01 10 11

-- | Apply M22 to V2.
m22_apply :: Num n => M22 n -> V2 n -> V2 n
m22_apply ((a,b),(c,d)) (x,y) = (a * x + b * y,c * x + d * y)

{- | Rotation M22 (radians).

>>> m22_apply (m22_rotation (pi/2)) (0,1) `v2_approx_eq` (1,0)
True
-}
m22_rotation :: Floating t => t -> M22 t
m22_rotation n = ((cos n,sin n),(negate (sin n),cos n))

-- * M33

-- | 3×3 matrix (R=3,C=3) ROW-ORDER ((11 12 13) (21 22 23) (31 32 33)
type M33 n = V3 (V3 n)

m33_determinant :: Num t => M33 t -> t
m33_determinant ((a,b,c),(d,e,f),(g,h,i)) = a*e*i + b*f*g + c*d*h - c*e*g - b*d*i - a*f*h

{- | Inverse

>>> m33_inverse ((1,1,1),(1,0,0),(0,1,0))
((0,1,0),(0,0,1),(1,-1,-1))
-}
m33_inverse :: Num t => M33 t -> M33 t
m33_inverse m =
  let ((a,b,c),(d,e,f),(g,h,i)) = m
      mul = m33_determinant m
  in m33_map
     (* mul)
     ((e * i - f * h,-(b * i - c * h),b * f - c * e)
     ,(-(d * i - f * g),a * i - c * g,-(a * f - c * d))
     ,(d * h - e * g,-(a * h - b * g),a * e - b * d))

{- | Inverse

>>> m33_inverse_ ((1,1,1),(1,0,0),(0,1,0))
((0,1,0),(0,0,1),(1,-1,-1))
-}
m33_inverse_ :: Num t => M33 t -> M33 t
m33_inverse_ m =
  let ((a11,a12,a13),(a21,a22,a23),(a31,a32,a33)) = m
      mul = m33_determinant m
      det = m22_determinant
      b11 = det ((a22,a23),(a32,a33))
      b12 = det ((a13,a12),(a33,a32))
      b13 = det ((a12,a13),(a22,a23))
      b21 = det ((a23,a21),(a33,a31))
      b22 = det ((a11,a13),(a31,a33))
      b23 = det ((a13,a11),(a23,a21))
      b31 = det ((a21,a22),(a31,a32))
      b32 = det ((a12,a11),(a32,a31))
      b33 = det ((a11,a12),(a21,a22))
  in m33_map (* mul) ((b11,b12,b13),(b21,b22,b23),(b31,b32,b33))

-- | Transpose M33.
m33_transpose :: M33 t -> M33 t
m33_transpose ((a,b,c),(d,e,f),(g,h,i)) = ((a,d,g),(b,e,h),(c,f,i))

m33_id :: Num t => M33 t
m33_id = ((1,0,0),(0,1,0),(0,0,1))

m33_map :: (t -> u) -> M33 t -> M33 u
m33_map fn ((a,b,c),(d,e,f),(g,h,i)) = ((fn a,fn b,fn c),(fn d,fn e,fn f),(fn g,fn h,fn i))

m33_zip :: (t -> u -> v) -> M33 t -> M33 u -> M33 v
m33_zip fn ((a,b,c),(d,e,f),(g,h,i)) ((j,k,l),(m,n,o),(p,q,r)) =
  ((fn a j,fn b k,fn c l),(fn d m,fn e n,fn f o),(fn g p,fn h q,fn i r))

m33_mul :: Num t => M33 t -> M33 t -> M33 t
m33_mul (a,b,c) (d,e,f) =
  let (i,j,k) = m33_transpose (d,e,f)
  in m33_zip v3_dot ((a,a,a),(b,b,b),(c,c,c)) ((i,j,k),(i,j,k),(i,j,k)) -- 00 01 02 10 11 12 20 21 22

-- | Apply M33 to V3.
m33_apply :: Num n => M33 n -> V3 n -> V3 n
m33_apply ((a,b,c),(d,e,f),(g,h,i)) (x,y,z) =
  (a * x + b * y + c * z
  ,d * x + e * y + f * z
  ,g * x + h * y + i * z)

{- | Rotation X M33 (radians).

>>> m33_apply (m33_rotation_x (pi/2)) (0,0,1) `v3_approx_eq` (0,-1,0)
True
-}
m33_rotation_x :: Floating t => t -> M33 t
m33_rotation_x n = ((1,0,0),(0,cos n,- (sin n)),(0,sin n,cos n))

{- | Rotation Y M33 (radians).

>>> m33_apply (m33_rotation_y (pi/2)) (0,0,1) `v3_approx_eq` (1,0,0)
True
-}
m33_rotation_y :: Floating t => t -> M33 t
m33_rotation_y n = ((cos n,0,sin n),(0,1,0),(- (sin n),0,cos n))

{- | Rotation Z M33 (radians).

>>> m33_apply (m33_rotation_z (pi/2)) (1,0,0) `v3_approx_eq` (0,1,0)
True
-}
m33_rotation_z :: Floating t => t -> M33 t
m33_rotation_z n = ((cos n,- (sin n),0),(sin n,cos n,0),(0,0,1))

{- | Rotation by angle θ around the axis of the unit vector (x,y,z).
x*x + y*y + z*z = 1

>>> m33_apply (m33_rotation_axis_angle (1,0,0) (pi/2)) (0,1,0) `v3_approx_eq` (0,0,-1)
True

>>> m33_apply (m33_rotation_axis_angle (0,1,0) (pi/2)) (1,0,0) `v3_approx_eq` (0,0,1)
True

>>> m33_apply (m33_rotation_axis_angle (0,0,1) (pi/2)) (1,0,0) `v3_approx_eq` (0,-1,0)
True
-}
m33_rotation_axis_angle :: Floating t => V3 t -> t -> M33 t
m33_rotation_axis_angle (x,y,z) theta =
  let c = cos theta
      s = sin theta
      t = 1 - c
  in ((t*x*x + c,t*x*y + z*s,t*x*z - y*s)
     ,(t*x*y - z*s,t*y*y + c,t*y*z + x*s)
     ,(t*x*z + y*s,t*y*z - x*s,t*z*z + c))

-- * M44

-- | 4×4 matrix (R=4,C=4) ROW-ORDER ((11 12 13 14) (21 22 23 24) (31 32 33 34) (41 42 43 44))
type M44 n = V4 (V4 n)

-- | Transpose M44.
m44_transpose :: M44 t -> M44 t
m44_transpose ((a,b,c,d),(e,f,g,h),(i,j,k,l),(m,n,o,p)) = ((a,e,i,m),(b,f,j,n),(c,g,k,o),(d,h,l,p))

{- | M44 identity matrix

> sage: matrix.identity(4) == matrix([[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]])
-}
m44_id :: Num t => M44 t
m44_id = ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))

m44_map :: (t -> u) -> M44 t -> M44 u
m44_map fn ((a,b,c,d),(e,f,g,h),(i,j,k,l),(m,n,o,p)) =
  ((fn a,fn b,fn c,fn d)
  ,(fn e,fn f,fn g,fn h)
  ,(fn i,fn j,fn k,fn l)
  ,(fn m,fn n,fn o,fn p))

m44_zip :: (t -> u -> v) -> M44 t -> M44 u -> M44 v
m44_zip fn m1 m2 =
  let ((a1,a2,a3,a4),(b1,b2,b3,b4),(c1,c2,c3,c4),(d1,d2,d3,d4)) = m1
      ((e1,e2,e3,e4),(f1,f2,f3,f4),(g1,g2,g3,g4),(h1,h2,h3,h4)) = m2
  in ((fn a1 e1,fn a2 e2,fn a3 e3,fn a4 e4)
     ,(fn b1 f1,fn b2 f2,fn b3 f3,fn b4 f4)
     ,(fn c1 g1,fn c2 g2,fn c3 g3,fn c4 g4)
     ,(fn d1 h1,fn d2 h2,fn d3 h3,fn d4 h4))

{- | M44 matrix multiplication

>>> let m1 = ((1,2,3,4),(5,6,7,8),(2,3,4,5),(6,7,8,9))
>>> let m2 = ((6,7,8,9),(5,6,7,8),(2,3,4,5),(1,2,3,4))
>>> m44_mul m1 m2
((26,36,46,56),(82,108,134,160),(40,54,68,82),(96,126,156,186))

sage:

m1 = matrix([[1,2,3,4],[5,6,7,8],[2,3,4,5],[6,7,8,9]])
m2 = matrix([[6,7,8,9],[5,6,7,8],[2,3,4,5],[1,2,3,4]])
m3 = matrix([[26,36,46,56],[82,108,134,160],[40,54,68,82],[96,126,156,186]])
m1 * m2 == m3
-}
m44_mul :: Num t => M44 t -> M44 t -> M44 t
m44_mul (a,b,c,d) (e,f,g,h) =
  let (i,j,k,l) = m44_transpose (e,f,g,h)
  in m44_zip
     v4_dot
     ((a,a,a,a),(b,b,b,b),(c,c,c,c),(d,d,d,d))
     ((i,j,k,l),(i,j,k,l),(i,j,k,l),(i,j,k,l))

-- | Apply M44 to V4.
m44_apply :: Num n => M44 n -> V4 n -> V4 n
m44_apply ((a,b,c,d),(e,f,g,h),(i,j,k,l),(m,n,o,p)) (x,y,z,w) =
  (a * x + b * y + c * z + d * w
  ,e * x + f * y + g * z + h * w
  ,i * x + j * y + k * z + l * w
  ,m * x + n * y + o * z + p * w)

m44_to_list :: M44 t -> [t]
m44_to_list ((a,b,c,d),(e,f,g,h),(i,j,k,l),(m,n,o,p)) = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p]

m44_companion :: Num t => V4 t -> M44 t
m44_companion (a0,a1,a2,a3) = ((0,0,0,-a0),(1,0,0,-a1),(0,1,0,-a2),(0,0,1,-a3))
