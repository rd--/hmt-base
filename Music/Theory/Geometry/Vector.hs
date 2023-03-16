-- | 2, 3 and 4 element Vectors.
module Music.Theory.Geometry.Vector where

import Data.Bifunctor {- base -}
import Data.List {- base -}

import Music.Theory.Math {- hmt-base -}

-- * 2-Vector (V2)

-- | 2-element vector.
type V2 n = (n,n)

-- | Field accessors.
v2_x,v2_y :: V2 n -> n
v2_x = fst
v2_y = snd

-- | (~=) at x & y.
v2_approx_eq :: (Floating n,Ord n) => V2 n -> V2 n -> Bool
v2_approx_eq (x1,y1) (x2,y2) = x1 ~= x2 && y1 ~= y2

-- | (f x,f y)
v2_map :: (t -> u) -> V2 t -> V2 u
v2_map f (a,b) = (f a,f b)

-- | (f x1 x2,f y1 y2)
v2_zip :: (a -> b -> c) -> V2 a -> V2 b -> V2 c
v2_zip f (i,j) (p,q) = (f i p,f j q)

-- | Dot product (x1·x2 + y1·y2)
v2_dot :: Num t => V2 t -> V2 t -> t
v2_dot (x1,y1) (x2,y2) = x1 * x2 + y1 * y2

-- | Determinant
v2_det :: Num t => V2 t -> V2 t -> t
v2_det (x1,y1) (x2,y2) = x1*y2 - x2*y1

-- | Rotate V2. Rotation is counter-clockwise.
--
-- > v2_rotate (pi/2) (1,0) `v2_approx_eq` (0,1)
v2_rotate :: Floating n => n -> V2 n -> V2 n
v2_rotate theta (x,y) =
    let c = cos theta
        s = sin theta
    in (x * c - y * s,y * c + x * s)

-- | Rotate point p1 r radians about p0.
--
-- > v2_rotate_about (pi/2) (1,0) (2,0) == (1,1)
v2_rotate_about :: Floating a => a -> V2 a -> V2 a -> V2 a
v2_rotate_about r p0 p1 = v2_rotate r (p1 `v2_sub` p0) `v2_add` p0

-- | n (x,y) -> (n*x,n*y)
--
-- > map (flip v2_scale (20,0)) [0,1,-1] == [(0,0),(20,0),(-20,-0)]
v2_scale :: Num n => n -> V2 n -> V2 n
v2_scale n = v2_map (* n)

-- | Pointwise '+'.
v2_add :: Num n => V2 n -> V2 n -> V2 n
v2_add = v2_zip (+)

-- | Translate by i along x-axis and j along y-axis.
v2_translate :: Num n => n -> n -> V2 n -> V2 n
v2_translate i j (x, y) = (x + i, y + j)

-- | 'foldl' of 'v2_add'.
v2_sum :: Num n => [V2 n] -> V2 n
v2_sum = foldl v2_add (0,0)

-- | Pointwise '-'.
v2_sub :: Num n => V2 n -> V2 n -> V2 n
v2_sub = v2_zip (-)

-- | (-x,-y)
v2_negate :: Num t => V2 t -> V2 t
v2_negate = v2_map negate

-- | Pointwise '*'.
v2_mul :: Num n => V2 n -> V2 n -> V2 n
v2_mul = v2_zip (*)

-- | Pointwise '/'.
v2_div :: Fractional n => V2 n -> V2 n -> V2 n
v2_div = v2_zip (/)

-- | If x < y then (x,y) else (y,x).
v2_sort :: Ord t => V2 t -> V2 t
v2_sort (i,j) = (min i j,max i j)

-- | 'v2_map' of 'round'
v2_round :: (RealFrac n,Integral i) => V2 n -> V2 i
v2_round = v2_map round

-- | Sum of squares (x·x + y·y)
v2_mag_sq :: Num t => V2 t -> t
v2_mag_sq (x,y) = sqr x + sqr y

-- | V2 length, ie. 'sqrt' of 'v2_mag_sq' (Frobenius norm)
v2_mag :: Floating t => V2 t -> t
v2_mag = sqrt . v2_mag_sq

-- | Euclidean distance function at V2
--
-- > v2_distance (0,0) (0,1) == 1
-- > v2_distance (0,0) (1,1) == sqrt 2
v2_distance :: Floating t => V2 t -> V2 t -> t
v2_distance p = v2_mag . v2_sub p

-- | p + q / 2
--
-- > v2_midpoint (0,0) (2,6) == (1,3)
v2_midpoint :: Fractional t => V2 t -> V2 t -> V2 t
v2_midpoint v = v2_scale (1/2) . v2_add v

{- | sum x / length x

> v2_centroid [] == undefined
> v2_centroid [(1,1)] == (1,1)
> v2_centroid [(0,0),(1,0),(1,1)] == (2/3,1/3)
-}
v2_centroid :: Fractional n => [V2 n] -> V2 n
v2_centroid x =
  case x of
    [] -> error "v2_centroid: nil?"
    [p] -> p
    _ -> v2_scale (1 / fromIntegral (length x)) (v2_sum x)

-- | 'min' of x and y.
v2_min :: Ord n => V2 n -> n
v2_min = uncurry min

-- | 'max' of x and y.
v2_max :: Ord n => V2 n -> n
v2_max = uncurry max

-- | Minimum x and y values for set of V2.
--
-- > v2_minimum (zip [1,2,3] [6,5,4]) == (1,4)
v2_minimum :: Ord n => [V2 n] -> V2 n
v2_minimum = foldr1 (v2_zip min)

-- | Maximum x and y values for set of V2.
v2_maximum :: Ord n => [V2 n] -> V2 n
v2_maximum = foldr1 (v2_zip max)

-- | (minima,maxima) of V2 set as ((x0,y0),(x1,y1))
--
-- > v2_bounds [(40,0),(0,40),(13,11),(-8,4)] == ((-8,0),(40,40))
v2_bounds :: Ord t => [V2 t] -> V2 (V2 t)
v2_bounds c = let r = unzip c in (v2_map minimum r,v2_map maximum r)

-- | Combine two (min,max) values.
v2_bounds_join :: (Num t,Ord t) => V2 (V2 t) -> V2 (V2 t) -> V2 (V2 t)
v2_bounds_join ((x0,y0),(x1,y1)) ((x2,y2),(x3,y3)) = ((min x0 x1,min y0 y1),(max x2 x3,max y2 y3))

-- | 'v2_min' of left and 'v2_max' of right bound, i.e. minima and maxima of all x and y together.
v2_extent_u :: Ord t => [V2 t] -> V2 t
v2_extent_u = bimap v2_min v2_max . v2_bounds

-- | (x-min,x-max) and (y-min,y-max) of set of V2.
--
-- > v2_extent_c [(-50,100),(50,0),(0,25)] == ((-50,50),(0,100))
v2_extent_c :: Ord t => [V2 t] -> V2 (V2 t)
v2_extent_c c =
  let (x,y) = unzip c
      minmax l = (minimum l,maximum l)
  in (minmax x,minmax y)

-- | Calculate linlin muladd (ie. hsc3 linlin_muladd)
v2_linlin_muladd :: Fractional n => V2 n -> V2 n -> V2 n
v2_linlin_muladd (sl,sr) (dl,dr) =
  let m = (dr - dl) / (sr - sl)
      a = dl - (m * sl)
  in (m,a)

-- | Calculate muladd values seperately for x and y.
v2_linlin_muladd_sep :: Fractional n => V2 (V2 n) -> V2 (V2 n) -> V2 (V2 n)
v2_linlin_muladd_sep (x_src,y_src) (x_dst,y_dst) =
  (v2_linlin_muladd x_src x_dst
  ,v2_linlin_muladd y_src y_dst)

{- | Linear map of V2 given (x,y) input and output ranges.

> r = [(1,0),(0.2,0.5),(0.46,0.1375),(0,0.05)]
> map (v2_linlin_rng_sep ((-10,40),(0,80)) ((0,1),(0,1))) [(40,0),(0,40),(13,11),(-10,4)] == r
-}
v2_linlin_rng_sep :: Fractional n => V2 (V2 n) -> V2 (V2 n) -> V2 n -> V2 n
v2_linlin_rng_sep src dst (x,y) =
  let ((xm,xa),(ym,ya)) = v2_linlin_muladd_sep src dst
  in (x * xm + xa,y * ym + ya)

{- | Apply equal linlin at x and y.

> r = [(1,0),(0,1),(0.325,0.275),(-0.2,0.1)]
> map (v2_linlin_rng (0,40) (0,1)) [(40,0),(0,40),(13,11),(-8,4)] == r
-}
v2_linlin_rng :: Fractional n => V2 n -> V2 n -> V2 n -> V2 n
v2_linlin_rng src dst = v2_linlin_rng_sep (src,src) (dst,dst)

{- | 'v2_linlin_rng_sep' with input range derived from input set using 'v2_extent_u'.

> v2_linlin_set_u ((0,1),(0,1)) [(-100,0),(0,100),(0,25)] == [(0.0,0.5),(0.5,1.0),(0.5,0.625)]
> v2_linlin_set_u ((-1,1),(-1,1)) [(-100,0),(0,100),(0,25)] == [(-1.0,0.0),(0.0,1.0),(0.0,0.25)]
-}
v2_linlin_set_u :: (Fractional n,Ord n) => V2 (V2 n) -> [V2 n] -> [V2 n]
v2_linlin_set_u r2 c =
  let r1 = v2_extent_u c
  in map (v2_linlin_rng_sep (r1,r1) r2) c

{- | 'v2_linlin_rng_sep' with input range derived from input set using 'v2_extent_c'.

> v2_linlin_set_c ((1,2),(2,4)) [(-50,0),(50,100),(0,25)] == [(1,2),(2,4),(1.5,2.5)]
> v2_linlin_set_c ((-10,10),(0,10)) [(-100,0),(0,100),(-25,25)] == [(-10,0),(10,10),(5,2.5)]
-}
v2_linlin_set_c :: (Fractional n,Ord n) => V2 (V2 n) -> [V2 n] -> [V2 n]
v2_linlin_set_c r2 c = map (v2_linlin_rng_sep (v2_extent_c c) r2) c

-- | The reflection of /p/ across a vertical line at /x/.
v2_reflect_x :: Num a => a -> V2 a -> V2 a
v2_reflect_x rx (x,y) = (rx + (rx - x),y)

-- | The reflection of 'Pt' across a horizontal line at /y/.
v2_reflect_y :: Num a => a -> V2 a -> V2 a
v2_reflect_y ry (x,y) = (x,ry + (ry - y))

-- | The reflection of p across the minimum distance to (/x/,/y/).
--
-- > v2_reflect_xy (1,1) (-1,0) == (3,2)
v2_reflect_xy :: Num a => V2 a -> V2 a -> V2 a
v2_reflect_xy (rx,ry) (x,y) = (rx + (rx - x),ry + (ry - y))

{- | Normalise, ie. scale to have unit magnitude (to within tolerance).

> let x = sqrt 2 / 2
> v2_mag_sq (x,x) ~= 1.0
> v2_normalize (x,x) == (x,x)
> v2_normalize (0,0) == (0,0)
> v2_normalize (1,1) -- ~= (x,x)
-}
v2_normalize :: (Eq n,Floating n) => V2 n -> V2 n
v2_normalize x = let m = v2_mag x in if m == 0 then x else v2_scale (1 / v2_mag x) x

{- | Normalise, ie. scale to have unit magnitude (to within tolerance).

> let x = sqrt 2 / 2
> v2_mag_sq (x,x) ~= 1.0
> v2_unit_vec_tol (x,x) == (x,x)
> v2_unit_vec_tol (0,0) == (0,0)
> v2_unit_vec_tol (1,1) -- ~= (x,x)
-}
v2_unit_vec_tol :: (Ord t, Floating t) => V2 t -> V2 t
v2_unit_vec_tol (x,y) =
  if abs (v2_mag_sq (x,y) - 1) < epsilon
  then (x,y)
  else if v2_mag_sq (x,y) == 0
       then (x,y)
       else let m = v2_mag (x,y) in (x / m,y / m)

{- | Angle to origin.
By convention the angle is zero rightwards along the X axis, and is measured counter-clockwise.

> map v2_angle_pt_0 [(0,1),(1,0),(0,-1),(-1,0)] == [pi/2,0,-pi/2,pi]
> v2_angle_pt_0 (2,1) ~= 0.463648
-}
v2_angle_pt_0 :: RealFloat r => V2 r -> r
v2_angle_pt_0 (x, y) = atan2 y x

-- | Angle from /p/ to /q/.
--
-- > v2_angle_pt (0,0) (0,1) == pi / 2
-- > v2_angle_pt (1,0) (0,1) == pi * 3 / 4
-- > v2_angle_pt (0,1) (0,1) == 0
v2_angle_pt :: RealFloat r => V2 r -> V2 r -> r
v2_angle_pt p q = v2_angle_pt_0 (q `v2_sub` p)

-- | The angle between two vectors on a plane. The angle is from v1 to
-- v2, positive anticlockwise.  The result is in (-pi,pi)
v2_angle_vec :: RealFloat r => V2 r -> V2 r -> r
v2_angle_vec (x1,y1) (x2,y2) =
    let t1 = atan2 y1 x1
        t2 = atan2 y2 x2
    in constrain (-pi,pi) (t2 - t1)

{- | Signed area, c.f. bv2_outer_product (x1 * y2 - y1 * x2)

> v2_signed_area (0, 2) (1, 0) == -2
> v2_signed_area (3, 0) (0, 2) == 6
> v2_signed_area (0, 1) (0, 1) == 0
> v2_signed_area (0, -1) (0, 1) == 0
> v2_signed_area (0, 1) (-1, -1) == 1
-}
v2_signed_area :: Num n => V2 n -> V2 n -> n
v2_signed_area (x1, y1) (x2, y2) = (x1 * y2) - (y1 * x2)

-- * 3-Vector (V3)

-- | 3-element vector.
type V3 n = (n,n,n)

v3_x,v3_y,v3_z :: V3 n -> n
v3_x (x,_,_) = x
v3_y (_,y,_) = y
v3_z (_,_,z) = z

v3_map :: (t -> u) -> V3 t -> V3 u
v3_map f (a,b,c) = (f a,f b,f c)

v3_zip :: (a -> b -> c) -> V3 a -> V3 b -> V3 c
v3_zip f (i,j,k) (p,q,r) = (f i p,f j q,f k r)

-- | Dot product
v3_dot :: Num t => V3 t -> V3 t -> t
v3_dot (x1,y1,z1) (x2,y2,z2) = x1 * x2 + y1 * y2 + z1 * z2

-- | The cross product is a vector that is perpendicular to both inputs, and normal to the plane containing them.
v3_cross_product :: Num t => V3 t -> V3 t -> V3 t
v3_cross_product (x1,y1,z1) (x2,y2,z2) =
  (y1 * z2 - y2 * z1
  ,x1 * z2 - x2 * z1
  ,x1 * y2 - x2 * y1)

-- | Scale V3.
v3_scale :: Num n => n -> V3 n -> V3 n
v3_scale n = v3_map (* n)

v3_add :: Num n => V3 n -> V3 n -> V3 n
v3_add = v3_zip (+)

v3_sum :: Num n => [V3 n] -> V3 n
v3_sum = foldl v3_add (0,0,0)

v3_sub :: Num n => V3 n -> V3 n -> V3 n
v3_sub = v3_zip (-)

v3_mul :: Num n => V3 n -> V3 n -> V3 n
v3_mul = v3_zip (*)

v3_div :: Fractional n => V3 n -> V3 n -> V3 n
v3_div = v3_zip (/)

v3_mag_sq :: Floating t => V3 t -> t
v3_mag_sq (x,y,z) = sqr x + sqr y + sqr z

-- | Magnitude or length of vector, 'sqrt' of 'v3_mag_sq' (Frobenius norm)
v3_mag :: Floating t => V3 t -> t
v3_mag = sqrt . v3_mag_sq

-- | Normalise vector, ie. divide by magnitude.
--
-- > v3_normalize (1,2,3) == let m = sqrt 14 in (1/m,2/m,3/m)
v3_normalize :: Floating t => V3 t -> V3 t
v3_normalize v = v3_scale (1 / v3_mag v) v

-- | Euclidean distance function at V3
--
-- > v3_distance (2,3,1) (8,-5,0) == 10.04987562112089
v3_distance :: Floating t => V3 t -> V3 t -> t
v3_distance p = v3_mag . v3_sub p

-- | p + q / 2
--
-- > v3_midpoint (0,0,0) (2,4,6) == (1,2,3)
v3_midpoint :: Fractional t => V3 t -> V3 t -> V3 t
v3_midpoint v = v3_scale (1/2) . v3_add v

-- | sum x / length x
--
-- > v3_centroid [] == undefined
-- > v3_centroid [(1,2,3)] == (1,2,3)
-- > v3_centroid [(0,0,0),(1,2,3),(2,4,6)] == (1,2,3)
v3_centroid :: Fractional n => [V3 n] -> V3 n
v3_centroid x =
  case x of
    [] -> error "v3_centroid: nil?"
    [p] -> p
    _ -> v3_scale (1 / fromIntegral (length x)) (v3_sum x)

v3_min :: Ord n => V3 n -> n
v3_min (x,y,z) = min (min x y) z

v3_max :: Ord n => V3 n -> n
v3_max (x,y,z) = max (max x y) z

v3_minimum :: Ord n => [V3 n] -> V3 n
v3_minimum = foldr1 (v3_zip min)

v3_maximum :: Ord n => [V3 n] -> V3 n
v3_maximum = foldr1 (v3_zip max)

-- | (v3_minimum,v3_maximum) of set.
v3_bounds :: Ord t => [V3 t] -> V2 (V3 t)
v3_bounds c = let r = unzip3 c in (v3_map minimum r,v3_map maximum r)

-- | Linear map of V3 given input and output ranges.
v3_linlin_rng :: Fractional n => V2 n -> V2 n -> V3 n -> V3 n
v3_linlin_rng (l0,r0) (l1,r1) p =
  let m = (r1 - l1) / (r0 - l0)
  in v3_add (v3_scale m (v3_sub p (l0,l0,l0))) (l1,l1,l1)

{- | 'v3_linlin_rng' with input range given by bounds of co-ordinates.

> v3_linlin_set (-1,1) [(-100,0,0),(0,100,0),(0,0,25)] == [(-1,0,0),(0,1,0),(0,0,0.25)]
> v3_linlin_set (0,1) [(-100,0,0),(0,100,0),(0,0,25)] == [(0,0.5,0.5),(0.5,1,0.5),(0.5,0.5,0.625)]
-}
v3_linlin_set :: (Ord t,Fractional t) => (t,t) -> [V3 t] -> [V3 t]
v3_linlin_set r c = map (v3_linlin_rng (bimap v3_min v3_max (v3_bounds c)) r) c

-- | Translate /p/ so center is at /c/.
--
-- > v3_center_at (0,0,0) [(-1,0,0),(0,1,2),(1,2,4)] == [(-1,-1,-2),(0,0,0),(1,1,2)]
-- > v3_center_at (1,1,1) [(1,1,1),(1,2,5)] == [(1,0.5,-1),(1,1.5,3)]
v3_center_at :: (Ord t,Fractional t) => V3 t -> [V3 t] -> [V3 t]
v3_center_at c p =
  let (v1,v2) = v3_bounds p
      r = v3_scale (1/2) (v3_sub v2 v1)
      d = v3_sub (v3_sub c r) v1
  in map (v3_add d) p

-- | Permute V3, (0,2,1).
v3_xyz_to_xzy :: V3 t -> V3 t
v3_xyz_to_xzy (x,y,z) = (x,z,y)

-- | Permute V3, (1,2,0).
v3_xyz_to_yzx :: V3 t -> V3 t
v3_xyz_to_yzx (x,y,z) = (y,z,x)

-- | Permute V3, (2,0,1).
v3_xyz_to_zxy :: V3 t -> V3 t
v3_xyz_to_zxy (x,y,z) = (z,x,y)

-- | Permute V3, (2,1,0).
v3_xyz_to_zyx :: V3 t -> V3 t
v3_xyz_to_zyx (x,y,z) = (z,y,x)

-- | Extract (x,y), (y,z) (x,z) from V3 into V2.
v3_xy,v3_yz,v3_xz :: V3 n -> V2 n
v3_xy (x,y,_) = (x,y)
v3_yz (_,y,z) = (y,z)
v3_xz (x,_,z) = (x,z)

-- | Apply /f/ at /x/
v3_on_x :: (t -> t) -> V3 t -> V3 t
v3_on_x f (x,y,z) = (f x,y,z)

-- | Apply /f/ at /y/
v3_on_y :: (t -> t) -> V3 t -> V3 t
v3_on_y f (x,y,z) = (x,f y,z)

-- | Apply /f/ at /z/
v3_on_z :: (t -> t) -> V3 t -> V3 t
v3_on_z f (x,y,z) = (x,y,f z)

-- | Apply /f/ at /x,y/
v3_on_xy :: (V2 t -> V2 t) -> V3 t -> V3 t
v3_on_xy f (x,y,z) = let (x',y') = f (x,y) in (x',y',z)

-- | Apply /f/ at /y,z/
v3_on_yz :: (V2 t -> V2 t) -> V3 t -> V3 t
v3_on_yz f (x,y,z) = let (y',z') = f (y,z) in (x,y',z')

-- | Apply /f/ at /x,z/
v3_on_xz :: (V2 t -> V2 t) -> V3 t -> V3 t
v3_on_xz f (x,y,z) = let (x',z') = f (x,z) in (x',y,z')

-- > v3_rotate_x (pi/2) (0,0,1) -- ~= (0,-1,0)
v3_rotate_x :: Floating n => n -> V3 n -> V3 n
v3_rotate_x theta = v3_on_yz (v2_rotate theta)

-- > v3_rotate_y (pi/2) (0,0,1) -- ~= (-1,0,0)
v3_rotate_y :: Floating n => n -> V3 n -> V3 n
v3_rotate_y theta = v3_on_xz (v2_rotate theta)

-- > v3_rotate_z (pi/2) (1,0,0) -- ~= (0,1,0)
-- > (v3_rotate_x (pi/2) . v3_rotate_z (-pi/2)) (1,2,4) -- ~= (2,-4,-1)
v3_rotate_z :: Floating n => n -> V3 n -> V3 n
v3_rotate_z theta = v3_on_xy (v2_rotate theta)

v3_max_abs :: (Ord t, Num t) => V3 t -> t
v3_max_abs (x,y,z) = max (max (abs x) (abs y)) (abs z)

-- | Scale so that the largest absolute coordinate value is one.
--
-- > v3_scale_to_max_abs (1,2,3) == (1/3,2/3,1)
v3_scale_to_max_abs :: (Fractional n, Ord n) => V3 n -> V3 n
v3_scale_to_max_abs v = v3_scale (1 / v3_max_abs v) v

-- * 4-Vector (V4)

-- | 4-element vector.
type V4 n = (n,n,n,n)

v4_x,v4_y,v4_z,v4_w :: V4 n -> n
v4_x (x,_,_,_) = x
v4_y (_,y,_,_) = y
v4_z (_,_,z,_) = z
v4_w (_,_,_,w) = w

v4_map :: (t -> u) -> V4 t -> V4 u
v4_map f (a,b,c,d) = (f a,f b,f c,f d)

-- | Scale V4.
v4_scale :: Num n => n -> V4 n -> V4 n
v4_scale n = v4_map (* n)

v4_zip :: (a -> b -> c) -> V4 a -> V4 b -> V4 c
v4_zip f (i,j,k,l) (p,q,r,s) = (f i p,f j q,f k r,f l s)

v4_zip3 :: (a -> b -> c -> d) -> V4 a -> V4 b -> V4 c -> V4 d
v4_zip3 f (i,j,k,l) (p,q,r,s) (a,b,c,d) = (f i p a,f j q b,f k r c,f l s d)

v4_negate :: Num t => V4 t -> V4 t
v4_negate = v4_map negate

-- | Dot product
v4_dot :: Num t => V4 t -> V4 t -> t
v4_dot (x1,y1,z1,w1) (x2,y2,z2,w2) = x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2

v4_add :: Num t => V4 t -> V4 t -> V4 t
v4_add = v4_zip (-)

v4_sub :: Num t => V4 t -> V4 t -> V4 t
v4_sub = v4_zip (-)

v4_mul :: Num t => V4 t -> V4 t -> V4 t
v4_mul = v4_zip (*)

v4_foldl :: (t -> t -> t) -> V4 t -> t
v4_foldl f (i,j,k,l) = f (f (f i j) k) l

-- | 'sqrt' of sum of squares
v4_mag :: Floating t => V4 t -> t
v4_mag (x,y,z,w) = sqrt (sqr x + sqr y + sqr z + sqr w)

-- | Euclidean distance function at V4
--
-- > v4_distance (2,0,0,0) (0,2,0,0) == 2 * sqrt 2
v4_distance :: Floating t => V4 t -> V4 t -> t
v4_distance p = v4_mag . v4_sub p

v4_min :: Ord n => V4 n -> n
v4_min (x,y,z,w) = min (min (min x y) z) w

v4_max :: Ord n => V4 n -> n
v4_max (x,y,z,w) = max (max (max x y) z) w

v4_minimum :: Ord n => [V4 n] -> V4 n
v4_minimum = foldr1 (v4_zip min)

v4_maximum :: Ord n => [V4 n] -> V4 n
v4_maximum = foldr1 (v4_zip max)

-- | (v4_minimum,v4_maximum) of set.
v4_bounds :: Ord t => [V4 t] -> V2 (V4 t)
v4_bounds c = let r = unzip4 c in (v4_map minimum r,v4_map maximum r)

-- | p + q / 2
--
-- > v4_midpoint (0,0,0,0) (2,4,6,8) == (1,2,3,4)
v4_midpoint :: Fractional t => V4 t -> V4 t -> V4 t
v4_midpoint v = v4_scale (1/2) . v4_add v

-- | Normalise V4 given input and output ranges.
v4_linlin_rng :: Fractional n => V2 n -> V2 n -> [V4 n] -> [V4 n]
v4_linlin_rng (l0,r0) (l1,r1) c =
  let m = (r1 - l1) / (r0 - l0)
      f p = v4_add (v4_scale m (v4_sub p (l0,l0,l0,l0))) (l1,l1,l1,l1)
  in map f c

-- | 'v4_linlin_rng' with input range given by bounds of co-ordinates.
v4_linlin_set :: (Ord t,Fractional t) => (t,t) -> [V4 t] -> [V4 t]
v4_linlin_set r c = v4_linlin_rng (bimap v4_min v4_max (v4_bounds c)) r c

-- | Normalise vector (ie. divide by magnitude)
v4_normalize :: Floating t => V4 t -> V4 t
v4_normalize v = v4_scale (1 / v4_mag v) v

-- | Extract in-order triples from V4 into V3.
v4_xyz,v4_xyw,v4_xzw,v4_yzw :: V4 n -> V3 n
v4_xyz (x,y,z,_) = (x,y,z)
v4_xyw (x,y,_,w) = (x,y,w)
v4_xzw (x,_,z,w) = (x,z,w)
v4_yzw (_,y,z,w) = (y,z,w)

-- | Divide all components by w
--
-- > v4_divide_by_w (4,6,8,2) == (2,3,4,1)
v4_divide_by_w :: (Fractional t) => V4 t -> V4 t
v4_divide_by_w (x,y,z,w) = (x/w,y/w,z/w,w/w)

-- | w -> (x,y,z) -> (x,y,z,w)
v4_v3_at_w :: t -> V3 t -> V4 t
v4_v3_at_w w (x,y,z) = (x,y,z,w)
