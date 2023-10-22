-- | Polygon
module Music.Theory.Geometry.Polygon where

import Data.Fixed {- base -}
import Data.List {- base -}

import Music.Theory.Geometry.Functions {- hmt-base -}
import Music.Theory.Geometry.Vector {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}

{- | Given (v1,ph0) and distances & angles [(d1,a1)..(dN,aN)] calculate vertices [v1..vN+1]
     d1 is the distance from v1 to v2.
     a1 is the interior angle at vertex v2.
     aN is the interior angle at v1 and is NOT consulted.
     If all interior angles and distances are given v1 will be (to within error) equal to vN+1.

>>> let f = map (v2_map round) . polygon_unfold_dgr ((0,0),0)
>>> let g n p q = f (zip (replicate n p) (replicate n q))
>>> g 3 100 60 -- equilateral triangle
[(0,0),(100,0),(50,87),(0,0)]

>>> g 4 100 90 -- square
[(0,0),(100,0),(100,100),(0,100),(0,0)]

>>> g 5 100 108 -- pentagon
[(0,0),(100,0),(131,95),(50,154),(-31,95),(0,0)]

>>> g 6 100 120 -- hexagon
[(0,0),(100,0),(150,87),(100,173),(0,173),(-50,87),(0,0)]

>>> g 7 100 (900/7)
[(0,0),(100,0),(162,78),(140,176),(50,219),(-40,176),(-62,78),(0,0)]

>>> g 8 100 135
[(0,0),(100,0),(171,71),(171,171),(100,241),(0,241),(-71,171),(-71,71),(0,0)]

>>> f (zip [30,82.24,73.27,60,60] [90,90,101.5,101.5,157])
[(0,0),(30,0),(30,82),(-43,82),(-55,23),(0,0)]
-}
polygon_unfold :: (RealFloat r,Real r) => ((r,r),r) -> [(r,r)] -> [V2 r]
polygon_unfold i =
  -- ph = phase, dst = distance, ang = interior angle
  let interior_angle_to_phase_increment x = pi - x
      increment_phase ph ang = ph + interior_angle_to_phase_increment ang `mod'` two_pi
      f (pt,ph) (dst,ang) =
        let r = v2_add (polar_to_rectangular (dst,ph)) pt
        in ((r,increment_phase ph ang),r)
  in (:) (fst i) . snd . mapAccumL f i

-- | 'polygon_unfold' of ((0,0),0)
polygon_unfold_0 :: (RealFloat r,Real r) => [(r,r)] -> [V2 r]
polygon_unfold_0 = polygon_unfold ((0,0),0)

{- | Variant with angles in degrees.

>>> let rnd = map (v2_map round)
>>> let u i j = rnd (polygon_unfold_dgr ((0,0),0) (zip i j))
>>> u [100,100,100] [60,60,60]
[(0,0),(100,0),(50,87),(0,0)]

>>> let phi = (1 + sqrt 5) / 2
>>> let f i = u (map (* 100) i)
>>> f [1,1,phi-1] (map (* (180 / 5)) [1,2,2])
[(0,0),(100,0),(19,59),(0,0)]

>>> f [1,phi,1] (map (* (180 / 5)) [1,1,3])
[(0,0),(100,0),(-31,95),(0,0)]

>>> let p = unzip . map (Data.Bifunctor.bimap round round) . polygon_param_dgr
>>> p [(0,0),(100,0),(19,59),(0,0)]
([100,100,62],[36,72,72])

>>> p [(0,0),(100,0),(-31,95),(0,0)]
([100,162,100],[36,36,108])

>>> p [(0,0),(62,0),(31,95),(-50,36),(0,0)]
([62,100,100,62],[72,72,72,144])

>>> p [(0,0),(62,0),(-19,59),(-50,-36),(0,0)]
([62,100,100,62],[36,72,36,216])
-}
polygon_unfold_dgr :: (RealFloat r,Real r) => ((r,r),r) -> [(r,r)] -> [V2 r]
polygon_unfold_dgr (i,j) = let at_snd f (p, q) = (p, f q) in polygon_unfold (i,degrees_to_radians j) . map (at_snd degrees_to_radians)

-- | 'polygon_unfold_dgr' of ((0,0),0)
polygon_unfold_dgr_0 :: (RealFloat r,Real r) => [(r,r)] -> [V2 r]
polygon_unfold_dgr_0 = polygon_unfold_dgr ((0,0),0)

{- | Inverse of 'polygon_unfold'.

>>> let f = polygon_param_dgr
>>> let h = unzip . map (Data.Bifunctor.bimap round round) . f
>>> h [(0,0),(100,0),(50,87),(0,0)]
([100,100,100],[60,60,60])

>>> h [(0,0),(100,0),(100,100),(0,100),(0,0)]
([100,100,100,100],[90,90,90,90])

>>> h [(0,0),(100,0),(131,95),(50,154),(-31,95),(0,0)]
([100,100,100,100,100],[108,108,108,108,108])

>>> h [(0,0),(30,0),(30,82),(-43,82),(-55,23),(0,0)]
([30,82,73,60,60],[90,90,101,101,157])

>>> let phi = (1 + sqrt 5) / 2
>>> let kite = polygon_unfold_dgr ((0,0),0) (zip (map (* 100) [1/phi,1,1,1/phi]) [72,72,72,144])
>>> h kite
([62,100,100,62],[72,72,72,144])
-}
polygon_param :: RealFloat r => [V2 r] -> [(r,r)]
polygon_param p =
    let phase_difference_to_interior_angle x = (- (x + pi)) `mod'` two_pi
        vc = map rectangular_to_polar (List.d_dx_by v2_sub p)
        acc_f z ph = (ph,phase_difference_to_interior_angle (ph - z))
        ph0 = v2_y (last vc)
    in zip (map v2_x vc) ((List.rotate (1::Int) . snd) (mapAccumL acc_f ph0 (map v2_y vc)))

-- | Variant with angles in degrees.
polygon_param_dgr :: RealFloat r => [V2 r] -> [(r,r)]
polygon_param_dgr = let at_snd f (p, q) = (p, f q) in map (at_snd radians_to_degrees) . polygon_param

-- | Polygon.
type Polygon t = [V2 t]

{- | Degree is the number of points.

>>> polygon_degree [(0,0),(1,0),(0,1)]
3
-}
polygon_degree :: Polygon t -> Int
polygon_degree = length

{- | List of /k/ edges of /k/ polygon.

>>> polygon_edges [(0,0),(1,0),(0,1)]
[((0,0),(1,0)),((1,0),(0,1)),((0,1),(0,0))]
-}
polygon_edges :: Polygon t -> [V2 (V2 t)]
polygon_edges l = zip l (List.tail_err (cycle l))

-- | Index /n/th point.
polygon_pt :: Polygon t -> Int -> V2 t
polygon_pt p k = p !! k

-- | Index /n/th edge.
polygon_edge :: Polygon t -> Int -> V2 (V2 t)
polygon_edge p k = polygon_edges p !! k

polygon_align_ln :: RealFloat t => Polygon t -> Int -> (Int,Bool) -> Polygon t
polygon_align_ln p k0 (k1,k1_rev) =
    let swap (i,j) = (j,i)
        l0 = polygon_edge p k0
        l1 = (if k1_rev then swap else id) (polygon_edge p k1)
        (tr,(c,r)) = line_align l0 l1
        f = v2_rotate_about r c . v2_add tr
    in map f p

polygon_reflect_ln_md :: Fractional t => V2 (V2 t) -> Polygon t -> Polygon t
polygon_reflect_ln_md ln = map (point_line_reflect ln)

polygon_reflect_xy :: Num t => (t,t) -> Polygon t -> Polygon t
polygon_reflect_xy xy = map (v2_reflect_xy xy)

polygon_edge_reflect :: Fractional t => Polygon t -> Int -> Polygon t
polygon_edge_reflect p k = polygon_reflect_ln_md (polygon_edge p k) p

polygon_pt_reflect :: Num t => Polygon t -> Int -> Polygon t
polygon_pt_reflect p k = polygon_reflect_xy (polygon_pt p k) p

polygon_reflect_x_right :: (Ord t, Num t) => Polygon t -> Polygon t
polygon_reflect_x_right l = let m = maximum (map v2_x l) in map (v2_reflect_x m) l

polygon_reflect_y_up :: (Ord t, Num t) => Polygon t -> Polygon t
polygon_reflect_y_up l = let m = maximum (map v2_y l) in map (v2_reflect_y m) l

{- | Does point lie inside polygon.

>>> polygon_contains_point [(0, 0),(1, 0),(1, 1),(0, 1)] (0.5, 0.5)
True
-}
polygon_contains_point :: (Ord t, Fractional t) => Polygon t -> V2 t -> Bool
polygon_contains_point p (x, y) =
  case p of
    [] -> error "polygon_contains_point"
    l0:l -> let xs = List.adj2 1 ((l0 : l) ++ [l0])
                f ((x1, y1), (x2, y2)) =
                  and [y > min y1 y2
                      ,y <= max y1 y2
                      ,x <= max x1 x2
                      ,y1 /= y2
                      ,x1 == x2 || x <= (y - y1) * (x2 - x1) / (y2 - y1) + x1]
            in odd (length (filter id (map f xs)))
