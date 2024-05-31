{- | Simple geometric functions.

Polygon variables:

n = degree, a = side length, r = ir = inradius, R = cr = circumradius, A = ar = area, s = sagitta
-}
module Music.Theory.Geometry.Functions where

import Data.Complex {- base -}

import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Math as Math {- hmt-base -}
import qualified Music.Theory.Tuple {- hmt-base -}

import Music.Theory.Geometry.Vector {- hmt-base -}

-- * Math

-- | Twice 'pi'
two_pi :: Floating n => n
two_pi = 2 * pi

-- | Secant.
sec :: Floating a => a -> a
sec z = 1 / cos z

-- | Cotangent.
cot :: Floating a => a -> a
cot z = 1 / tan z

{- | Degrees to radians.

>>> degrees_to_radians 60 == (pi/3)
True

>>> map degrees_to_radians [-180,-90,0,90,180] == [-pi,-pi/2,0,pi/2,pi]
True
-}
degrees_to_radians :: Floating n => n -> n
degrees_to_radians = (* pi) . (/ 180)

-- | (degrees,minutes) to radians
degrees_minutes_to_radians :: Floating n => (n, n) -> n
degrees_minutes_to_radians (i, j) = degrees_to_radians (i + (j / 60))

-- | (degrees,minutes,seconds) to radians
degrees_minutes_seconds_to_radians :: Floating n => (n, n, n) -> n
degrees_minutes_seconds_to_radians (i, j, k) = degrees_to_radians (i + (j / 60) + (k / 3600))

{- | Radians to degrees.

>>> radians_to_degrees (pi/3)
60.0

>>> map radians_to_degrees [-pi,-pi/2,0,pi/2,pi]
[-180.0,-90.0,0.0,90.0,180.0]

>>> radians_to_degrees (pi/8)
22.5

>>> radians_to_degrees 0.463648
26.565073579681595
-}
radians_to_degrees :: Floating n => n -> n
radians_to_degrees = (* 180) . (/ pi)

{- | (magnitude,phase:radians) -> (x,y)

>>> let f ph = Data.Bifunctor.bimap round round (polar_to_rectangular (100,ph))
>>> map f [0,pi/4 .. pi]
[(100,0),(71,71),(0,100),(-71,71),(-100,0)]
-}
polar_to_rectangular :: RealFloat t => V2 t -> V2 t
polar_to_rectangular (mg, ph) = let c = mkPolar mg ph in (realPart c, imagPart c)

{- | (magnitude,phase:degrees) -> (x,y)

>>> polar_to_rectangular_dgr (100,108) -- (-31,95)
(-30.901699437494734,95.10565162951536)

>>> let f ph = Data.Bifunctor.bimap round round (polar_to_rectangular_dgr (100,ph))
>>> map f [0,60 .. 180]
[(100,0),(50,87),(-50,87),(-100,0)]
-}
polar_to_rectangular_dgr :: RealFloat t => V2 t -> V2 t
polar_to_rectangular_dgr (i, j) = polar_to_rectangular (i, degrees_to_radians j)

-- | (x,y) -> (magnitude,phase:radians)
rectangular_to_polar :: RealFloat t => V2 t -> V2 t
rectangular_to_polar (x, y) = polar (x :+ y)

{- | (x,y) -> (magnitude,phase:degrees)

>>> v2_round (rectangular_to_polar_dgr (-31,95))
(100,108)
-}
rectangular_to_polar_dgr :: RealFloat t => V2 t -> V2 t
rectangular_to_polar_dgr = (\(i, j) -> (i, radians_to_degrees j)) . rectangular_to_polar

-- * Triangle

{- | <https://mathworld.wolfram.com/Semiperimeter.html> sss=side-side-side

>>> triangle_semiperimeter_sss 1 1 1
1.5
-}
triangle_semiperimeter_sss :: Fractional a => a -> a -> a -> a
triangle_semiperimeter_sss a b c = (1 / 2) * (a + b + c)

{- | <https://mathworld.wolfram.com/Area.html>

>>> triangle_area_herons_sss 1 1 1
0.4330127018922193
-}
triangle_area_herons_sss :: Floating a => a -> a -> a -> a
triangle_area_herons_sss a b c =
  let s = triangle_semiperimeter_sss a b c
  in sqrt (s * (s - a) * (s - b) * (s - c))

{- | Area (aas)

>>> triangle_area_aas (two_pi / 3) (pi / 6) 1
0.14433756729740643
-}
triangle_area_aas :: Floating a => a -> a -> a -> a
triangle_area_aas alpha beta a =
  let gamma = pi - alpha - beta
      n = Math.sqr a * sin beta * sin gamma
      d = 2 * sin alpha
  in n / d

{- | Area (asa)

>>> triangle_area_asa (pi / 6) 1 (pi / 6)
0.14433756729740643
-}
triangle_area_asa :: Floating a => a -> a -> a -> a
triangle_area_asa alpha c beta =
  let n = Math.sqr c
      d = 2 * (cot alpha + cot beta)
  in n / d

{- | Side (asa)

>>> triangle_side_asa (pi / 6) 1 (pi / 6)
0.5773502691896258
-}
triangle_side_asa :: Floating a => a -> a -> a -> a
triangle_side_asa alpha c beta =
  let gamma = pi - alpha - beta
  in (sin alpha / sin gamma) * c

-- | <http://mathworld.wolfram.com/LawofCosines.html>
law_of_cosines :: Floating a => a -> a -> a -> a
law_of_cosines s a s' = sqrt (Math.sqr s + Math.sqr s' - 2 * s * s' * cos a)

{- | Given lengths of two sides (a and b) and the angle between them (gamma) calculate side length (c).
Argument order is: a gamma b.

>>> triangle_side_sas 0.5 (two_pi / 3) 0.5
0.8660254037844386
-}
triangle_side_sas :: Floating a => a -> a -> a -> a
triangle_side_sas = law_of_cosines

{- | Centroid

>>> triangle_centroid (-1,0) (0,1) (1,0) == (0,1/3)
True
-}
triangle_centroid :: Fractional t => V2 t -> V2 t -> V2 t -> V2 t
triangle_centroid (x1, y1) (x2, y2) (x3, y3) =
  let x = (x1 + x2 + x3) / 3
      y = (y1 + y2 + y3) / 3
  in (x, y)

-- | Given lengths of three sides (a, b and c) calculate the angle between b and c (alpha).
triangle_angle_alpha :: Floating a => a -> a -> a -> a
triangle_angle_alpha a b c = acos ((b * b + c * c - a * a) / (2 * b * c))

-- | Given lengths of three sides (a, b and c) calculate the angle between a and c (beta).
triangle_angle_beta :: Floating a => a -> a -> a -> a
triangle_angle_beta a b c = acos ((a * a + c * c - b * b) / (2 * a * c))

-- | Given lengths of three sides (a, b and c) calculate the angle between a and b (gamma).
triangle_angle_gamma :: Floating a => a -> a -> a -> a
triangle_angle_gamma a b c = acos ((a * a + b * b - c * c) / (2 * a * b))

-- * Equilateral triangle

{- | Equilateral triangle area (sss)

>>> equilateral_triangle_area 1 == triangle_area_herons_sss 1 1 1
True
-}
equilateral_triangle_area :: Floating a => a -> a
equilateral_triangle_area a = (sqrt 3 / 4) * (a * a)

{- | Circumradius

>>> equilateral_triangle_circumradius 1 == regular_polygon_circumradius_a 3 1
True
-}
equilateral_triangle_circumradius :: Floating a => a -> a
equilateral_triangle_circumradius a = a / sqrt 3

{- | Inradius

>>> equilateral_triangle_inradius 1 Math.~= regular_polygon_inradius_a 3 1
True
-}
equilateral_triangle_inradius :: Floating a => a -> a
equilateral_triangle_inradius = (/ 2) . equilateral_triangle_circumradius

-- | Ccw unit square.  <https://mathworld.wolfram.com/UnitSquare.html>
unit_square :: Num n => [V2 n]
unit_square = [(0, 0), (1, 0), (1, 1), (0, 1)]

-- * Pentagon

{- | Regular pentagon circumradius (R), c.f. <https://mathworld.wolfram.com/Circumradius.html>

>>> pentagon_circumradius_a 1
0.85065080835204

>>> regular_polygon_circumradius_a 5 1
0.8506508083520399
-}
pentagon_circumradius_a :: Floating a => a -> a
pentagon_circumradius_a a = (1 / 10) * sqrt (50 + 10 * sqrt 5) * a

{- | Regular pentagon inradius (r), c.f. <https://mathworld.wolfram.com/Inradius.html>

>>> pentagon_inradius_a 1 == regular_polygon_inradius_a 5 1
True
-}
pentagon_inradius_a :: Floating a => a -> a
pentagon_inradius_a a = (1 / 10) * sqrt (25 + 10 * sqrt 5) * a

{- | Regular pentagon sagitta, c.f. <https://mathworld.wolfram.com/Sagitta.html>

>>> pentagon_sagitta_a 1
0.16245984811645314

>>> regular_polygon_sagitta_cr 5 (pentagon_circumradius_a 1) == pentagon_sagitta_a 1
True
-}
pentagon_sagitta_a :: Floating a => a -> a
pentagon_sagitta_a a = (1 / 10) * sqrt (25 - 10 * sqrt 5) * a

{- | Regular pentagon area, c.f. <https://mathworld.wolfram.com/Area.html>

>>> pentagon_area_a 1
1.720477400588967

>>> pentagon_area_a 1 == regular_polygon_area_a 5 1
True
-}
pentagon_area_a :: Floating a => a -> a
pentagon_area_a a = (1 / 4) * sqrt (25 + 10 * sqrt 5) * a * a

-- * Hexagon

{- | The radius of a polygon's incircle or of a polyhedron's insphere,
     denoted r or sometimes rho.  <https://mathworld.wolfram.com/Inradius.html>
     The indiameter of a unit hexagon is (sqrt 3).

>>> hexagon_inradius_a 1
0.8660254037844386

>>> regular_polygon_inradius_a 6 1
0.8660254037844387

>>> hexagon_inradius_a 1 == sqrt 3 / 2
True
-}
hexagon_inradius_a :: Floating a => a -> a
hexagon_inradius_a a = 0.5 * sqrt 3 * a

{- | For hexagons, the side length (a) and the circumradius (R) are equal.

>>> hexagon_circumradius_a 1
1

>>> hexagon_circumradius_a 1 Math.~= regular_polygon_circumradius_a 6 1
True
-}
hexagon_circumradius_a :: a -> a
hexagon_circumradius_a = id

{- | The perpendicular distance h from an arc's midpoint to the chord
   across it, equal to the radius R minus the apothem r.
   <https://mathworld.wolfram.com/Sagitta.html>

>>> hexagon_sagitta_a 1
0.1339745962155614

>>> regular_polygon_sagitta_cr 6 (hexagon_circumradius_a 1)
0.13397459621556132
-}
hexagon_sagitta_a :: Floating a => a -> a
hexagon_sagitta_a a = 0.5 * (2 - sqrt 3) * a

{- | Given a circle, the apothem is the perpendicular distance r from
   the midpoint of a chord to the circle's center. It is also equal to
   the radius R minus the sagitta h.  For a regular polygon, the
   apothem simply is the distance from the center to a side, i.e., the
   inradius r of the polygon. <https://mathworld.wolfram.com/Apothem.html>

>>> hexagon_apothem_a 1
0.8660254037844386

>>> regular_polygon_apothem_a 6 1
0.8660254037844387
-}
hexagon_apothem_a :: Floating a => a -> a
hexagon_apothem_a = hexagon_inradius_a

{- | Area of hexagon of indicated side length.

>>> hexagon_area_a 1
2.598076211353316
-}
hexagon_area_a :: Floating a => a -> a
hexagon_area_a a = 1.5 * sqrt 3 * Math.sqr a

-- * Polygon

{- | <http://mathworld.wolfram.com/PolygonArea.html>

Note that the area of a convex polygon is defined to be positive if
the points are arranged in a counterclockwise order, and negative
if they are in clockwise order (Beyer 1987).

>>> let u = [(0,0),(1,0),(1,1),(0,1)]
>>> polygon_signed_area u
1.0

>>> polygon_signed_area (reverse u)
-1.0
-}
polygon_signed_area :: Fractional t => [V2 t] -> t
polygon_signed_area p =
  let q = zip p (List.tail_err (cycle p))
      f ((x1, y1), (x2, y2)) = x1 * y2 - x2 * y1
  in sum (map f q) / 2

{- | a = 2 × R × sin (π / n)

>>> regular_polygon_side_length 5 1
1.1755705045849463
-}
regular_polygon_side_length :: Floating a => a -> a -> a
regular_polygon_side_length n cr = 2 * cr * sin (pi / n)

{- | r = R × cos (π / n)

>>> map (\n -> regular_polygon_inradius_cr n 1) [3,4,5,6]
[0.5000000000000001,0.7071067811865476,0.8090169943749475,0.8660254037844387]
-}
regular_polygon_inradius_cr :: Floating a => a -> a -> a
regular_polygon_inradius_cr n cr = cr * cos (pi / n)

{- | r = ½ × a × cot (π / n)

>>> map (\n -> regular_polygon_circumradius_a n 1) [3,4,5,6]
[0.5773502691896258,0.7071067811865476,0.8506508083520399,1.0000000000000002]
-}
regular_polygon_inradius_a :: Floating a => a -> a -> a
regular_polygon_inradius_a n a = (1 / 2) * a * cot (pi / n)

{- | R = r × cot (π / n)

>>> map (\n -> regular_polygon_circumradius_r n 1) [3,4,5,6]
[0.577350269189626,1.0000000000000002,1.3763819204711736,1.7320508075688774]
-}
regular_polygon_circumradius_r :: Floating a => a -> a -> a
regular_polygon_circumradius_r n r = r * cot (pi / n)

{- | R = ½ × a × csc (π / n)

>>> map (\n -> regular_polygon_circumradius_a n 1) [3,4,5,6]
[0.5773502691896258,0.7071067811865476,0.8506508083520399,1.0000000000000002]
-}
regular_polygon_circumradius_a :: Floating a => a -> a -> a
regular_polygon_circumradius_a n a =
  let csc x = 1 / sin x
  in (1 / 2) * a * csc (pi / n)

{- | A = ½ × n × R × R × sin (2 × π / n)

>>> regular_polygon_area_cr 6 1
2.598076211353316
-}
regular_polygon_area_cr :: Floating a => a -> a -> a
regular_polygon_area_cr n cr = (1 / 2) * n * Math.sqr cr * sin (two_pi / n)

{- | A = ¼ × n × a × a × cot (π / n)

>>> map (\n -> regular_polygon_area_a n 1) [3,4,5,6]
[0.43301270189221946,1.0000000000000002,1.720477400588967,2.598076211353316]
-}
regular_polygon_area_a :: Floating a => a -> a -> a
regular_polygon_area_a n a = (1 / 4) * n * Math.sqr a * cot (pi / n)

{- | Sagitta

>>> regular_polygon_sagitta_cr 6 1
0.13397459621556132
-}
regular_polygon_sagitta_cr :: Floating a => a -> a -> a
regular_polygon_sagitta_cr n cr = 2 * cr * Math.sqr (sin (pi / (2 * n)))

{- | Apothem given side length.

>>> regular_polygon_apothem_a 6 1
0.8660254037844387
-}
regular_polygon_apothem_a :: Floating a => a -> a -> a
regular_polygon_apothem_a n a = a / (2 * tan (pi / n))

{- | Apothem given side circumradius.

>>> regular_polygon_apothem_r 6 1
0.8660254037844387
-}
regular_polygon_apothem_r :: Floating a => a -> a -> a
regular_polygon_apothem_r n r = r * cos (pi / n)

-- * Cube

{- | C3 (cube) vertices, lexographic ordering.

        3-------7
       /|      /|
      2-+-----6 |
      | |     | |   y
      | 1-----+-5   | z
      |/      |/    |/
      0-------4     +--x

>>> cube_vertices (0,1)
[(0,0,0),(0,0,1),(0,1,0),(0,1,1),(1,0,0),(1,0,1),(1,1,0),(1,1,1)]
-}
cube_vertices :: Num n => V2 n -> [V3 n]
cube_vertices (i, j) = [(i, i, i), (i, i, j), (i, j, i), (i, j, j), (j, i, i), (j, i, j), (j, j, i), (j, j, j)]

{- | Un-directed graph of cube given lexographic labelling as above.
Construction of Q3 by connecting pairs of corresponding vertices in two copies of Q2

>>> let (v,e) = cube_graph
>>> (length v,length e)
(8,12)
-}
cube_graph :: ([Int], [(Int, Int)])
cube_graph =
  let v = [0, 4, 6, 2, 1, 5, 7, 3]
      adj2 l = zip l (List.tail_err l)
      e = concatMap adj2 [[0, 4, 6, 2, 0], [1, 5, 7, 3, 1], [0, 1], [4, 5], [2, 3], [6, 7]]
  in (v, e)

{- | Table to label cube_vertices as below.

        a-------b
       /|      /|
      A-+-----B |
      | |     | |   y
      | c-----+-d   | z
      |/      |/    |/
      C-------D     +--x

>>> import Data.List
>>> map snd (sort cube_vertices_label_tbl)
"CcAaDdBb"
-}
cube_vertices_label_tbl :: [(Int, Char)]
cube_vertices_label_tbl = zip [2, 6, 0, 4, 3, 7, 1, 5] "ABCDabcd"

{- | C4 (8-cell, octachoron, tesseract, 4-cube) vertices, lexographic ordering.

>>> length (tesseract_vertices (0,1))
16
-}
tesseract_vertices :: Num n => V2 n -> [V4 n]
tesseract_vertices (i, j) =
  let c3 = cube_vertices (i, j)
  in concatMap (\n -> map (\(x, y, z) -> (n, x, y, z)) c3) [i, j]

{- | Un-directed graph of tesseract.
Construction of Q4 by connecting pairs of corresponding vertices in two copies of Q3.

>>> let (v,e) = tesseract_graph
>>> (length v,length e)
(16,32)
-}
tesseract_graph :: ([Int], [(Int, Int)])
tesseract_graph =
  let (c3_v, c3_e) = cube_graph
      c4_v = c3_v ++ map (+ 8) c3_v
      c4_e =
        concat
          [ c3_e
          , map (\(i, j) -> (i + 8, j + 8)) c3_e
          , zip [0 .. 7] [8 .. 15]
          ]
  in (c4_v, c4_e)

{- | Table to label tesseract_vertices as cube_vertices.

>>> import Data.List
>>> map snd (sort tesseract_vertices_label_tbl)
"CcAaDdBbGgEeHhFf"
-}
tesseract_vertices_label_tbl :: [(Int, Char)]
tesseract_vertices_label_tbl =
  let f (i, c) = (i + 8, toEnum (fromEnum c + 4))
  in cube_vertices_label_tbl ++ map f cube_vertices_label_tbl

{- | C5 (5-cell, pentachoron, pentatope, 4-simplex) co-ordinates.
     All vertices are connected by edges.

>>> let c = pentatope (0,2)
>>> Data.List.nub [v4_distance i j | i <- c,j <- c, i < j]
[2.8284271247461903]
-}
pentatope :: (Num t, Floating t) => (t, t) -> [V4 t]
pentatope (i, j) =
  let phi = (1 + sqrt 5) / 2
      k = 0.5 * phi * (j - i) + i
  in [(j, i, i, i), (i, j, i, i), (i, i, j, i), (i, i, i, j), (k, k, k, k)]

-- | C5 graph. |v|=4 |e|=10
pentatope_graph :: ([Int], [(Int, Int)])
pentatope_graph = let v = [0 .. 4] in (v, [(i, j) | i <- v, j <- v, i < j])

{- | C16 (16-cell, hexadecachoron, 4-orthoplex)
     All non-oposite vertices are connected by edges.
     Opposite vertices are adjacent (even,odd) in this ordering.
-}
hexadecachoron :: Num t => [V4 t]
hexadecachoron =
  [ (1, 0, 0, 0)
  , (-1, 0, 0, 0)
  , (0, 1, 0, 0)
  , (0, -1, 0, 0)
  , (0, 0, 1, 0)
  , (0, 0, -1, 0)
  , (0, 0, 0, 1)
  , (0, 0, 0, -1)
  ]

-- | C16 graph. |v|=8 |e|=24. Opposite vertices are adjacent (even,odd).
hexadecachoron_graph :: ([Int], [(Int, Int)])
hexadecachoron_graph =
  let v = [0 .. 7]
      f i j = odd i || j /= (i + 1)
  in (v, [(i, j) | i <- v, j <- v, i < j, f i j])

-- * Fano

{- | Graph of Fano plane with zero-indexed nimber labelling of nodes.

>>> let (v,e) = fano_plane_graph
>>> (length v,length e)
(7,15)

> import Music.Theory.Graph.Type
> gr_map (subtract 1) fano_plane_graph
([-1,0,1,2,3,4,5],[(-1,1),(-1,3),(-1,5),(0,1),(0,4),(0,5),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,4),(3,5),(4,5)])
-}
fano_plane_graph :: ([Int], [(Int, Int)])
fano_plane_graph =
  ( [0, 1, 2, 3, 4, 5, 6]
  , [(0, 2), (0, 4), (0, 6), (1, 2), (1, 5), (1, 6), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6), (4, 5), (4, 6), (5, 6)]
  )

{- | Polar co-ordinates for the standard drawing of the Fano plane,
with circumradius /cr/ and P7 at (0,0).
-}
fano_plane_coord :: Floating n => n -> [V2 n]
fano_plane_coord cr =
  let r = regular_polygon_inradius_cr 3 cr
      p7 = (0, 0)
      (p6, p3, p5) = Music.Theory.Tuple.t3_from_list (map (\n -> (r, pi / 6 + two_pi * n)) [0, 1 / 3, 2 / 3])
      (p2, p1, p4) = Music.Theory.Tuple.t3_from_list (map (\n -> (cr, pi / 2 + two_pi * n)) [0, 1 / 3, 2 / 3])
  in [p1, p2, p3, p4, p5, p6, p7]

-- * Intersect

{- | The point at /z/ along line, 0 is the start of the line and 1 is the end.

>>> point_along_line 0.5 ((0,0),(1,1))
(0.5,0.5)
-}
point_along_line :: Num n => n -> V2 (V2 n) -> V2 n
point_along_line z (p1, p2) =
  let v = v2_scale z (v2_sub p2 p1)
  in v2_add v p1

{- | Minimal distance between a point and a line.
     Calculates distance along indicated line segment and intersection point.

     (p1,p2) p3 -> (u,p4).
     (p1,p2) is a line segment, p3 is a point.
     u is the distance along (p1,p2) to which p3 is nearest.
     p4 is the nearest point to p3 along the line (p1,p2).
     If u is not in (0,1) then the intersection point is not in the line segment.

     <http://paulbourke.net/geometry/pointlineplane/>

>>> map (point_line_intersect ((0,0),(10,10))) [(10,5),(15,0)]
[(0.75,(7.5,7.5)),(0.75,(7.5,7.5))]

>>> point_line_intersect ((0,0),(10,10)) (-2.5,-7.5)
(-0.5,(-5.0,-5.0))
-}
point_line_intersect :: Fractional t => V2 (V2 t) -> V2 t -> (t, V2 t)
point_line_intersect ((x1, y1), (x2, y2)) (x3, y3) =
  let xd = x2 - x1
      yd = y2 - y1
      u = ((x3 - x1) * xd + (y3 - y1) * yd) / (xd * xd + yd * yd)
      x4 = x1 + u * (x2 - x1)
      y4 = y1 + u * (y2 - y1)
  in (u, (x4, y4))

{- | Minimum distance between a point and a line, and a flag indicating
   if the intersection point is within the indicated line segment or
   not.

>>> map (point_line_distance ((0,0),(10,10))) [(2,7.5),(-7.5,-2)]
[(True,3.8890872965260113),(False,3.8890872965260113)]
-}
point_line_distance :: (Floating t, Ord t) => V2 (V2 t) -> V2 t -> (Bool, t)
point_line_distance (p1, p2) p3 =
  let (u, p4) = point_line_intersect (p1, p2) p3
  in (u >= 0 && u <= 1, v2_distance p3 p4)

{- | 'v2_reflect_xy' about 'point_line_intersect'.

>>> map (point_line_reflect ((0,0),(1,1))) [(1,0),(0.25,1)]
[(0.0,1.0),(1.0,0.25)]
-}
point_line_reflect :: Fractional a => (V2 a, V2 a) -> V2 a -> V2 a
point_line_reflect ln p =
  let (_, r) = point_line_intersect ln p
  in v2_reflect_xy r p

-- * Align

{- | Give translation and rotation from /p/ to /q/.
     Magnitude is not considered.

>>> line_align ((0,0),(1,0)) ((1,1),(1,2))
((1.0,1.0),((1.0,1.0),-4.71238898038469))
-}
line_align :: RealFloat r => V2 (V2 r) -> V2 (V2 r) -> (V2 r, (V2 r, r))
line_align p q =
  let (pt0, vc0) = (fst p, uncurry v2_sub p)
      (pt1, vc1) = (fst q, uncurry v2_sub q)
      (_, ph0) = rectangular_to_polar vc0
      (_, ph1) = rectangular_to_polar vc1
  in (pt1 `v2_sub` pt0, (pt1, ph1 - ph0))

{- | Find interesction of two infinite lines.

>>> line_line_intersection ((4,0),(6,10)) ((0,3),(10,7))
(5.0,5.0)

If lines are parallel, intersection point will contain infinite values.

>>> line_line_intersection ((0,0),(1,1)) ((1,2),(4,5))
(-Infinity,-Infinity)
-}
line_line_intersection :: Fractional r => V2 (V2 r) -> V2 (V2 r) -> V2 r
line_line_intersection ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) =
  let a1 = y2 - y1
      b1 = x1 - x2
      c1 = (a1 * x1) + (b1 * y1)
      a2 = y4 - y3
      b2 = x3 - x4
      c2 = (a2 * x3) + (b2 * y3)
      delta = (a1 * b2) - (a2 * b1)
  in (((b2 * c1) - (b1 * c2)) / delta
     ,((a1 * c2) - (a2 * c1)) / delta)

-- * Circle

{- | Distances along a line, given as Pt and Vc, that it intersects with a circle.

>>> let o = (0,0)
>>> let c = (o,1)
>>> let z = sqrt 2 / 2
>>> map (\v -> line_circle_intersection (o,v) c) [(1,0),(1,1)] == [Just (1,-1),Just (z,-z)]
True
-}
line_circle_intersection :: (Ord a, Floating a) => (V2 a, V2 a) -> (V2 a, a) -> Maybe (V2 a)
line_circle_intersection ((lx, ly), (dx, dy)) ((cx, cy), r) =
  let a = (dx * dx + dy * dy)
      b = 2 * ((lx - cx) * dx + (ly - cy) * dy)
      c = (lx - cx) * (lx - cx) + (ly - cy) * (ly - cy) - r * r
      z = b * b - 4 * a * c
  in if z < 0
      then Nothing
      else
        if a == 0
          then if c == 0 then Just (0, 0) else Nothing
          else
            Just
              ( (-b + sqrt z) / (2 * a)
              , (-b - sqrt z) / (2 * a)
              )

-- * Spherical

{- |  Inclination (theta, polar angle, zenith angle, colatitude) to elevation (delta, latitude).
Inclination is in (0,pi) measuring from the positive z-axis.
Elevation is in (-pi/2, pi/2) measuring from the x-y plane.

>>> let f = round . radians_to_degrees . inclination_to_elevation . (* pi)
>>> map f [0, 1/4, 1/2, 3/4, 1]
[90,45,0,-45,-90]
-}
inclination_to_elevation :: RealFloat n => n -> n
inclination_to_elevation theta = negate (theta - (pi / 2))

{- | Inverse of inclination_to_elevation.

>>> let f = round . radians_to_degrees . elevation_to_inclination . (* pi)
>>> map f [1/2, 1/4, 0, -1/4, -1/2]
[0,45,90,135,180]
-}
elevation_to_inclination :: RealFloat n => n -> n
elevation_to_inclination delta = (pi / 2) - delta

{- | Iso convention (r=radius, theta=θ=inclination=X, phi=φ=azimuth=Z)

>>> let c2s = cartesian_to_spherical
>>> c2s (spherical_to_cartesian (1,1,1)) -- identity
(1.0,1.0,1.0)

>>> map c2s [(0,0,1),(0,1,0),(1,0,0)] `v3_list_approx_eq` [(1,0,0),(1,pi/2,pi/2),(1,pi/2,0)]
True

>>> map c2s [(1,1,0),(1,0,1),(0,1,1)] `v3_list_approx_eq` [(sqrt 2,pi/2,pi/4),(sqrt 2,pi/4,0),(sqrt 2,pi/4,pi/2)]
True

>>> c2s (1,1,1) `v3_approx_eq` (sqrt 3,atan (sqrt 2),pi/4)
True

>>> map c2s [(0,0,-1),(0,-1,0),(-1,0,0)] `v3_list_approx_eq` [(1,pi,0),(1,pi/2,-pi/2),(1,pi/2,pi)]
True

>>> map c2s [(0,0,-1),(0,0,1)] `v3_list_approx_eq` [(1,pi,0),(1,0,0)]
True
-}
cartesian_to_spherical :: RealFloat n => V3 n -> V3 n
cartesian_to_spherical (x, y, z) =
  let rho = sqrt (x * x + y * y + z * z)
      theta = atan2 (sqrt (x * x + y * y)) z -- acos (z / r)
      phi = atan2 y x -- atan (y / x)
  in (rho, theta, phi)

{- | Iso convention (r=𝑟=radius, theta=θ=inclination=X, phi=φ=azimuth=Z)

By convention: r ≥ 0, 0 ≤ θ ≤ π, 0 ≤ φ < 2π

>>> let s2c = spherical_to_cartesian
>>> s2c (cartesian_to_spherical (1,1,1)) `v3_approx_eq` (1,1,1)
True

>>> map s2c [(1,0,0),(1,pi/2,pi/2),(1,pi/2,0)] `v3_list_approx_eq` [(0,0,1),(0,1,0),(1,0,0)] -- Z Y X
True

>>> map s2c [(sqrt 2,pi/2,pi/4),(sqrt 2,pi/4,0),(sqrt 2,pi/4,pi/2)] `v3_list_approx_eq` [(1,1,0),(1,0,1),(0,1,1)]
True

>>> s2c (sqrt 3,atan (sqrt 2),pi/4) `v3_approx_eq` (1,1,1)
True

>>> map s2c [(1,pi,0),(1,pi/2,-pi/2),(1,pi/2,pi)] `v3_list_approx_eq` [(0,0,-1),(0,-1,0),(-1,0,0)]
True
-}
spherical_to_cartesian :: Floating n => V3 n -> V3 n
spherical_to_cartesian (r, theta, phi) =
  ( r * sin theta * cos phi
  , r * sin theta * sin phi
  , r * cos theta
  )

-- * Surface Normal

{- | <https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal>

>>> v3_p3_normal ((0,0,0),(1,0,0),(0,1,0))
(0,0,1)

>>> v3_p3_normal ((0,0,1),(1,0,0),(0,1,0))
(1,1,1)
-}
v3_p3_normal :: Num t => V3 (V3 t) -> V3 t
v3_p3_normal (p1, p2, p3) =
  let (ux, uy, uz) = v3_sub p2 p1
      (vx, vy, vz) = v3_sub p3 p1
  in ( uy * vz - uz * vy
     , uz * vx - ux * vz
     , ux * vy - uy * vx
     )

{- | 'v3_normalize' of 'v3_p3_normal'.

>>> v3_p3_normal_unit ((0,0,0),(1,0,0),(0,1,0))
(0.0,0.0,1.0)

>>> v3_p3_normal_unit ((0,0,1),(1,0,0),(0,1,0)) == (1/sqrt 3,1/sqrt 3,1/sqrt 3)
True
-}
v3_p3_normal_unit :: Floating t => V3 (V3 t) -> V3 t
v3_p3_normal_unit = v3_normalize . v3_p3_normal

{- | <https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal>

>>> v3_normal [(0,0,0),(1,0,0),(0,1,0)]
(0,0,1)

>>> v3_normal [(0,0,1),(1,0,0),(0,1,0)]
(1,1,1)
-}
v3_normal :: Num t => [V3 t] -> V3 t
v3_normal =
  let close l = l ++ [List.head_err l]
      recur (nx, ny, nz) p =
        case p of
          (x1, y1, z1) : (x2, y2, z2) : _ ->
            recur
              ( nx + ((y1 - y2) * (z1 + z2))
              , ny + ((z1 - z2) * (x1 + x2))
              , nz + ((x1 - x2) * (y1 + y2))
              )
              (List.tail_err p)
          _ -> (nx, ny, nz)
  in recur (0, 0, 0) . close

{- | 'v3_normalize' of 'v3_normal'.

>>> v3_normal_unit [(0,0,0),(1,0,0),(0,1,0)]
(0.0,0.0,1.0)

>>> v3_normal_unit [(0,0,1),(1,0,0),(0,1,0)] == (1/sqrt 3,1/sqrt 3,1/sqrt 3)
True
-}
v3_normal_unit :: Floating t => [V3 t] -> V3 t
v3_normal_unit = v3_normalize . v3_normal

-- * Trapezoid

{- | A right trapezoid is a trapezoid having two right angles.
     <https://mathworld.wolfram.com/RightTrapezoid.html>
     Area of right trapezoid.

        |
   |    |h2
h1 |    |
   .--a-.

>>> let f = right_trapezoid_area 1
>>> [f 0 0, f 0 1, f 1 1, f 1 2, f 2 2]
[0.0,0.5,1.0,1.5,2.0]
-}
right_trapezoid_area :: Fractional a => a -> a -> a -> a
right_trapezoid_area a h1 h2 = 0.5 * a * (h1 + h2)

{- | Perimeter of right trapezoid.

>>> let f = right_trapezoid_perimeter 1
>>> [f 0 1, f 1 1, f 1 2]
[3.414213562373095,4.0,5.414213562373095]
-}
right_trapezoid_perimeter :: Floating a => a -> a -> a -> a
right_trapezoid_perimeter a h1 h2 = let sq x = x * x in a + h1 + h2 + sqrt (sq a + sq (h2 - h1))

-- * Annulus

{- | An annulus is the region between two concentric circles.
R (r2) is the radius of the outer circle, r (r1) is the radius of the inner circle.
The area of an annulus is the difference in the areas of the larger circle and the smaller one.

>>> annulus_area 13 5
452.3893421169302

>>> annulus_area 20 16
452.3893421169302
-}
annulus_area :: Floating a => a -> a -> a
annulus_area r2 r1 = pi * ((r2 * r2) - (r1 * r1))

{- | An annulus sector is a segment of an annulus, given by an angle (theta) in radians.

>>> annulus_sector_area (pi / 2) 20 16
113.09733552923255
-}
annulus_sector_area :: Floating a => a -> a -> a -> a
annulus_sector_area theta r2 r1 = (theta / 2) * ((r2 * r2) - (r1 * r1))
