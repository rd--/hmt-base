-- | <http://scaledinnovation.com/analytics/splines/aboutSplines.html>
module Music.Theory.Geometry.Bezier.Spencer where

import Data.List {- base -}

import Music.Theory.Geometry.Bezier {- hmt-base -}
import Music.Theory.Geometry.Vector {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}

{- | Given data points (p1,p2,p3) calculate control points (c1,c2).

(p1,p2,p3) -> (c1,c2)
(p2,p3,p4) -> (c3,c4)
(p3,p4,p5) -> (c5,c6)

p1 c1 p2 c2 c3 p3 c4 c5 p4 ...
-}
spencer_control_points :: Floating t => t -> V3 (V2 t) -> V2 (V2 t)
spencer_control_points t ((x0, y0), (x1, y1), (x2, y2)) =
  let sq x = x * x
      d01 = sqrt (sq (x1 - x0) + sq (y1 - y0))
      d12 = sqrt (sq (x2 - x1) + sq (y2 - y1))
      fa = t * d01 / (d01 + d12)
      fb = t * d12 / (d01 + d12)
      p1x = x1 - fa * (x2 - x0)
      p1y = y1 - fa * (y2 - y0)
      p2x = x1 + fb * (x2 - x0)
      p2y = y1 + fb * (y2 - y0)
  in ((p1x, p1y), (p2x, p2y))

{- | Generate control points, two for each three-tuple.

[p1,p2,p3,p4] -> [c1,c2,c3,c4]
[p1,p2,p3,p4,p5] -> [c1,c2,c3,c4,c5,c6]
...
-}
spencer_control_points_seq :: Floating t => t -> [V2 t] -> [V2 t]
spencer_control_points_seq t =
  let adj3 (p : q : r : s) = (p, q, r) : adj3 (q : r : s)
      adj3 _ = []
      t2_to_list (i, j) = [i, j]
  in concatMap (t2_to_list . spencer_control_points t) . adj3

{- | For lines (ie. open curves) the first and last arcs are simple quadratics.

>>> spencer_open_group "abcdef"
[Left 'a',Right ('b','c'),Right ('d','e'),Left 'f']
-}
spencer_open_group :: [a] -> [Either a (a, a)]
spencer_open_group l =
  concat
    [ [Left (List.head_err l)]
    , map Right (List.adj2 2 (List.tail_err l))
    , [Left (List.last_err l)]
    ]

{- | Transform input sequence and left-rotate control-point sequence.

>>> spencer_control_points_closed 0.5 [(50,200),(150,200),(150,300),(50,300)]
[(75.0,175.0),(125.0,175.0),(175.0,225.0),(175.0,275.0),(125.0,325.0),(75.0,325.0),(25.0,275.0),(25.0,225.0)]
-}
spencer_control_points_closed :: Floating t => t -> [V2 t] -> [V2 t]
spencer_control_points_closed t l =
  let err = error "spencer_control_points_closed"
      sq =
        let (l1, lK) = List.firstLast l
        in concat [[lK], l, [l1]]
      o = spencer_control_points_seq t sq
      rotate_left x =
        case uncons x of
          Nothing -> err
          Just (x1, xN) -> xN ++ [x1]
  in rotate_left o

{- | For loops (ie. closed curves) all arcs are cubic.

>>> spencer_closed_group "fabcdefa"
[Right ('f','a'),Right ('b','c'),Right ('d','e'),Right ('f','a')]
-}
spencer_closed_group :: [t] -> [Either a (t, t)]
spencer_closed_group = map Right . List.adj2 2

-- | ((p1,p2),Either c1 (c1,c2))
type Spencer_Data t = (V2 (V2 t), Either (V2 t) (V2 (V2 t)))

-- | Open curve data.
spencer_open_dat :: Floating t => t -> [V2 t] -> [Spencer_Data t]
spencer_open_dat t p = zip (List.adj2 1 p) (spencer_open_group (spencer_control_points_seq t p))

-- | Closed curve data.
spencer_closed_dat :: Floating t => t -> [V2 t] -> [Spencer_Data t]
spencer_closed_dat t p =
  let close x = x ++ [List.head_err x]
  in zip (List.adj2 1 (close p)) (spencer_closed_group (spencer_control_points_closed t p))

-- | If /c/ then 'spencer_closed_dat' else 'spencer_open_dat'
spencer_dat :: Floating t => Bool -> t -> [V2 t] -> [Spencer_Data t]
spencer_dat c = if c then spencer_closed_dat else spencer_open_dat

-- | Lift Spencer_Data to cubic Bezier format.
spencer_dat_to_bezier4 :: Fractional t => Spencer_Data t -> V4 (V2 t)
spencer_dat_to_bezier4 d =
  case d of
    ((p1, p2), Left c1) -> bezier_quadratic_to_cubic (p1, c1, p2)
    ((p1, p2), Right (c1, c2)) -> (p1, c1, c2, p2)

-- | /c/ = closed, /t/ = tension
spencer_bezier4 :: Floating t => Bool -> t -> [V2 t] -> [V4 (V2 t)]
spencer_bezier4 c t = map spencer_dat_to_bezier4 . spencer_dat c t
