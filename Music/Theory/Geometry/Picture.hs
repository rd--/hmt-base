-- | Simple pictures.
module Music.Theory.Geometry.Picture where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.Colour as Colour {- hmt-base -}
{- hmt-base -}
{- hmt-base -}
import Music.Theory.Geometry.Vector {- hmt-base -}
import qualified Music.Theory.List as List
import qualified Music.Theory.Math as Math

-- * Type

type Line_Width r = r

-- | (dash,no-dash)
type Dash r = ([r], r)

type Colour = Colour.Rgba Double

no_dash :: Num r => Dash r
no_dash = ([], 0)

-- | (line-width,colour,dash-pattern)
data Pen r = Pen (Line_Width r) (Colour) (Dash r) deriving (Eq, Show)

-- | (centre,radius)
type Centre_Radius t = (V2 t, t)

data Mark r
  = Line (Pen r) (V2 (V2 r))
  | Polygon (Either (Pen r) (Colour)) [V2 r]
  | Circle (Either (Pen r) (Colour)) (Centre_Radius r)
  | Dot (Colour) (Centre_Radius r)
  deriving (Eq, Show)

type Picture r = [Mark r]

-- * Constructors

line_seq :: Num r => Pen r -> [V2 r] -> [Mark r]
line_seq pen =
  let adj l = zip l (List.tail_err l)
  in map (Line pen) . adj

polygon_l :: Pen r -> [V2 r] -> Mark r
polygon_l pen = Polygon (Left pen)

polygon_f :: Colour -> [V2 r] -> Mark r
polygon_f clr = Polygon (Right clr)

circle_l :: Pen r -> Centre_Radius r -> Mark r
circle_l pen = Circle (Left pen)

circle_f :: Colour -> Centre_Radius r -> Mark r
circle_f clr = Circle (Right clr)

-- * Analysis

circle_bounds :: Num t => V2 t -> t -> V2 (V2 t)
circle_bounds (x, y) n = ((x - n, y - n), (n * 2, n * 2))

mark_wn :: (Num n, Ord n) => Mark n -> V2 (V2 n)
mark_wn m =
  case m of
    Line _ (p1, p2) -> v2_bounds [p1, p2]
    Polygon _ p -> v2_bounds p
    Circle _ (c, r) -> circle_bounds c r
    Dot _ (c, r) -> circle_bounds c r

mark_normal :: Ord r => Mark r -> Mark r
mark_normal m =
  case m of
    Line p (p1, p2) -> Line p (if p1 <= p2 then (p1, p2) else (p2, p1))
    Polygon _ _ -> m -- should ensure CCW
    Circle _ _ -> m
    Dot _ _ -> m

mark_pt_set :: Mark r -> [V2 r]
mark_pt_set m =
  case m of
    Line _ (p, q) -> [p, q]
    Polygon _ p -> p
    Circle _ (p, _) -> [p]
    Dot _ (p, _) -> [p]

mark_ln :: Mark r -> Maybe (V2 (V2 r))
mark_ln m =
  case m of
    Line _ l -> Just l
    _ -> Nothing

mark_circle :: Mark r -> Maybe (Centre_Radius r)
mark_circle m =
  case m of
    Circle _ c -> Just c
    _ -> Nothing

picture_pt_set :: Picture r -> [V2 r]
picture_pt_set = concatMap mark_pt_set

picture_ln_set :: Picture r -> [V2 (V2 r)]
picture_ln_set = mapMaybe mark_ln

picture_normalise :: Ord r => Picture r -> Picture r
picture_normalise = nub . map mark_normal

picture_wn :: (Ord r, Num r) => Picture r -> V2 (V2 r)
picture_wn = foldl1 v2_bounds_join . map mark_wn

-- * Graph

{- | Extract coloured vertex-sequences from a picture.
     Dots and circles generate 1-element sequences, lines 2-element sequences, n-polygons generate n+1-element sequences.
-}
picture_ln :: Picture t -> [(Colour, [V2 t])]
picture_ln mk =
  let get_c x = case x of
        Left (Pen _ c _) -> c
        Right c -> c
      f m = case m of
        Line (Pen _ c _) (p1, p2) -> (c, [p1, p2])
        Polygon c p -> (get_c c, p ++ [List.head_err p])
        Circle c (p1, _) -> (get_c c, [p1])
        Dot c (p1, _) -> (c, [p1])
  in map f mk

{- | Extract graph from set of coloured vertex sequences, ie. 'picture_ln'.
     Vertices are compared using the given equality function (ie. '~=')
     Deletes duplicate vertices and edges, which are un-directed.
-}
picture_ln_gr :: (Ord n, Ord c) => (n -> n -> Bool) -> [(c, [n])] -> ([(Int, n)], [(V2 Int, c)])
picture_ln_gr eq_f ln =
  let v = nubBy eq_f (sort (concatMap snd ln))
      v_ix x = fromMaybe (error "picture_ln_gr?") (findIndex (eq_f x) v)
      o (i, j) = (min i j, max i j)
      adj2 x = zip x (List.tail_err x)
      mk_e (c, p) = map (\(i, j) -> (o (v_ix i, v_ix j), c)) (adj2 p)
      e = nub (sort (concatMap mk_e ln))
  in (zip [0 ..] v, e)

-- | 'picture_ln_gr' of 'picture_ln' of '~='
picture_gr :: (Floating n, Ord n) => Picture n -> ([(Int, V2 n)], [(V2 Int, Colour)])
picture_gr =
  let eq (i, j) (p, q) = i Math.~= p && j Math.~= q
  in picture_ln_gr eq . picture_ln
