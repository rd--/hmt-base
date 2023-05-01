module Music.Theory.Image.Svg where

import Text.Printf {- base -}

import Music.Theory.Colour {- hmt-base -}
import Music.Theory.Geometry.Vector {- hmt-base -}

type R = Double

-- | (red,green,blue)
type Clr = (Int,Int,Int)

-- | image-size:(width,height) viewbox:(x,y,width,height)
--
-- > svg_begin_elem (100,100) ((-1,-1),(1,1))
svg_begin_elem :: V2 R -> V2 (V2 R) -> String
svg_begin_elem (w,h) ((vx,vy),(vw,vh)) =
  printf
  "<svg width=\"%f\" height=\"%f\" viewBox=\"%f %f %f %f\" xmlns=\"http://www.w3.org/2000/svg\">"
  w h vx vy vw vh

svg_end_elem :: String
svg_end_elem = "</svg>"

{- | stroke and stroke-width attributes

> stroke_attr ((0,0,0),1)
> stroke_attr ((0,0,255),2)
-}
stroke_attr :: (Clr,R) -> String
stroke_attr (clr_s,w) =
  let w_ = if w == 1 then "" else printf "stroke-width=\"%f\"" w
      clr_ e = if e == (0,0,0) then "black" else rgb8_to_hex_str e
      s_ = printf "stroke=\"%s\"" (clr_ clr_s)
  in unwords [s_,w_]

{- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/line>

k = precision

> line_elem  ((0,0,0),1) ((0,100),(100,0))
-}
line_elem :: Int -> (Clr,R) -> V2 (V2 R) -> String
line_elem k strk ((x1,y1),(x2,y2)) =
  printf
  "<line x1=\"%.*f\" y1=\"%.*f\" x2=\"%.*f\" y2=\"%.*f\" %s fill=\"none\" />"
  k x1 k y1 k x2 k y2 (stroke_attr strk)

-- | (stroke,fill)
type Strk_Fill = (Maybe (Clr,R),Maybe (Clr,R))

-- | fill and fill-opacity attributes
fill_attr :: (Clr,R) -> String
fill_attr (clr,op) =
  let clr_ e = if e == (0,0,0) then "black" else rgb8_to_hex_str e
      alpha = if op == 1 then "" else printf "fill-opacity=\"%f\"" op
  in printf "fill=\"%s\" %s" (clr_ clr) alpha

-- | 'fill_attr' of fill=none
fill_attr_m :: Maybe (Clr,R) -> String
fill_attr_m = maybe "fill=\"none\"" fill_attr

{- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/circle>

> circle_elem (Just ((0,0,0),1),Nothing) ((0,0),10)
> circle_elem (Nothing,Just ((255,0,0),1)) ((0,0),10)
-}
circle_elem :: Strk_Fill -> (V2 R,R) -> String
circle_elem (strk,fill) ((x,y),r) =
  printf
  "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" %s %s />"
  x y r (maybe "" stroke_attr strk) (fill_attr_m fill)

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/polyline>
--
-- > polyline_elem (Just ((0,0,255),1),Nothing) [(0,100),(100,0)]
-- > polyline_elem (Nothing,Just ((0,0,0),1)) [(0,100),(100,0)]
polyline_elem :: Strk_Fill -> [V2 R] -> String
polyline_elem (strk,fill) ln =
  let ln_ = unwords (map (\(x,y) -> printf "%f,%f" x y) ln)
  in printf
     "<polyline points=\"%s\" %s %s />"
     ln_ (maybe "" stroke_attr strk) (fill_attr_m fill)

-- | <https://svgwg.org/specs/paths/#PathDataCubicBezierCommands>
bezier4_elem :: (Clr,R) -> V4 (V2 R) -> String
bezier4_elem strk ((x0,y0),(x1,y1),(x2,y2),(x3,y3)) =
  printf
  "<path d=\"M %f,%f C %f,%f %f,%f %f,%f\" %s fill=\"none\" />"
  x0 y0 x1 y1 x2 y2 x3 y3 (stroke_attr strk)

-- | <https://svgwg.org/specs/paths/#PathDataEllipticalArcCommands>
--
-- > arc_elem ((0,0,0),1) ((300,200),(150,150),0,(1,0),(150,-150))
arc_elem :: (Clr,R) -> (V2 R,V2 R,R,V2 Int,V2 R) -> String
arc_elem strk ((x1,y1),(rx,ry),rot,(f1,f2),(x2,y2)) =
  printf
  "<path d=\"M %f,%f A %f,%f %f %d,%d %f,%f\" %s fill=\"none\" />"
  x1 y1 rx ry rot f1 f2 x2 y2 (stroke_attr strk)

type Svg_Line_Dat = [((Clr,R),V2 (V2 R))]

-- | m=margin-% bnd=((x0,y0),(x1,y1))
svg_viewbox :: R -> V2 (V2 R) -> V2 (V2 R)
svg_viewbox m ((x0,y0),(x1,y1)) =
  let dx = x1 - x0
      dy = y1 - y0
      n = (m / 100) * max dx dy
  in ((x0 - n,y0 - n),(dx + 2 * n,dy + 2 * n))

-- | m=margin-%, k=precision
svg_store_line :: FilePath -> (V2 R,R,Int) -> Svg_Line_Dat -> IO ()
svg_store_line fn (sz,m,k) dat = do
  let (p,ln) = unzip dat
      vw = svg_viewbox m (v2_bounds (concatMap (\(i,j) -> [i,j]) ln))
      txt = svg_begin_elem sz vw : zipWith (line_elem k) p ln
  writeFile fn (unlines (txt ++ [svg_end_elem]))

svg_store_line_unif :: (Clr,R) -> FilePath -> (V2 R,R,Int) -> [V2 (V2 R)] -> IO ()
svg_store_line_unif u fn opt dat = svg_store_line fn opt (zip (repeat u) dat)

type Svg_Polyline_Dat = [(Strk_Fill,[V2 R])]

-- | m=margin-%
svg_store_polyline :: FilePath -> V2 R -> R -> Svg_Polyline_Dat -> IO ()
svg_store_polyline fn sz m dat = do
  let (p,ln) = unzip dat
      vw = svg_viewbox m (v2_bounds (concat ln))
      txt = svg_begin_elem sz vw : zipWith polyline_elem p ln
  writeFile fn (unlines (txt ++ [svg_end_elem]))

svg_store_polyline_unif :: Strk_Fill -> FilePath -> V2 R -> R -> [[V2 R]] -> IO ()
svg_store_polyline_unif u fn sz m dat = svg_store_polyline fn sz m (zip (repeat u) dat)
