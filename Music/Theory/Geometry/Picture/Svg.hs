-- | Picture Svg.
module Music.Theory.Geometry.Picture.Svg where

import Text.Printf {- base -}

import qualified Music.Theory.Colour as Colour {- hmt-base -}
import qualified Music.Theory.Geometry.Functions as Functions {- hmt-base -}
import qualified Music.Theory.Geometry.Matrix as Matrix {- hmt-base -}
import qualified Music.Theory.Geometry.Picture as Picture {- hmt-base -}
import qualified Music.Theory.Geometry.Vector as Vector {- hmt-base -}

-- | Svg attribute (key, value)
type SvgAttr = (String, String)

-- | Colour to hex string.
clr_to_hex_string :: Colour.Rgba Double -> String
clr_to_hex_string =
  Colour.rgb8_to_hex_str
    . (Colour.rgb_to_rgb8 :: Colour.Rgb Double -> Colour.Rgb Int)
    . Colour.rgba_to_rgb

-- | Colour alpha channel.
clr_to_opacity :: Colour.Rgba Double -> Double
clr_to_opacity (_, _, _, a) = a

{- | Fill, and set stroke to none.

Default fill-opacity=1
-}
fill :: Colour.Rgba Double -> [SvgAttr]
fill clr =
  [ ("fill", clr_to_hex_string clr)
  , ("fill-opacity", show (clr_to_opacity clr)) -- elide if default
  , ("stroke", "none")
  ]

{- | Stroke, and set fill to transparent.

lw = line-width, clr = colour

Default values are: stroke-opacity=1 stroke-width=1px fill-opacity=1

Default settings should be elided.
Also attributes that are inherited may be set on the <g> container element.
SVG 2 requires that length attributes (i.e. for stroke-width) have units specified.
Printing precision should be specified.
-}
stroke :: Picture.Pen Double -> [SvgAttr]
stroke (Picture.Pen lw clr dash) =
  concat
    [
      [ ("stroke", clr_to_hex_string clr)
      , ("stroke-opacity", show (clr_to_opacity clr)) -- elide if default
      , ("stroke-width", show lw ++ "px") -- elide if default
      , ("fill-opacity", "0")
      ]
    , if dash == Picture.no_dash
        then []
        else
          [ ("stroke-dasharray", unwords (map show (fst dash)))
          , ("stroke-dashoffset", show (snd dash))
          ]
    ]

-- | Mark
mark :: Either (Picture.Pen Double) (Colour.Rgba Double) -> [SvgAttr]
mark = either stroke fill

-- | Svg node
type SvgNode = String

-- | Svg element
data SvgElem = SvgElem SvgNode [SvgAttr] [SvgElem]

{- | Line

>>> line ((0, 0), (1, 1))
[("x1","0"),("y1","0"),("x2","1"),("y2","1")]
-}
line :: Show t => Vector.V2 (Vector.V2 t) -> [SvgAttr]
line ((x1, y1), (x2, y2)) = zipWith (,) ["x1", "y1", "x2", "y2"] (map show [x1, y1, x2, y2])

{- | Polygon

>>> polygon [(0, 0), (1, 1), (1, 0)]
[("points","0,0 1,1 1,0")]
-}
polygon :: Show t => [Vector.V2 t] -> [SvgAttr]
polygon pt =
  let f (x, y) = show x ++ "," ++ show y
  in [("points", unwords (map f pt))]

{- | Circle

>>> circle ((0, 0), 1)
[("cx","0"),("cy","0"),("r","1")]
-}
circle :: Show t => Picture.Centre_Radius t -> [SvgAttr]
circle ((x, y), r) = [("cx", show x), ("cy", show y), ("r", show r)]

{- | Arc.  theta=central angle.  phi=initial angle.

>>> arc ((0,0),1) (pi / 4) 0
[("d","M 1.0 0.0 A 1.0 1.0 0 0 0 0.7071067811865476 0.7071067811865475")]
-}
arc :: Picture.Centre_Radius Double -> Double -> Double -> [SvgAttr]
arc ((x, y), r) theta phi =
  let (x1, y1) = Vector.v2_add (x, y) (Functions.polar_to_rectangular (r, phi))
      (x2, y2) = Vector.v2_add (x, y) (Functions.polar_to_rectangular (r, theta + phi))
      largeArcFlag = if theta <= pi then 0 else 1 :: Int
  in [("d", printf "M %f %f A %f %f 0 %d 0 %f %f" x1 y1 r r largeArcFlag x2 y2)]

-- | Render Mark to Svg element
mark_render :: Picture.Mark Double -> SvgElem
mark_render m =
  case m of
    Picture.Line pen ln -> SvgElem "line" (line ln ++ stroke pen) []
    Picture.Polygon e pt -> SvgElem "polygon" (polygon pt ++ mark e) []
    Picture.Circle e cr -> SvgElem "circle" (circle cr ++ mark e) []
    Picture.Arc pen cr theta phi -> SvgElem "path" (arc cr theta phi ++ stroke pen) []
    Picture.Dot clr cr -> SvgElem "circle" (circle cr ++ fill clr) []

-- | Translate
translate :: Show t => Vector.V2 t -> String
translate (x, y) = concat ["translate(", unwords (map show [x, y]), ")"]

-- | Matrix
matrix :: Show t => (Matrix.M22 t, Vector.V2 t) -> String
matrix (((a, b), (c, d)), (e, f)) = concat ["matrix(", unwords (map show [a, b, c, d, e, f]), ")"]

-- | Group with transform.
transform_group :: Show t => (Matrix.M22 t, Vector.V2 t) -> [SvgElem] -> SvgElem
transform_group m e = SvgElem "g" [("transform", matrix m)] e

-- | m = margin, add transform group to flip y axis
picture_render :: Double -> Vector.V2 (Vector.V2 Double) -> Picture.Picture Double -> SvgElem
picture_render m wn p =
  let ((x0, y0), (_x1, y1)) = wn
      dy = y1 - y0
      e = (m / 2) - x0
      f = dy + (m / 2) + y0
  in transform_group (((1, 0), (0, -1)), (e, f)) (map mark_render p)

picture_to_svg_elem :: Double -> Picture.Picture Double -> SvgElem
picture_to_svg_elem m p =
  let wn = Picture.picture_wn p
      ((x0, y0), (x1, y1)) = wn
      w = (m * 2) + (x1 - x0)
      h = (m * 2) + (y1 - y0)
      attr =
        [ ("xmlns", "http://www.w3.org/2000/svg")
        , ("viewbox", unwords (map show [x0 - m, y0 - m, w, h]))
        , ("width", show w)
        , ("height", show h)
        ]
  in SvgElem "svg" attr [picture_render m wn p]

svg_attr_pp :: SvgAttr -> String
svg_attr_pp (name, value) = concat [name, "=\"", value, "\""]

{- | Svg element pretty printer

> let pen = Picture.Pen 0.25 (0,0,0,1) Picture.no_dash
> let line = Picture.Line pen ((0, 0), (100, 100))
> let element = picture_to_svg_elem 20 [line]
> putStr $ svg_elem_pp element
-}
svg_elem_pp :: SvgElem -> String
svg_elem_pp (SvgElem name attr children) =
  concat
    [ "<"
    , name
    , " "
    , unwords (map svg_attr_pp attr)
    , if null children then "/>" else ">"
    , unwords (map svg_elem_pp children)
    , if null children then "" else concat ["</", name, ">"]
    ]

-- | m=margin, fn=file-name, p=picture
picture_to_svg :: Double -> FilePath -> Picture.Picture Double -> IO ()
picture_to_svg m fn p = writeFile fn (svg_elem_pp (picture_to_svg_elem m p))
