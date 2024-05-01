-- | Picture Svg.
module Music.Theory.Geometry.Picture.Svg where

import Music.Theory.Colour {- hmt-base -}
import Music.Theory.Geometry.Matrix {- hmt-base -}
import Music.Theory.Geometry.Picture {- hmt-base -}
import Music.Theory.Geometry.Vector {- hmt-base -}
import Music.Theory.Math {- hmt-base -}

-- | Svg attribute
type SvgAttr = (String, String)

-- | Colour to hex string.
clr_to_hex_string :: Rgba R -> String
clr_to_hex_string = rgb8_to_hex_str . (rgb_to_rgb8 :: Rgb R -> Rgb Int) . rgba_to_rgb

-- | Colour alpha channel.
clr_to_opacity :: Rgba R -> Double
clr_to_opacity (_, _, _, a) = a

-- | Fill, and set stroke to none.
fill :: Rgba R -> [SvgAttr]
fill clr =
  [ ("fill", clr_to_hex_string clr)
  , ("fill-opacity", show (clr_to_opacity clr))
  , ("stroke", "none")
  ]

-- | Stroke, and set fill to transparent.
stroke :: Pen R -> [SvgAttr]
stroke (Pen lw clr dash) =
  concat
    [
      [ ("stroke", clr_to_hex_string clr)
      , ("stroke-opacity", show (clr_to_opacity clr))
      , ("stroke-width", show lw)
      , ("fill-opacity", "0")
      ]
    , if dash == no_dash
        then []
        else
          [ ("stroke-dasharray", unwords (map show (fst dash)))
          , ("stroke-dashoffset", show (snd dash))
          ]
    ]

-- | Mark
mark :: Either (Pen R) (Rgba R) -> [SvgAttr]
mark = either stroke fill

-- | Svg node
type SvgNode = String

-- | Svg element
data SvgElem = SvgElem SvgNode [SvgAttr] [SvgElem]

-- | Line
line :: Show t => V2 (V2 t) -> [SvgAttr]
line ((x1, y1), (x2, y2)) = zipWith (,) ["x1", "y1", "x2", "y2"] (map show [x1, y1, x2, y2])

-- | Polygon
polygon :: Show t => [V2 t] -> [SvgAttr]
polygon pt =
  let f (x, y) = show x ++ "," ++ show y
  in [("points", unwords (map f pt))]

-- | Circle
circle :: Show t => Centre_Radius t -> [SvgAttr]
circle ((x, y), r) = [("cx", show x), ("cy", show y), ("r", show r)]

-- | Render Mark to Svg element
mark_render :: Mark R -> SvgElem
mark_render m =
  case m of
    Line pen ln -> SvgElem "line" (line ln ++ stroke pen) []
    Polygon e pt -> SvgElem "polygon" (polygon pt ++ mark e) []
    Circle e cr -> SvgElem "circle" (circle cr ++ mark e) []
    Dot clr cr -> SvgElem "circle" (circle cr ++ fill clr) []

-- | Translate
translate :: Show t => V2 t -> String
translate (x, y) = concat ["translate(", unwords (map show [x, y]), ")"]

-- | Matrix
matrix :: Show t => (M22 t, V2 t) -> String
matrix (((a, b), (c, d)), (e, f)) = concat ["matrix(", unwords (map show [a, b, c, d, e, f]), ")"]

-- | Group with transform.
transform_group :: Show t => (M22 t, V2 t) -> [SvgElem] -> SvgElem
transform_group m e = SvgElem "g" [("transform", matrix m)] e

-- | m = margin
picture_render :: R -> V2 (V2 R) -> Picture R -> SvgElem
picture_render m w p =
  let ((x0, y0), (_x1, y1)) = w
      dy = y1 - y0
      e = (m / 2) - x0
      f = dy + (m / 2) + y0
  in transform_group (((1, 0), (0, -1)), (e, f)) (map mark_render p)

picture_to_svg_elem :: R -> Picture R -> SvgElem
picture_to_svg_elem m p =
  let w = picture_wn p
      ((x0, y0), (x1, y1)) = w
      vb = ("viewbox", unwords (map show [x0 - m, y0 - m, (m * 2) + (x1 - x0), (m * 2) + (y1 - y0)]))
  in SvgElem "svg" [("xmlns", "http://www.w3.org/2000/svg"), vb] [picture_render m w p]

svg_attr_pp :: SvgAttr -> String
svg_attr_pp (name, value) = concat [name, "=\"", value, "\""]

{- | Svg element pretty printer

> let pen = Pen 0.25 (0,0,0,1) no_dash
> let line = Line pen ((0, 0), (100, 100))
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
picture_to_svg :: R -> FilePath -> Picture R -> IO ()
picture_to_svg m fn p = writeFile fn (svg_elem_pp (picture_to_svg_elem m p))
