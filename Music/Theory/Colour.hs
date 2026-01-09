{- | Colour related functions

Rgb is a real-valued (0-1) normalised (red,green,blue) triple.

Rgb8 is an integer valued (0-255) normalised (red,green,blue) triple.

Rgb24 is an a 24-bit packed form of an Rgb8 triple.

Hsl is a real-valued (0-1) normalised (hue,saturation,lightness) triple.

Hsv is a real-valued (0-1) normalised (hue,saturation,value) triple.

C is an opaque colour, Ca is a C with an α-channel.
-}
module Music.Theory.Colour where

import Data.Bits {- base -}
import qualified Numeric {- base -}
import Text.Printf {- base -}

import qualified Data.Colour as Colour {- colour -}
import qualified Data.Colour.RGBSpace.HSL as Hsl {- colour -}
import qualified Data.Colour.RGBSpace.HSV as Hsv {- colour -}
import qualified Data.Colour.SRGB as Srgb {- colour -}
import qualified Data.Colour.SRGB.Linear as Srgb.Linear {- colour -}

import qualified Music.Theory.Array.Csv as Csv {- hmt-base -}
import qualified Music.Theory.Geometry.Vector as Vector {- hmt-base -}

-- * Tuples/Vectors

type Rgb t = Vector.V3 t
type Rgba t = Vector.V4 t

{- | Unpack 24-bit Rgb value to 8-bit Rgb triple.

>>> rgb24_unpack 0xC80815 == (0xC8,0x08,0x15)
True
-}
rgb24_unpack :: (Bits t, Num t) => t -> Rgb t
rgb24_unpack n = (shiftR (n .&. 0xFF0000) 16, shiftR (n .&. 0x00FF00) 8, n .&. 0x0000FF)

{- | Pack 8-bit Rgb triple into 24-bit Rgb value.

>>> rgb24_pack (0xC8,0x08,0x15) == 0xC80815
True
-}
rgb24_pack :: Bits t => Rgb t -> t
rgb24_pack (r, g, b) = shiftL r 16 .|. shiftL g 8 .|. b

{- | 8-bit Rgb triple to hex string.

>>> rgb8_to_hex_str (rgb24_unpack 0xC80815)
"#c80815"
-}
rgb8_to_hex_str :: (Integral t, PrintfArg t) => Rgb t -> String
rgb8_to_hex_str (r, g, b) = printf "#%02x%02x%02x" r g b

{- | Rgb (0-255) to Rgb (0-1)

>>> rgb8_to_rgb (0, 128, 255) == (0, 128/255, 1)
True
-}
rgb8_to_rgb :: (Real r, Fractional t) => Rgb r -> Rgb t
rgb8_to_rgb = clr_normalise 255

{- | Rgb (0-1) to Rgb (0-255)

>>> rgb_to_rgb8 (0, 0.5, 1)
(0,128,255)
-}
rgb_to_rgb8 :: (Integral i, RealFrac r) => Rgb r -> Rgb i
rgb_to_rgb8 (r, g, b) = (round (r * 255), round (g * 255), round (b * 255))

rgba_to_rgb :: Rgba t -> Rgb t
rgba_to_rgb (r, g, b, _) = (r, g, b)

rgb_to_rgba :: Rgb t -> t -> Rgba t
rgb_to_rgba (r, g, b) a = (r, g, b, a)

-- * Parse

{- | Parse '#rgb' and '#rrggbb' strings.

>>> hex_colour_parse "#c02" == (0xcc, 0x00, 0x22)
True

>>> hex_colour_parse "#c80815" == (0xc8, 0x08, 0x15)
True
-}
hex_colour_parse :: (Eq n, Num n) => String -> Rgb n
hex_colour_parse s =
  let p x =
        case Numeric.readHex x of
          [(r, "")] -> r
          _ -> error "hex_colour_parse: cannot parse element"
  in case s of
      ['#', r, g, b] -> (p [r, r], p [g, g], p [b, b])
      ['#', r1, r2, g1, g2, b1, b2] -> (p [r1, r2], p [g1, g2], p [b1, b2])
      _ -> error "hex_colour_parse: not hex triple?"

-- * Generalised

{- | Normalise colour by dividing each component by /m/.

>>> clr_normalise 255 (hex_colour_parse "#ff0066")
(1.0,0.0,0.4)
-}
clr_normalise :: (Real r, Fractional f) => f -> (r, r, r) -> (f, f, f)
clr_normalise m (r, g, b) = let f x = realToFrac x / m in (f r, f g, f b)

-- * Csv

{- | Read (name,red,green,blue) CSV table.

>>> let fn = "/home/rohan/sw/hsc3-data/data/colour/svg.csv"
>>> tbl <- clr_read_csv_rgb24_table fn
>>> lookup "powderblue" tbl
Just (176,224,230)
-}
clr_read_csv_rgb24_table :: FilePath -> IO [(String, Rgb Int)]
clr_read_csv_rgb24_table fn = do
  tbl <- Csv.csv_table_read_def id fn
  let f e =
        case e of
          [nm, r, g, b] -> (nm, (read r, read g, read b))
          _ -> error "clr_read_csv_rgb24_table"
  return (map f tbl)

-- * Colour

-- | Opaque colour.
type C = Colour.Colour Double

-- | Colour with /alpha/ channel.
type Ca = Colour.AlphaColour Double

-- | Grey 'Colour'.
mk_grey :: (Ord a, Floating a) => a -> Colour.Colour a
mk_grey x = Srgb.sRGB x x x

-- | Reduce 'Colour' to grey.  Constants are @0.3@, @0.59@ and @0.11@.
to_greyscale :: (Ord a, Floating a) => Colour.Colour a -> a
to_greyscale c =
  let (Srgb.RGB r g b) = Srgb.toSRGB c
  in r * 0.3 + g * 0.59 + b * 0.11

-- | 'mk_grey' '.' 'to_greyscale'.
to_greyscale_c :: (Ord a, Floating a) => Colour.Colour a -> Colour.Colour a
to_greyscale_c = mk_grey . to_greyscale

-- | Discard /alpha/ channel, if possible.
pureColour :: (Ord a, Fractional a) => Colour.AlphaColour a -> Colour.Colour a
pureColour c =
  let a = Colour.alphaChannel c
  in if a > 0
      then Colour.darken (recip a) (c `Colour.over` Colour.black)
      else error "hcg-: transparent has no pure colour"

-- | Unpack 'Srgb.RGB' to RGB triple.
srgb_components :: Srgb.RGB t -> Rgb t
srgb_components c = (Srgb.channelRed c, Srgb.channelGreen c, Srgb.channelBlue c)

-- | 'C' to /(red,green,blue)/ tuple.
c_to_rgb :: (Ord t, Floating t) => Colour.Colour t -> Rgb t
c_to_rgb = srgb_components . Srgb.toSRGB

{- | Colour to Rgb

>>> map c_to_rgb8 [mk_grey 0.5,Colour.black]
[(128,128,128),(0,0,0)]
-}
c_to_rgb8 :: (RealFrac t, Floating t, Num r) => Colour.Colour t -> Rgb r
c_to_rgb8 c =
  let c' = Srgb.toSRGB24 c
      i = fromIntegral
  in (i (Srgb.channelRed c'), i (Srgb.channelGreen c'), i (Srgb.channelBlue c'))

{- | Tuple to 'C', inverse of 'c_to_rgb'.

>>> rgb_to_c (1, 0, 0)
Data.Colour.Srgb.Linear.rgb 1.0 0.0 0.0
-}
rgb_to_c :: (Ord t, Floating t) => Rgb t -> Colour.Colour t
rgb_to_c (r, g, b) = Srgb.sRGB r g b

lrgb_to_c :: Fractional t => Rgb t -> Colour.Colour t
lrgb_to_c (r, g, b) = Srgb.Linear.rgb r g b

{- | Rgb8 to Colour

>>> let c = rgb_to_c (200 / 255, 8 / 255, 21 / 255)
>>> rgb8_to_c ((0xC8,0x08,0x15) :: (Int,Int,Int)) == c -- 0xC80815
True
-}
rgb8_to_c :: (Real r, Ord t, Floating t) => Rgb r -> Colour.Colour t
rgb8_to_c = rgb_to_c . Music.Theory.Colour.rgb8_to_rgb

-- | Hue-Saturation-Lightness
type Hsl t = Vector.V3 t

-- | Hue is in [0,1] (not [0, 2pi] or [0, 360])
c_to_hsl :: (Ord t, Floating t) => Colour.Colour t -> Hsl t
c_to_hsl c =
  let (h, s, l) = Hsl.hslView (Srgb.toSRGB c)
  in (h / 360, s, l)

-- | Hue is in [0,1] (not [0, 2pi] or [0, 360])
hsl_to_c :: (RealFrac t, Floating t) => Hsl t -> Colour.Colour t
hsl_to_c (h, s, l) = rgb_to_c (srgb_components (Hsl.hsl (h * 360) s l))

{- | Rgb to Hsl (both in [0,1])

>>> rgb_to_hsl (1,0,0)
(0.0,1.0,0.5)
-}
rgb_to_hsl :: (Ord t, Floating t) => Rgb t -> Hsl t
rgb_to_hsl = c_to_hsl . rgb_to_c

{- | Hsl to Rgb (both in [0,1])

>>> hsl_to_rgb (0,1,0.5)
(1.0,0.0,0.0)
-}
hsl_to_rgb :: (RealFrac t, Floating t) => Hsl t -> Rgb t
hsl_to_rgb = c_to_rgb . hsl_to_c

-- | Hue-Saturation-Value
type Hsv t = Vector.V3 t

-- | Hue is in [0,1] (not [0, 2pi] or [0, 360])
c_to_hsv :: (Ord t, Floating t) => Colour.Colour t -> Hsv t
c_to_hsv c =
  let (h, s, l) = Hsv.hsvView (Srgb.toSRGB c)
  in (h / 360, s, l)

-- | Hue is in [0,1] (not [0, 2pi] or [0, 360])
hsv_to_c :: (RealFrac t, Floating t) => Hsv t -> Colour.Colour t
hsv_to_c (h, s, l) = rgb_to_c (srgb_components (Hsv.hsv (h * 360) s l))

{- | Rgb to Hsv (both in [0,1])

>>> rgb_to_hsv (1,0,0)
(0.0,1.0,1.0)
-}
rgb_to_hsv :: (Ord t, Floating t) => Rgb t -> Hsv t
rgb_to_hsv = c_to_hsv . rgb_to_c

{- | Hsv to Rgb (both in [0,1])

>>> hsv_to_rgb (0,1,1)
(1.0,0.0,0.0)
-}
hsv_to_rgb :: (RealFrac t, Floating t) => Hsv t -> Rgb t
hsv_to_rgb = c_to_rgb . hsv_to_c

{- | Tuple to 'Ca', inverse of 'c_to_rgba'.

>>> rgba_to_ca (1, 1, 0, 0.5)
Data.Colour.Srgb.Linear.rgb 1.0 1.0 0.0 `withOpacity` 0.5
-}
rgba_to_ca :: (Ord t, Floating t) => Rgba t -> Colour.AlphaColour t
rgba_to_ca (r, g, b, a) = rgb_to_c (r, g, b) `Colour.withOpacity` a

{- | Linear space

>>> lrgba_to_ca (0.5, 0, 0, 0.5)
Data.Colour.Srgb.Linear.rgb 0.5 0.0 0.0 `withOpacity` 0.5

>>> let c = lrgba_to_ca (2 / 3, 1 / 3, 0, 3 / 4)
>>> lrgba_to_ca (1, 0, 0, 0.5) `Colour.over` lrgba_to_ca (0, 1, 0, 0.5) == c
True
-}
lrgba_to_ca :: Fractional t => Rgba t -> Colour.AlphaColour t
lrgba_to_ca (r, g, b, a) = lrgb_to_c (r, g, b) `Colour.withOpacity` a

c_to_ca :: Num t => Colour.Colour t -> Colour.AlphaColour t
c_to_ca c = c `Colour.withOpacity` 1

rgb_to_ca :: (Ord t, Floating t) => Rgb t -> Colour.AlphaColour t
rgb_to_ca = c_to_ca . rgb_to_c

-- | 'Ca' to /(red,green,blue,alpha)/ tuple
ca_to_rgba :: (Ord t, Floating t) => Colour.AlphaColour t -> Rgba t
ca_to_rgba x =
  let x' = Srgb.toSRGB (pureColour x)
  in ( Srgb.channelRed x'
     , Srgb.channelGreen x'
     , Srgb.channelBlue x'
     , Colour.alphaChannel x
     )

-- | Is the /alpha/ channel zero.
ca_is_transparent :: (Ord t, Num t) => Colour.AlphaColour t -> Bool
ca_is_transparent x = not (Colour.alphaChannel x > 0)
