-- | Colour functions.
module Music.Theory.Colour where

import Data.Bits {- base -}
import qualified Numeric {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.Geometry.Vector as Vector {- hmt-base -}

-- * Tuples

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
rgb8_to_rgb :: (Real r, Ord t, Floating t) => Rgb r -> Rgb t
rgb8_to_rgb (r, g, b) = (realToFrac r / 255, realToFrac g / 255, realToFrac b / 255)

{- | Rgb (0-1) to Rgb (0-255)

>>> rgb_to_rgb8 (0, 0.5, 1)
(0,128,255)
-}
rgb_to_rgb8 :: (Integral r, RealFrac t) => Rgb t -> Rgb r
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
