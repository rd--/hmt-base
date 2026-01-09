{- | Natural Color System

<https://en.wikipedia.org/wiki/Natural_Color_System>
-}
module Music.Theory.Colour.Ncs where

import qualified Data.List.Split {- split -}

import qualified Music.Theory.Colour as Colour {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}

ncs_table :: [(String, Colour.Rgb I)]
ncs_table =
  [ ("White", (255, 255, 255))
  , ("Black", (0, 0, 0))
  , ("Green", (0, 159, 107)) -- 0x00A368
  , ("Red", (196, 2, 51))
  , ("Yellow", (255, 211, 0))
  , ("Blue", (0, 135, 189)) -- 0x0088BF
  ]

ncs_table_rgb :: String -> Colour.Rgb R
ncs_table_rgb nm = Colour.rgb8_to_rgb (List.lookup_err nm ncs_table)

ncs_red, ncs_green, ncs_yellow, ncs_blue :: Colour.Rgb R
ncs_red = ncs_table_rgb "Red"
ncs_green = ncs_table_rgb "Green"
ncs_yellow = ncs_table_rgb "Yellow"
ncs_blue = ncs_table_rgb "Blue"

-- | R-Y-G-B
hue_map :: Num n => [(Char, n)]
hue_map = [('R', 0), ('Y', 60), ('G', 120), ('B', 240)]

hue_lookup :: Num n => Char -> n
hue_lookup k = List.lookup_err k hue_map

-- | Int
type I = Int

-- | Real
type R = Double

-- | Hue
type Hue = (Char, I, Char)

-- | Ncs (s=blackness, c=chromaticness, theta=hue)
type Ncs = (I, I, Hue)

-- | Blackness (0 -- 100)
ncs_s :: Ncs -> I
ncs_s (s,_,_) = s

-- | Chromaticness (0 -- 100)
ncs_c :: Ncs -> I
ncs_c (_,c,_) = c

-- | Hue
ncs_phi :: Ncs -> Hue
ncs_phi (_,_,phi) = phi

-- | Whiteness (0 -- 100)
ncs_w :: Ncs -> I
ncs_w (s,c,_) = 100 - c - s

{- | Saturation (0 -- 1)

>>> ncs_m (ncs_parse "2030-Y90R")
0.375
-}
ncs_m :: Ncs -> R
ncs_m (s,c,_) = i_to_r c / (100 - i_to_r s)

-- | Lightness (0 -- 1)
ncs_v :: Ncs -> R
ncs_v (s,c,_) =
  if c == 0
  then (100 - i_to_r s) / 100
  else undefined

i_to_r :: I -> R
i_to_r = fromIntegral

{- | Parse Ncs string

>>> ncs_to_rgb8 (ncs_parse "S0580-Y10R")

>>> ncs_parse "S2030-Y90R"
(20,30,('Y',90,'R'))

>>> ncs_parse "S1810-Y58G"
(18,10,('Y',58,'G'))

>>> ncs_parse "S2060-G40B"
(20,60,('G',40,'B'))
-}
ncs_parse :: String -> Ncs
ncs_parse text =
  case text of
    'S' : text' -> ncs_parse text'
    _ -> case Data.List.Split.splitPlaces [2::Int,2,1,1,2,1] text of
           [s,c,"-",[a],h,[b]] -> (read s, read c, (a, read h, b))
           _ -> error "ncs_parse"

-- | Ncs to Hsv
ncs_to_hsv :: Ncs -> Colour.Hsv I
ncs_to_hsv = undefined

-- | Ncs to Rgb8
ncs_to_rgb :: Ncs -> Colour.Rgb R
ncs_to_rgb ncs =
  let (h, s, v) = ncs_to_hsv ncs
  in Colour.hsv_to_rgb (i_to_r h / 360, i_to_r s / 99, i_to_r v / 99)

-- | Ncs to Rgb8
ncs_to_rgb8 :: Ncs -> Colour.Rgb I
ncs_to_rgb8 = Colour.rgb_to_rgb8 . ncs_to_rgb
