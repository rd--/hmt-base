-- | Colour names, non-specific.
module Music.Theory.Colour.Names where

import Data.Maybe {- base -}

import qualified Music.Theory.Colour {- hmt-base -}

import qualified Data.Colour {- colour -}

{- | I24 to C

>>> colour_lookup_err "Venetian red" == i24_to_c 0xC80815
True
-}
i24_to_c :: Int -> Data.Colour.Colour Double
i24_to_c = Music.Theory.Colour.rgb8_to_c . Music.Theory.Colour.rgb24_unpack

colour_tbl :: [(String, Data.Colour.Colour Double)]
colour_tbl =
  let f (_, nm, hex) = (nm, i24_to_c hex)
  in map f colour_names_table

{- | Colour lookup

>>> colour_lookup_err "Cerulean"
Data.Colour.SRGB.Linear.rgb 0.0 0.19806931955994886 0.386429433787049

>>> colour_lookup_err "Fern green"
Data.Colour.SRGB.Linear.rgb 0.0 0.2874408377269175 6.124605423161761e-2

>>> colour_lookup_err "Candlelight yellow"
Data.Colour.SRGB.Linear.rgb 0.9734452903984125 0.6375968739940326 8.023192985384994e-3
-}
colour_lookup_err :: String -> Data.Colour.Colour Double
colour_lookup_err = fromMaybe (error "colour_lookup") . flip lookup colour_tbl

-- > map (\(_,_,n) -> hex_to_rgb24 n) colour_names_table
colour_names_table :: Num n => [(String, String, n)]
colour_names_table =
  [ ("red", "Venetian red", 0xC80815)
  , ("blue", "Swedish azure blue", 0x005B99)
  , ("orange", "Safety orange", 0xFF6600)
  , ("magenta", "Dye magenta", 0xCA1F7B)
  , ("magenta", "Process magenta", 0xFF0090)
  , ("yellow", "Candlelight yellow", 0xFCD116)
  , ("cyan", "Cyan additive secondary", 0x00FFFF)
  , ("cyan", "Cyan subtractive primary", 0x00B7EB)
  , ("green", "Fern green", 0x009246)
  , ("brown", "Sepia brown", 0x704214)
  , ("green", "Verdigris", 0x43B3AE)
  , ("cyan", "Viridian", 0x40826D)
  , ("cyan", "Cerulean", 0x007BA7)
  ]

{- | (year,name,code-a,code-b,hex-rgb)

> map (\(_,_,_,_,n) -> hex_to_rgb24 n) pantone_yr
-}
pantone_yr :: [(Int, String, Int, Int, Int)]
pantone_yr =
  [ (2000, "Cerulean", 15, 4020, 0x9BB7D4)
  , (2001, "Fuchsia Rose", 17, 2031, 0xC74375)
  , (2002, "True Red", 19, 1664, 0xBF1932)
  , (2003, "Aqua Sky", 14, 4811, 0x7BC4C4)
  , (2004, "Tigerlily", 17, 1456, 0xE2583E)
  , (2005, "Blue Turquoise", 15, 5217, 0x53B0AE)
  , (2006, "Sand Dollar", 13, 1106, 0xDECDBE)
  , (2007, "Chili Pepper", 19, 1557, 0x9B1B30)
  , (2008, "Blue Iris", 18, 3943, 0x5A5B9F)
  , (2009, "Mimosa", 14, 0848, 0xF0C05A)
  , (2010, "Turquoise", 15, 5519, 0x45B5AA)
  , (2011, "Honeysuckle", 18, 2120, 0xD94F70)
  , (2012, "Tangerine Tango", 17, 1463, 0xDD4124)
  , (2013, "Emerald", 17, 5641, 0x009473)
  , (2014, "Radiant Orchid", 18, 3224, 0xB163A3)
  , (2015, "Marsala", 18, 1438, 0x955251)
  , (2016, "Rose Quartz", 13, 1520, 0xF7CAC9)
  , (2016, "Serenity", 15, 3913, 0x92A8D1)
  , (2017, "Greenery", 15, 0343, 0x88B04B)
  ]

cerulean_v :: Num n => [n]
cerulean_v =
  [ 0x007BA7
  , 0x0040FF
  , 0x2A52BE -- Cerulean Blue (Maerz and Paul)
  , 0x98B4D4 -- Cerulean (Pantone)
  , 0x1DACD6 -- Cerulean (Crayola)
  , 0x6D9BC3 -- Cerulean Frost (Crayola)
  ]
