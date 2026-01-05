-- | https://iamkate.com/data/12-bit-rainbow/
module Music.Theory.Colour.Morley where

import qualified Music.Theory.Colour as Colour {- hmt-base -}

morley_12bit_hex :: [String]
morley_12bit_hex = words "#817 #a35 #c66 #e94 #ed0 #9d5 #4d8 #2cb #0bc #09c #36b #639"

morley_12bit :: Integral i => [(i, i, i)]
morley_12bit = map Colour.hex_colour_parse morley_12bit_hex
