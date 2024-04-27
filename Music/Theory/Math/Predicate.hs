-- | Math predicates
module Music.Theory.Math.Predicate where

import Data.Int {- base -}
import Data.Ratio {- base -}
import Data.Word {- base -}

-- * Double number predicates

{- | Is double value integral.

>>> map double_is_integral [0,1,1.5,2]
[True,True,False,True]
-}
double_is_integral :: Double -> Bool
double_is_integral x =
  let (_, i) = properFraction x :: (Integer, Double)
  in i == 0.0

-- | Is double value integral and in (0,255).
double_is_word8 :: Double -> Bool
double_is_word8 x = double_is_integral x && in_word8 (floor x :: Int64)

-- * Range predicates

in_range :: (Integral r, Integral n) => r -> r -> n -> Bool
in_range l r n =
  let f :: Integral i => i -> Integer
      f = fromIntegral
  in f n >= f l && f n <= f r

{- | Is /a/ in range for Word8.

>>> map in_word8 [-1,0,255,256]
[False,True,True,False]
-}
in_word8 :: Integral a => a -> Bool
in_word8 = in_range (minBound :: Word8) (maxBound :: Word8)

{- | Is /a/ in range for Int8.

>>> map in_int8 [-129,-128,0,127,128]
[False,True,True,True,False]
-}
in_int8 :: Integral a => a -> Bool
in_int8 = in_range (minBound :: Int8) (maxBound :: Int8)

in_int16 :: Integral a => a -> Bool
in_int16 = in_range (minBound :: Int16) (maxBound :: Int16)

in_int32 :: Integral a => a -> Bool
in_int32 = in_range (minBound :: Int32) (maxBound :: Int32)

in_int64 :: Integral a => a -> Bool
in_int64 = in_range (minBound :: Int64) (maxBound :: Int64)

-- * Rational number predicates

-- | Is /n/ integral, ie. is 'denominator' @1@.
ratio_is_integral :: Integral n => Ratio n -> Bool
ratio_is_integral = (== 1) . denominator

-- | Is /n/ integral, and '<=' to /m/.
ratio_is_bounded_integral :: Integral n => n -> Ratio n -> Bool
ratio_is_bounded_integral m n =
  ratio_is_integral n
    && numerator n <= m

-- | Is /n/ integral and in range for 'Int'.
ratio_is_int :: Integral n => Ratio n -> Bool
ratio_is_int = ratio_is_bounded_integral (fromIntegral (maxBound :: Int))

-- | Is /n/ integral and in range for 'Word8'.
ratio_is_word8 :: Integral n => Ratio n -> Bool
ratio_is_word8 = ratio_is_bounded_integral (fromIntegral (maxBound :: Word8))
