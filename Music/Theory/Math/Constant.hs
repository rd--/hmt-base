-- | IEE754 constants (c.f. Numeric.MathFunctions.Constants)
module Music.Theory.Math.Constant where

-- | The smallest 'Double' n such that 1 + n /= 1.
epsilonValue :: Double
epsilonValue =
  let (signif, expo) = decodeFloat (1.0 :: Double)
  in encodeFloat (signif + 1) expo - 1.0

-- | Largest representable finite value.
largestFiniteValue :: Double
largestFiniteValue = 1.7976931348623157e308

-- | The smallest representable positive normalized value.
smallestNormalizedValue :: Double
smallestNormalizedValue = 2.2250738585072014e-308
