{-# LANGUAGE ConstraintKinds,FlexibleContexts #-}
-- | Byte cast functions.
module Music.Theory.Byte.Cast where

import qualified Control.Monad.ST {- base -}
import qualified Data.Word {- base -}

import qualified Data.Array.ST {- array -}
import qualified Data.Array.Unsafe {- array -}

-- | Type of array to cast through.
type Cast_Array s t = Data.Array.ST.MArray (Data.Array.ST.STUArray s) t (Control.Monad.ST.ST s)

-- | Cast using array.
castUsingArray :: (Cast_Array s a, Cast_Array s b) => a -> Control.Monad.ST.ST s b
castUsingArray d =
  flip Data.Array.ST.readArray 0
  =<< Data.Array.Unsafe.castSTUArray
  =<< Data.Array.ST.newArray (0, 0 :: Int) d

{- | Cast Float to Data.Word.Word32

>>> castFloatToWord32 3.141
1078527525
-}
castFloatToWord32 :: Float -> Data.Word.Word32
castFloatToWord32 d =
  Control.Monad.ST.runST
  (castUsingArray d)

{- | Case Data.Word.Word32 to Float

>>> castWord32ToFloat 1078527525
3.141
-}
castWord32ToFloat :: Data.Word.Word32 -> Float
castWord32ToFloat d =
  Control.Monad.ST.runST
  (castUsingArray d)

{- | Cast Double to Data.Word.Word64

>>> castDoubleToWord64 3.141
4614255322014802772
-}
castDoubleToWord64 :: Double -> Data.Word.Word64
castDoubleToWord64 d =
  Control.Monad.ST.runST
  (castUsingArray d)

{- | Case Data.Word.Word64 to Double

>>> castWord64ToDouble 4614255322014802772
3.141
-}
castWord64ToDouble :: Data.Word.Word64 -> Double
castWord64ToDouble d =
  Control.Monad.ST.runST
  (castUsingArray d)
