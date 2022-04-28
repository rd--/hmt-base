-- | c.f. Statistics.Sample.Histogram (this is much slower but doesn't require any libraries)
module Music.Theory.Math.Histogram where

import Music.Theory.List {- hmt-base -}
import Music.Theory.Math.Constant {- hmt-base -}

{- | Calculate histogram on numBins places.  Returns the range of each bin and the number of elements in each.


> map (snd . bHistogram 10) [[1 .. 10],[1,1,1,2,2,3,10]] == [[1,1,1,1,1,1,1,1,1,1],[3,2,1,0,0,0,0,0,0,1]]
-}
bHistogram :: Int -> [Double] -> ([(Double, Double)], [Int])
bHistogram numBins xs =
  let (lo, hi) = bHistogramRange numBins xs
      d = (hi - lo) / fromIntegral numBins
      step i = lo + d * fromIntegral i
      lhs_seq = map step [0 .. numBins - 1]
      rng_seq = map (\n -> (n, n + d)) lhs_seq
      cnt_seq = map (\rng -> length (filterInRange rng xs)) rng_seq
  in (rng_seq, cnt_seq)

{- | Calculate range.

> bHistogramRange 10 (replicate 10 1) == (0.9, 1.1)
> bHistogramRange 10 (replicate 10 0) == (-1, 1)
> bHistogramRange 10 [1 .. 10] == (0.5, 10.5)
> bHistogramRange 25 [1 .. 10] == (0.8125,10.1875)
-}
bHistogramRange :: Int -> [Double] -> (Double, Double)
bHistogramRange numBins xs =
  let d = if numBins == 1 then 0 else (hi - lo) / ((fromIntegral numBins - 1) * 2)
      (lo, hi) = minmax xs
  in if numBins < 1 || null xs
     then error "bHistogramRange: empty sample"
     else if lo == hi
          then let a = abs lo / 10
               in if a < smallestNormalizedValue then (-1,1) else (lo - a, lo + a)
          else (lo-d, hi+d)
