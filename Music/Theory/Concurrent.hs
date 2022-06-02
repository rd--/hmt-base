module Music.Theory.Concurrent where

import Control.Concurrent {- base -}

-- | Pause current thread for the indicated duration (in seconds), see 'pauseThreadLimit'.
threadDelaySeconds :: RealFrac n => n -> IO ()
threadDelaySeconds = threadDelay . floor . (*) 1e6

{- | The number of seconds that 'threadDelaySeconds' can wait for.
Values larger than this require a different thread delay mechanism, see 'threadSleepForSeconds'.
The value is the number of microseconds in @maxBound::Int@.
For 64-bit architectures this is not likely to be an issue, however for 32-bit it can be.

> round ((2 ** 31) / (60 * 60) / 1e6) == 1 -- hours
> round ((2 ** 63) / (60 * 60 * 24 * 365 * 100) / 1e6) == 2925 -- years
-}
threadDelaySecondsLimit :: Fractional n => n
threadDelaySecondsLimit = fromIntegral ((maxBound::Int) - 1) / 1e6

-- | Sleep current thread for the indicated duration (in seconds).
--   Divides long sleeps into parts smaller than 'threadSleepForSeconds'.
threadSleepForSeconds :: RealFrac n => n -> IO ()
threadSleepForSeconds n =
    if n < threadDelaySecondsLimit
    then threadDelaySeconds n
    else threadDelaySeconds (threadDelaySecondsLimit :: Double) >> threadSleepForSeconds (n - threadDelaySecondsLimit)
