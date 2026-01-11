module Music.Theory.Time where

-- * Time

{- | Milliseconds to seconds

>>> let ms = [1,10,50,100,1000]
>>> let s = [0.001,0.01,0.05,0.1,1]
>>> map ms_to_sec ms == s
true
-}
ms_to_sec :: Fractional n => n -> n
ms_to_sec = (/ 1000)

-- | Seconds to milliseconds
sec_to_ms :: Fractional n => n -> n
sec_to_ms = (* 1000)
