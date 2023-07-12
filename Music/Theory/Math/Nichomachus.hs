{- | Nichomachus of Gerasa (Νικόμαχος) c.60-c.120

<https://pdfs.semanticscholar.org/5dac/8842ad857c822ab854ede3decadfe0464f15.pdf>
-}
module Music.Theory.Math.Nichomachus where

{- | a-b = b-c ; b = a+c / 2

>>> arithmetic_mean 2 6
4.0

>>> arithmetic_mean 1 2 == (1+2)/2 -- 3/2
True
-}
arithmetic_mean :: Fractional a => a -> a -> a
arithmetic_mean a c = (a + c) / 2

{- | a/b = b/c ; b = sqrt ac

>>> geometric_mean 1 4
2.0

>>> geometric_mean 1 2 == sqrt (1*2) -- sqrt 2
True
-}
geometric_mean :: Floating a => a -> a -> a
geometric_mean a c = sqrt (a * c)

{- | a-b / a = b-c / c ; 2ac / a+c

>>> harmonic_mean 2 6
3.0

>>> harmonic_mean 1 2 == (2*1*2)/(1+2) -- 4/3
True
-}
harmonic_mean :: Fractional a => a -> a -> a
harmonic_mean a c = (2 * a * c) / (a + c) -- OR -- 2 / (1/a + 1/c)

{- | a-b / c = b-c / a ; a-b / b-c = c/a ; aa+cc / a+c

>>> cont_harmonic_mean 3 6
5.0

>>> cont_harmonic_mean 1 2 == (1*1+2*2)/(1+2) -- 5/3
True
-}
cont_harmonic_mean :: Fractional a => a -> a -> a
cont_harmonic_mean a c = (a * a + c * c) / (a + c)

{- | a-b / c = b-c / b ; a-b / b-c = c/b ; c - a + (sqrt (5aa - 2ac + cc)) / 2

>>> cont_geometric_mean 2 5
4.0

Golden Ratio = 1.6180

>>> cont_geometric_mean 1 2 == (2-1+sqrt(5*1*1-2*1*2+2*2))/2 -- (1+sqrt 5)/2
True

> import Music.Theory.Math
> cont_geometric_mean 1 2 ~= 1.618034
-}
cont_geometric_mean :: Floating a => a -> a -> a
cont_geometric_mean a c = (c - a + sqrt (5 * a * a - 2 * a * c + c * c)) / 2

{- | a-b / c = b-c / b ; a-b / b-c = c/b ; a - c + (sqrt (aa - 2ac + 5cc)) / 2

>>> subcont_geometric_mean 1 6
4.0

>>> subcont_geometric_mean 1 2 == (-1 + sqrt 17) / 2 -- 1.5616
True
-}
subcont_geometric_mean :: Floating a => a -> a -> a
subcont_geometric_mean a c = (a - c + sqrt (a * a - 2 * a * c + 5 * c * c)) / 2
