-- | Functions for map projections.
module Music.Theory.Geometry.Cartography.Projections where

import qualified Music.Theory.Geometry.Functions as Geometry {- hmt-base -}
import qualified Music.Theory.Math as Math {- hmt-base -}

-- | R = Real
type R = Double

-- | P = Point (φ = latitude = [-π/2,π/2], λ = longitude = [-π,π])
type P = (R,R)

-- | Square root of /n/ is /n/ if non-negative, else zero.
asqrt :: R -> R
asqrt n = if n <= 0 then 0 else sqrt n

-- | Pi
π :: R
π = pi

{- | Approximate equality, ε = 1^-10

>>> (pi/2) ==~ 1.5707963267
True
-}
(==~) :: (Fractional t, Ord t) => t -> t -> Bool
(==~) p q =
  let ε = 1.0e-10
  in abs (p - q) < ε

-- | Verify 'P' is in range.
p_verify :: P -> Bool
p_verify (φ,λ) =
    let f (l,r) n = n >= l && n <= r
    in f (-π/2,π/2) φ && f (-π,π) λ

to_oblique :: P -> P -> P
to_oblique (φp,λp) (φ,λ) =
    let φ' = asin (sin φp * sin φ - cos φp * cos φ * cos λ)
        λ' = let α = cos φ * sin λ
                 β = sin φp * cos φ * cos λ + cos φp * sin φ
             in λp + atan2 α β
    in (φ',λ')

f_2_2_5 :: R -> R -> R -> R -> P -> P
f_2_2_5 cx cy a b (φ,λ) =
    let x = cx * λ * (a + sqrt (1 - b * Math.sqr (φ / π)))
        y = cy * φ
    in (x,y)

sinc :: R -> R
sinc x = if x == 0 then 1 else sin x / x

-- | David A. Aitoff, 1889
aitoff :: P -> P
aitoff (φ,λ) =
  let α = acos (cos φ * cos (λ / 2))
      x = (2 * cos φ * sin (λ / 2)) / sinc α
      y = sin φ / sinc α
  in (x,y)

-- | Bernard Sylvanus, 1511; Rigobert Bonne, 1727-1795
bonne :: R -> P -> P
bonne φ1 (φ,λ) =
    let ρ = atan φ1 + φ1 - φ
        e = (λ * cos φ) / ρ
        x = ρ * sin e
        y = atan φ1 - (ρ * cos e)
    in (x,y)

-- | César-François Cassini de Thury, 1745
cassini :: P -> P
cassini (φ,λ) =
    let x = asin (cos φ * sin λ)
        y = atan (tan φ / cos λ)
    in (x,y)

-- | <https://mathworld.wolfram.com/CylindricalProjection.html>
cylindrical :: R -> P -> P
cylindrical λ0 (φ,λ) =
  let x = λ - λ0
      y = tan φ
  in (x,y)

-- | Édouard Collignon, c. 1865
collignon :: P -> P
collignon (φ,λ) =
    let α = sqrt (1 - sin φ)
        x = (2 / sqrt π) * λ * α
        y = sqrt π * (1 - α)
    in (x,y)

-- | John Evelyn Edmund Craster, 1929; Snyder p.200
craster :: P -> P
craster (φ,λ) =
    let x = sqrt (3 / π) * λ * (2 * cos (2 * φ / 3) - 1)
        y = sqrt (3 * π) * sin (φ / 3)
    in (x,y)

-- | Levinus P. Denoyer, 1920/1921
denoyer :: P -> P
denoyer (φ,λ) =
    let λ' = abs λ
        c0 = 95/100
        c1 = -(1/12)
        c3 = 1/600
        d1 = 9/10
        d5 = 3/100
        x = let c' = c0 + λ' * (c1 + λ' * λ' * c3)
                d' = φ * (d1 + d5 * φ * φ * φ * φ)
            in λ * cos (c' * d')
        y = φ
    in (x,y)

eckert_i :: P -> P
eckert_i (φ,λ) =
    let α = 2 * sqrt ((2/3) * π)
        x = α * λ * (1 - (abs φ) / π)
        y = α * φ
    in (x,y)

-- | Max Eckert-Greifendorff, 1906
eckert_ii :: P -> P
eckert_ii (φ,λ) =
    let α = sqrt (4 - 3 * sin (abs φ))
        x = (2 / sqrt (6 * π)) * λ * α
        y = signum φ * sqrt ((2 * π) / 3) * (2 - α)
    in (x,y)

eckert_iii :: P -> P
eckert_iii =
    let k = sqrt (π * (4 + π))
    in f_2_2_5 (2 / k) (4 / k) 1 4

eckert_v :: P -> P
eckert_v (φ,λ) =
    let x = λ * (1 + cos φ) / sqrt (2 + π)
        y = 2 * φ / sqrt (2 + π)
    in (x,y)

-- | Marinus of Tyre, AD.100
equirectangular :: R -> R -> P -> P
equirectangular φ1 λ0 (φ,λ) =
    let x = (λ - λ0) * cos φ1
        y = φ - φ1
    in (x,y)

-- | Lawrence Fahey, 1975
fahey :: P -> P
fahey (φ,λ) =
    let k = cos (35 * (π / 180))
        x = λ * k * sqrt (1 - (tan (φ / 2)) ** 2)
        y = (1 + k) * tan (φ / 2)
    in (x,y)

-- | De Prépetit Foucaut, 1862.
foucaut :: R -> P -> P
foucaut n (φ,λ) =
    let x = λ * cos φ / (n + (1 - n) * cos φ)
        y = n * φ + (1 - n) * sin φ
    in (x,y)

-- | James Gall, 1855, r = radius
gall_stereographic :: R -> P -> P
gall_stereographic r (φ,λ) =
  let x = (r * λ) / sqrt 2
      y = r * (1 + sqrt 2 / 2) * tan (φ / 2)
  in (x,y)

-- | James Gall (Arno Peters), 1855
gall_peters :: R -> P -> P
gall_peters r (φ,λ) =
    let x = r * λ
        y = r * 2 * sin φ
    in (x,y)

-- | G. A. Ginzburg (1944)
ginsburg_viii :: P -> P
ginsburg_viii (φ,λ) =
    let k0 = 0.162388
        k2 = 0.000952426
        x = λ * (1 - k0 * Math.sqr φ) * (0.87 - k2 * (λ ** 4))
        y = φ * (1 + Math.sqr φ / 12)
    in (x,y)

-- | Thales of Miletus <https://mathworld.wolfram.com/GnomonicProjection.html>
gnomonic :: R -> R -> P -> P
gnomonic φ1 λ0 (φ,λ) =
  let c = sin φ1 * sin φ + cos φ1 * cos φ * cos (λ - λ0)
      x = (cos φ * sin λ - λ0) / c
      y = (cos φ1 * sin φ - sin φ1 * cos φ * cos (λ - λ0)) / c
  in (x,y)

-- | Ferdinand Rudolph Hassler, 1825
--   λ0 = longitude of the central meridian
--   φ0 = latitude of origin at λ0
hassler :: R -> R -> P -> P
hassler φ0 λ0 (φ,λ) =
  if φ == 0
  then (λ - λ0,negate φ0)
  else let x = Geometry.cot φ * sin ((λ - λ0) * sin φ)
           y = φ - φ0 + Geometry.cot φ * (1 - cos ((λ - λ0) * sin φ))
       in (x,y)

-- | Alphons J. van der Grinten, 1898
van_der_grinten_i :: P -> P
van_der_grinten_i (φ,λ) =
    let a = 0.5 * abs (π/λ - λ/π)
        θ = asin (abs (2 * φ) / π)
        g = cos θ / (sin θ + cos θ - 1)
        p = g * ((2 / sin θ) - 1)
        q = a' + g
        a' = Math.sqr a
        g' = Math.sqr g
        p' = Math.sqr p
        q' = Math.sqr q
        x = let i = a * (g - p')
                j = sqrt ((a' * Math.sqr (g - p')) - ((p' + a') * (g' - p')))
                n = π * (i + j)
                d = p' + a'
            in signum λ * (n / d)
        y = let i = p * q
                j = a * sqrt ((a' + 1) * (p' + a') - q')
                n = π * abs (i - j)
                d = p' + a'
            in signum φ * (n / d)
    in if φ ==~ 0
       then (λ,0)
       else if λ ==~ 0 || abs φ ==~ (pi/2)
            then (0,signum φ * π * tan (θ / 2))
            else (x,y)

-- | Vladimir V. Kavrayskiy, 1939
kavrayskiy_vii :: P -> P
kavrayskiy_vii (φ,λ) =
    let x = ((3 * λ) / (2 * π)) * asqrt ((Math.sqr π / 3) - Math.sqr φ)
        y = φ
    in (x,y)

-- | Johann Heinrich Lambert, 1772; conformal conic.
--
-- λ0 = reference longitude,
-- φ0 = reference latitude,
-- φ1 and φ2 = standard parallels
lambert_conformal_conic :: R -> R -> R -> R -> P -> P
lambert_conformal_conic φ0 φ1 φ2 λ0 (φ,λ) =
    let atan' p a = (atan a) ** p
        tan' p a = (tan a) ** p
        n = let z = π/4 + φ2/2
            in log (cos φ1 * Geometry.sec φ2) / log (tan z * atan z)
        ρ = f * atan' n (π/4 + φ/4)
        ρ0 = f * atan' n (π/4 + φ0/2)
        f = (cos φ1 * tan' n (π/4 + φ1/2)) / n
        x = ρ * sin (n * (λ - λ0))
        y = ρ0 - ρ * cos (n * (λ - λ0))
    in (x,y)

-- | Johann Heinrich Lambert, 1772
--
-- φ1 = standard parallel
-- λ0 = central longitude
lambert_azimuthal :: R -> R -> P -> P
lambert_azimuthal φ1 λ0 (φ,λ) =
  let k = sqrt (2 / (1 + sin φ1 * sin φ + cos φ1 * cos φ * cos (λ - λ0)))
      x = k * cos φ * sin (λ - λ0)
      y = k * (cos φ1 * sin φ - sin φ1 * cos φ * cos (λ - λ0))
  in (x,y)

-- | Johann Heinrich Lambert, 1772
lambert_cylindrical :: R -> P -> P
lambert_cylindrical λ0 (φ,λ) =
  let x = λ - λ0
      y = sin φ
  in (x,y)

-- | Léo Larrivée, 1988
larrivee :: P -> P
larrivee (φ, λ) =
    let x = λ * (1 + (cos φ) ** 0.5) / 2
        y = φ / (cos (φ/2) * cos (λ/6))
    in (x,y)

-- | Gerardus Mercator, 1569
mercator :: R -> P -> Maybe P
mercator ε (φ, λ) =
    let x = λ
        y = log ((1 + sin φ) / (1 - sin φ)) / 2
        -- y = log (tan (π/4 + φ/2))
        -- y = atanh (sin φ)
    in if abs φ > ε
       then Nothing
       else Just (x,y)

{-
mercator_oblique :: P -> P
mercator_oblique (φ, λ) =
    let ε = 1.03-4
        λ' = let n = cos φ1 * sin φ2 * cosλ1 - sin φ1 * cos φ2 * cosλ2
                 d = sin φ1 * cos φ2 * sinλ2 - cos φ1 * sin φ2 * sinλ1
             in atan (n / d)
        φ' = let n = - (cos (λ' - λ1))
                 d = tan φ1
             in atan (n / d)
        a = sin φ' * sin φ - cos φ' * cos φ * sin λ
        x = let n = tan φ * cos φ' + sin φ' * sin λ
                d = cos λ
            in atan (n / d)
        y = atanh a
    in (x,y)
-}

mercator_transverse :: P -> Maybe P
mercator_transverse (φ, λ) =
    let ε = 1.0e-10
        b = cos φ * sin λ
        x = 0.5 * log ((1 + b) / (1 - b))
        y = cos φ * cos λ / sqrt (1 - b * b)
    in if abs (abs b - 1) <= ε || abs y - 1 > ε
       then Nothing
       else if abs y >= 1
            then Just (x,0)
            else Just (x,signum φ * acos y)

{-
mercator_transverse :: P -> Maybe P
mercator_transverse (φ, λ) =
    let ε = 1.0e-10
        b = cos φ * sin λ
        x = atanh b
        y = atan (tan φ / cos λ) - φ0
        φ = asin (sin d / cosh x)
        λ = λ0 + atan (sinh x / cos D)
    in (x,y)
-}

-- | Osborn Maitland Miller, 1942
miller_cylindrical :: P -> P
miller_cylindrical (φ, λ) =
  let x = λ
      y = (5/4) * log (tan ((pi / 4) + (2/5 * φ)))
  in (x,y)

-- | Ernst Hammer, 1892
nell_hammer :: P -> P
nell_hammer (φ, λ) =
    let x = λ * (1 + cos φ) / 2
        y = 2 * (φ - tan (φ / 2))
    in (x,y)

-- | Hipparchus
planisphere :: R -> P -> P
planisphere r (φ, λ) = Geometry.polar_to_rectangular (2 * r * tan (pi / 4 - φ / 2),λ)

-- | R. V. Putnins, 1934
putnins_p1 :: P -> P
putnins_p1 = f_2_2_5 0.94745 0.94745 0 3

-- | Jean Cossin of Dieppe, 1570
sinusoidal :: P -> P
sinusoidal (φ, λ) =
    let x = λ * cos φ
        y = φ
    in (x,y)

-- | <https://mathworld.wolfram.com/OrthographicProjection.html>
orthographic :: R -> P -> P -> Maybe P
orthographic r (φ0,λ0) (φ,λ) =
  let x = r * cos φ * sin (λ - λ0)
      y = r * (cos φ0 * sin φ - sin φ0 * cos φ * cos (λ - λ0))
      c = sin φ0 * sin φ + cos φ0 * cos φ * cos (λ - λ0)
  in if c < 0 then Nothing else Just (x,y)

-- | <https://mathworld.wolfram.com/StereographicProjection.html>
stereographic :: R -> R -> R -> P -> P
stereographic r φ1 λ0 (φ,λ) =
  let k = (2 * r) / (1 + sin φ1 * sin φ + cos φ1 * cos φ * cos (λ - λ0))
      x = k * cos φ * sin (λ - λ0)
      y = k * (cos φ1 * sin φ - sin φ1 * cos φ * cos (λ - λ0))
  in (x,y)

wagner_ii :: P -> P
wagner_ii (φ, λ) =
    let θ = asin (0.88022 * sin (0.8855 * φ))
        x = 0.92483 * λ * cos θ
        y = 1.38725 * θ
    in (x,y)

-- | K.H. Wagner, 1932
wagner_vi :: P -> P
wagner_vi (φ, λ) =
    let x = λ * asqrt (1 - 3 * (Math.sqr (φ / π)))
        y = φ
    in (x,y)

-- * Naming

-- | (P->P) projections, by name.
projectionP_name_table_r :: [(P -> P,String)]
projectionP_name_table_r =
    [((\(p,q) -> (q,p)),"Identity")
    ,(bonne (pi/2),"Bonne (φ1=π/2)")
    ,(cassini,"Cassini")
    ,(collignon,"Collignon")
    ,(craster,"Craster")
    ,(denoyer,"Denoyer")
    ,(eckert_i,"Eckert I")
    ,(eckert_ii,"Eckert II")
    ,(eckert_iii,"Eckert III")
    ,(eckert_v,"Eckert V")
    ,(fahey,"Fahey")
    ,(foucaut 0.5,"Foucaut (n=1/2)")
    ,(gall_peters 1,"Gall/Peters (r=1)")
    ,(ginsburg_viii,"Ginsburg VIII")
    ,(kavrayskiy_vii,"Kavrayskiy VII")
    ,(lambert_conformal_conic 0 0 (pi/2) (pi/2)
     ,"Lambert Conformal Conic (λ0=0,φ0=0,φ1=π/2,φ2=π/2)")
    ,(larrivee,"Larrivee")
    ,(nell_hammer,"Nell/Hammer")
    ,(putnins_p1,"Putnins P1")
    ,(sinusoidal,"Sinusoidal")
    ,(van_der_grinten_i,"van der Grinten I")
    ,(wagner_ii,"Wagner II")
    ,(wagner_vi,"Wagner VI")
    ]

projectionP_name_table :: [(String,P -> P)]
projectionP_name_table =
    let swap :: (p,q) -> (q,p)
        swap (p,q) = (q,p)
    in map swap projectionP_name_table_r

-- | (P->Maybe P) projections, by name.
projectionM_name_table :: [(String,P -> Maybe P)]
projectionM_name_table =
    [("Mercator (ε=~π/2)",mercator ((pi / 2) * 0.96))
    ,("Mercator [Transverse]",mercator_transverse)]

{- | Composite of lifted 'projectionP_name_table' and 'projectionM_name_table'.

>> putStrLn $ unlines $ map fst projection_name_table
-}
projection_name_table :: [(String,P -> Maybe P)]
projection_name_table =
    let f (nm,fn) = (nm,return . fn)
    in map f projectionP_name_table ++ projectionM_name_table

{-
reverse_lookup :: Eq k => k -> [(v,k)] -> Maybe v
reverse_lookup k = lookup k . map (\(p,q) -> (q,p))

projectionD_by_name :: String -> Maybe (P -> P)
projectionD_by_name nm = reverse_lookup nm projectionD_name_table

projectionM_by_name :: String -> Maybe (P -> Maybe P)
projectionM_by_name nm = reverse_lookup nm projectionM_name_table

projectionL_by_name :: String -> Maybe (P -> Maybe P)
projectionL_by_name nm = lookup nm projectionL_name_table
-}
