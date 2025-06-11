-- | Char functions
module Music.Theory.Char where

{- | Table mapping digit characters to subscript characters.

>>> length numeric_subscripts
10

>>> let Just x = lookup '3' numeric_subscripts
>>> putChar x
₃
-}
numeric_subscripts :: [(Char, Char)]
numeric_subscripts =
  [ ('0', '₀') -- U+2080 ₀ Subscript Zero
  , ('1', '₁') -- U+2081 ₁ Subscript One
  , ('2', '₂') -- U+2082 ₂ Subscript Two
  , ('3', '₃') -- U+2083 ₃ Subscript Three
  , ('4', '₄') -- U+2084 ₄ Subscript Four
  , ('5', '₅') -- U+2085 ₅ Subscript Five
  , ('6', '₆') -- U+2086 ₆ Subscript Six
  , ('7', '₇') -- U+2087 ₇ Subscript Seven
  , ('8', '₈') -- U+2088 ₈ Subscript Eight
  , ('9', '₉') -- U+2089 ₉ Subscript Nine
  ]

-- * Greek letters

{- | Table of greek letters (upper-case,lower-case,name).

>>> length greek_letters
24

>>> (['Α' .. 'Ρ'] ++ ['Σ' .. 'Ω']) == map (\(c,_,_) -> c) greek_letters
True

>>> (['α' .. 'ρ'] ++ ['σ' .. 'ω']) == map (\(_,c,_) -> c) greek_letters
True
-}
greek_letters :: [(Char, Char, String)]
greek_letters =
  [ ('Α', 'α', "Alpha")
  , ('Β', 'β', "Beta")
  , ('Γ', 'γ', "Gamma")
  , ('Δ', 'δ', "Delta")
  , ('Ε', 'ε', "Epsilon")
  , ('Ζ', 'ζ', "Zeta")
  , ('Η', 'η', "Eta")
  , ('Θ', 'θ', "Theta")
  , ('Ι', 'ι', "Iota")
  , ('Κ', 'κ', "Kappa")
  , ('Λ', 'λ', "Lambda")
  , ('Μ', 'μ', "Mu")
  , ('Ν', 'ν', "Nu")
  , ('Ξ', 'ξ', "Xi")
  , ('Ο', 'ο', "Omicron")
  , ('Π', 'π', "Pi")
  , ('Ρ', 'ρ', "Rho")
  , ('Σ', 'σ', "Sigma")
  , ('Τ', 'τ', "Tau")
  , ('Υ', 'υ', "Upsilon")
  , ('Φ', 'φ', "Phi")
  , ('Χ', 'χ', "Chi")
  , ('Ψ', 'ψ', "Psi")
  , ('Ω', 'ω', "Omega")
  ]
