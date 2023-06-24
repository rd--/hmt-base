{- | Ordinary time and duration notations.
     In terms of Weeks, Days, Hours, Minutes, Second and Centiseconds.
     c.f. "Music.Theory.Time.Duration".
-}
module Music.Theory.Time.Notation where

import Text.Printf {- base -}

import qualified Data.List.Split as Split {- split -}
import qualified Data.Time as Time {- time -}

import qualified Music.Theory.Function as Function {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}

-- * Integral types

-- | Week, one-indexed, ie. 1-52
type Week = Int

-- | Week, one-indexed, ie. 1-31
type Day = Int

-- | Hour, zero-indexed, ie. 0-23
type Hour = Int

-- | Minute, zero-indexed, ie. 0-59
type Min = Int

-- | Second, zero-indexed, ie. 0-59
type Sec = Int

-- | Centi-seconds, zero-indexed, ie. 0-99
type Csec = Int -- (0-99)

-- * Composite types

-- | Minutes, seconds as @(min,sec)@
type MinSec = (Min,Sec)

-- | Generic MinSec
type GMinSec n = (n,n)

-- | Minutes, seconds, centi-seconds as @(min,sec,csec)@
type MinCsec = (Min,Sec,Csec)

-- | Generic MinCsec
type GMinCsec n = (n,n,n)

-- | (Hours,Minutes,Seconds)
type Hms = (Hour,Min,Sec)

-- | (Days,Hours,Minutes,Seconds)
type Dhms = (Day,Hour,Min,Sec)

-- * Fractional types

-- | Fractional days.
type FDay = Double

-- | Fractional hour, ie. 1.50 is one and a half hours, ie. 1 hour and 30 minutes.
type FHour = Double

-- | Fractional minute, ie. 1.50 is one and a half minutes, ie. 1 minute and 30 seconds, cf. 'FMinSec'
type FMin = Double

-- | Fractional seconds.
type FSec = Double

-- | Fractional minutes and seconds (mm.ss, ie. 01.45 is 1 minute and 45 seconds).
type FMinSec = Double

-- * Time.UTCTime format strings.

-- | 'Time.parseTimeOrError' with 'Time.defaultTimeLocale'.
parse_time_str :: Time.ParseTime t => String -> String -> t
parse_time_str = Time.parseTimeOrError True Time.defaultTimeLocale

format_time_str :: Time.FormatTime t => String -> t -> String
format_time_str = Time.formatTime Time.defaultTimeLocale

-- * Iso-8601

{- | Parse date in ISO-8601 extended (@YYYY-MM-DD@) or basic (@YYYYMMDD@) form.

>>> Time.toGregorian (Time.utctDay (parse_iso8601_date "2011-10-09"))
(2011,10,9)

>>> Time.toGregorian (Time.utctDay (parse_iso8601_date "20190803"))
(2019,8,3)
-}
parse_iso8601_date :: String -> Time.UTCTime
parse_iso8601_date s =
  case length s of
    8 -> parse_time_str "%Y%m%d" s -- basic
    10 -> parse_time_str "%F" s -- extended
    _ -> error "parse_iso8601_date?"

{- | Format date in ISO-8601 form.

>>> format_iso8601_date True (parse_iso8601_date "2011-10-09")
"2011-10-09"

>>> format_iso8601_date False (parse_iso8601_date "20190803")
"20190803"
-}
format_iso8601_date :: Time.FormatTime t => Bool -> t -> String
format_iso8601_date ext = if ext then format_time_str "%F" else format_time_str "%Y%m%d"

{- | Format date in ISO-8601 (@YYYY-WWW@) form.

>>> map (format_iso8601_week . parse_iso8601_date) ["2017-01-01","2011-10-09"]
["2016-W52","2011-W40"]
-}
format_iso8601_week :: Time.FormatTime t => t -> String
format_iso8601_week = format_time_str "%G-W%V"

{- | Parse ISO-8601 time is extended (@HH:MM:SS@) or basic (@HHMMSS@) form.

>>> format_iso8601_time True (parse_iso8601_time "21:44:00")
"21:44:00"

>>> format_iso8601_time False (parse_iso8601_time "172511")
"172511"
-}
parse_iso8601_time :: String -> Time.UTCTime
parse_iso8601_time s =
  case length s of
    6 -> parse_time_str "%H%M%S" s -- basic
    8 -> parse_time_str "%H:%M:%S" s -- extended
    _ -> error "parse_iso8601_time?"

{- | Format time in ISO-8601 form.

>>> format_iso8601_time True (parse_iso8601_date_time "2011-10-09T21:44:00")
"21:44:00"

>>> format_iso8601_time False (parse_iso8601_date_time "20190803T172511")
"172511"
-}
format_iso8601_time :: Time.FormatTime t => Bool -> t -> String
format_iso8601_time ext = format_time_str (if ext then "%H:%M:%S" else "%H%M%S")

{- | Parse date and time in extended or basic forms.

>>> Time.utctDayTime (parse_iso8601_date_time "2011-10-09T21:44:00") == Time.secondsToDiffTime 78240
True

>>> Time.utctDayTime (parse_iso8601_date_time "20190803T172511") == Time.secondsToDiffTime 62711
True
-}
parse_iso8601_date_time :: String -> Time.UTCTime
parse_iso8601_date_time s =
  case length s of
    15 -> parse_time_str "%Y%m%dT%H%M%S" s -- basic
    19 -> parse_time_str "%FT%H:%M:%S" s -- extended
    _ -> error ("parse_iso8601_date_time: " ++ s)

{- | Format date in @YYYY-MM-DD@ and time in @HH:MM:SS@ forms.

>>> t = parse_iso8601_date_time "2011-10-09T21:44:00"
>>> format_iso8601_date_time True t
"2011-10-09T21:44:00"

>>> format_iso8601_date_time False t
"20111009T214400"
-}
format_iso8601_date_time :: Time.FormatTime t => Bool -> t -> String
format_iso8601_date_time ext = format_time_str (if ext then "%FT%H:%M:%S" else "%Y%m%dT%H%M%S")

-- * FMin

{- | 'fsec_to_minsec' . '*' 60

>>> fmin_to_minsec 6.48
(6,29)
-}
fmin_to_minsec :: FMin -> MinSec
fmin_to_minsec = fsec_to_minsec . (*) 60

-- * FSec

{- | Translate fractional seconds to picoseconds.

>>> fsec_to_picoseconds 78240.05
78240050000000000
-}
fsec_to_picoseconds :: FSec -> Integer
fsec_to_picoseconds s = floor (s * (10 ** 12))

fsec_to_difftime :: FSec -> Time.DiffTime
fsec_to_difftime = Time.picosecondsToDiffTime . fsec_to_picoseconds

-- * FMinSec

{- | Translate fractional minutes.seconds to picoseconds.

>>> map fminsec_to_fsec [0.45,15.355]
[45.0,935.5]
-}
fminsec_to_fsec :: FMinSec -> FSec
fminsec_to_fsec n =
    let m = ffloor n
        s = (n - m) * 100
    in (m * 60) + s

fminsec_to_sec_generic :: (RealFrac f,Integral i) => f -> i
fminsec_to_sec_generic n =
    let m = floor n
        s = round ((n - fromIntegral m) * 100)
    in (m * 60) + s

{- | Fractional minutes are mm.ss, so that 15.35 is 15 minutes and 35 seconds.

>>> map fminsec_to_sec [0.45,15.35]
[45,935]
-}
fminsec_to_sec :: FMinSec -> Sec
fminsec_to_sec = fminsec_to_sec_generic

{- | Fractional seconds to fractional minutes.seconds.

>>> fsec_to_fminsec 935.5
15.355
-}
fsec_to_fminsec :: FSec -> FMinSec
fsec_to_fminsec n =
    let m = ffloor (n / 60)
        s = n - (m * 60)
    in m + (s / 100)

{- | Sec to FMinSec

>>> sec_to_fminsec 935
15.35
-}
sec_to_fminsec :: Sec -> FMinSec
sec_to_fminsec n =
    let m = ffloor (fromIntegral n / 60)
        s = fromIntegral n - (m * 60)
    in m + (s / 100)

{- | FMinSec addition

>>> fminsec_add 1.30 0.45
2.15

>>> fminsec_add 1.30 0.45
2.15
-}
fminsec_add :: Function.BinOp FMinSec
fminsec_add p q = fsec_to_fminsec (fminsec_to_fsec p + fminsec_to_fsec q)

fminsec_sub :: Function.BinOp FMinSec
fminsec_sub p q = fsec_to_fminsec (fminsec_to_fsec p - fminsec_to_fsec q)

{- | FMinSec multiplication

>>> fminsec_mul 0.45 2
1.3
-}
fminsec_mul :: Function.BinOp FMinSec
fminsec_mul t n = fsec_to_fminsec (fminsec_to_fsec t * n)

-- * FHour

-- | Type specialised 'fromInteger' of 'floor'.
ffloor :: Double -> Double
ffloor = fromInteger . floor

{- | Fractional hour to (hours,minutes,seconds).

>>> fhour_to_hms 21.75
(21,45,0)
-}
fhour_to_hms :: FHour -> Hms
fhour_to_hms h =
    let m = (h - ffloor h) * 60
        s = (m - ffloor m) * 60
    in (floor h,floor m,round s)

{- | Hms to fractional hours.

>>> hms_to_fhour (21,45,0)
21.75
-}
hms_to_fhour :: Hms -> FHour
hms_to_fhour (h,m,s) = fromIntegral h + (fromIntegral m / 60) + (fromIntegral s / (60 * 60))

{- | Fractional hour to seconds.

>>> fhour_to_fsec 21.75
78300.0
-}
fhour_to_fsec :: FHour -> FSec
fhour_to_fsec = (*) (60 * 60)

fhour_to_difftime :: FHour -> Time.DiffTime
fhour_to_difftime = fsec_to_difftime . fhour_to_fsec

-- * FDay

{- | Time in fractional days.

>>> round (utctime_to_fday (parse_iso8601_date_time "2011-10-09T09:00:00"))
55843

>>> round (utctime_to_fday (parse_iso8601_date_time "2011-10-09T21:00:00"))
55844
-}
utctime_to_fday :: Time.UTCTime -> FDay
utctime_to_fday t =
    let d = Time.utctDay t
        d' = fromIntegral (Time.toModifiedJulianDay d)
        s = Time.utctDayTime t
        s_max = 86401
    in d' + (fromRational (toRational s) / s_max)

-- * DiffTime

{- | 'Time.DiffTime' in fractional seconds.

>>> difftime_to_fsec (hms_to_difftime (21,44,30))
78270.0
-}
difftime_to_fsec :: Time.DiffTime -> FSec
difftime_to_fsec = fromRational . toRational

{- | 'Time.DiffTime' in fractional minutes.

>>> difftime_to_fmin (hms_to_difftime (21,44,30))
1304.5
-}
difftime_to_fmin :: Time.DiffTime -> Double
difftime_to_fmin = (/ 60) . difftime_to_fsec

{- | 'Time.DiffTime' in fractional hours.

>>> difftime_to_fhour (hms_to_difftime (21,45,00))
21.75
-}
difftime_to_fhour :: Time.DiffTime -> FHour
difftime_to_fhour = (/ 60) . difftime_to_fmin

hms_to_difftime :: Hms -> Time.DiffTime
hms_to_difftime = fhour_to_difftime . hms_to_fhour

-- * Hms

hms_to_sec :: Hms -> Sec
hms_to_sec (h,m,s) = h * 60 * 60 + m * 60 + s

{- | Seconds to (hours,minutes,seconds).

>>> map sec_to_hms [60-1,60+1,60*60-1,60*60+1]
[(0,0,59),(0,1,1),(0,59,59),(1,0,1)]
-}
sec_to_hms :: Sec -> Hms
sec_to_hms s =
  let (h,s') = s `divMod` (60 * 60)
      (m,s'') = sec_to_minsec s'
  in (h,m,s'')

{- | 'Hms' pretty printer.

>>> map (hms_pp True) [(0,1,2),(1,2,3)]
["01:02","01:02:03"]
-}
hms_pp :: Bool -> Hms -> String
hms_pp trunc (h,m,s) =
  if trunc && h == 0
  then printf "%02d:%02d" m s
  else printf "%02d:%02d:%02d" h m s

{- * 'Hms' parser.

>>> hms_parse "0:01:00"
(0,1,0)
-}
hms_parse :: String -> Hms
hms_parse x =
    case Split.splitOn ":" x of
      [h,m,s] -> (read h,read m,read s)
      _ -> error "parse_hms"

-- * MinSec

{- | 'divMod' by @60@.

>>> sec_to_minsec 123
(2,3)
-}
sec_to_minsec :: Integral n => n -> GMinSec n
sec_to_minsec = flip divMod 60

{- | Inverse of 'sec_minsec'.

>>> minsec_to_sec (2,3)
123
-}
minsec_to_sec :: Num n => GMinSec n -> n
minsec_to_sec (m,s) = m * 60 + s

-- | Convert /p/ and /q/ to seconds, apply /f/, and convert back to 'MinSec'.
minsec_binop :: Integral t => (t -> t -> t) -> GMinSec t -> GMinSec t -> GMinSec t
minsec_binop f p q = sec_to_minsec (f (minsec_to_sec p) (minsec_to_sec q))

{- | 'minsec_binop' '-', assumes /q/ precedes /p/.

>>> minsec_sub (2,35) (1,59)
(0,36)
-}
minsec_sub :: Integral n => GMinSec n -> GMinSec n -> GMinSec n
minsec_sub = minsec_binop (-)

{- | 'minsec_binop' 'subtract', assumes /p/ precedes /q/.

>>> minsec_diff (1,59) (2,35)
(0,36)
-}
minsec_diff :: Integral n => GMinSec n -> GMinSec n -> GMinSec n
minsec_diff = minsec_binop subtract

{- | 'minsec_binop' '+'.

>>> minsec_add (1,59) (2,35)
(4,34)
-}
minsec_add :: Integral n => GMinSec n -> GMinSec n -> GMinSec n
minsec_add = minsec_binop (+)

{- | 'foldl' of 'minsec_add'

>>> minsec_sum [(1,59),(2,35),(4,34)]
(9,8)
-}
minsec_sum :: Integral n => [GMinSec n] -> GMinSec n
minsec_sum = foldl minsec_add (0,0)

{- | 'round' fractional seconds to @(min,sec)@.

>>> map fsec_to_minsec [59.49,60,60.51]
[(0,59),(1,0),(1,1)]
-}
fsec_to_minsec :: FSec -> MinSec
fsec_to_minsec = sec_to_minsec . round

{- | 'MinSec' pretty printer.

>>> map (minsec_pp . fsec_to_minsec) [59,61]
["00:59","01:01"]
-}
minsec_pp :: MinSec -> String
minsec_pp (m,s) = printf "%02d:%02d" m s

-- * 'MinSec' parser.
minsec_parse :: (Num n,Read n) => String -> GMinSec n
minsec_parse x =
    case Split.splitOn ":" x of
      [m,s] -> (read m,read s)
      _ -> error ("minsec_parse: " ++ x)

-- * MinCsec

{- | Fractional seconds to @(min,sec,csec)@, csec value is 'round'ed.

>>> map fsec_to_mincsec [1,1.5,4/3]
[(0,1,0),(0,1,50),(0,1,33)]
-}
fsec_to_mincsec :: FSec -> MinCsec
fsec_to_mincsec tm =
    let tm' = floor tm
        (m,s) = sec_to_minsec tm'
        cs = round ((tm - fromIntegral tm') * 100)
    in (m,s,cs)

-- | Inverse of 'fsec_mincsec'.
mincsec_to_fsec :: Real n => GMinCsec n -> FSec
mincsec_to_fsec (m,s,cs) = realToFrac m * 60 + realToFrac s + (realToFrac cs / 100)

{- | GMinCsec -> Num

>>> map (mincsec_to_csec . fsec_to_mincsec) [1,6+2/3,123.45]
[100,667,12345]
-}
mincsec_to_csec :: Num n => GMinCsec n -> n
mincsec_to_csec (m,s,cs) = m * 60 * 100 + s * 100 + cs

{- | Centi-seconds to 'MinCsec'.

>>> map csec_to_mincsec [123,12345]
[(0,1,23),(2,3,45)]
-}
csec_to_mincsec :: Integral n => n -> GMinCsec n
csec_to_mincsec csec =
    let (m,cs) = csec `divMod` 6000
        (s,cs') = cs `divMod` 100
    in (m,s,cs')

{- | 'MinCsec' pretty printer, concise mode omits centiseconds when zero.

>>> map (mincsec_pp_opt True . fsec_to_mincsec) [1,60.5]
["00:01","01:00.50"]
-}
mincsec_pp_opt :: Bool -> MinCsec -> String
mincsec_pp_opt concise (m,s,cs) =
  if concise && cs == 0
  then printf "%02d:%02d" m s
  else printf "%02d:%02d.%02d" m s cs

{- | 'MinCsec' pretty printer.

>>> map (mincsec_pp . fsec_to_mincsec) [1,6+2/3,123.45]
["00:01.00","00:06.67","02:03.45"]
-}
mincsec_pp :: MinCsec -> String
mincsec_pp = mincsec_pp_opt False

mincsec_binop :: Integral t => (t -> t -> t) -> GMinCsec t -> GMinCsec t -> GMinCsec t
mincsec_binop f p q = csec_to_mincsec (f (mincsec_to_csec p) (mincsec_to_csec q))

-- * DHms

-- | Convert seconds into (days,hours,minutes,seconds).
sec_to_dhms_generic :: Integral n => n -> (n,n,n,n)
sec_to_dhms_generic n =
    let (d,h') = n `divMod` (24 * 60 * 60)
        (h,m') = h' `divMod` (60 * 60)
        (m,s) = m' `divMod` 60
    in (d,h,m,s)

{- | Type specialised 'sec_to_dhms_generic'.

>>> sec_to_dhms 1475469
(17,1,51,9)
-}
sec_to_dhms :: Sec -> Dhms
sec_to_dhms = sec_to_dhms_generic

{- | Inverse of 'seconds_to_dhms'.

>>> dhms_to_sec (17,1,51,9)
1475469
-}
dhms_to_sec :: Num n => (n,n,n,n) -> n
dhms_to_sec (d,h,m,s) = sum [d * 24 * 60 * 60,h * 60 * 60,m * 60,s]

-- | Generic form of 'parse_dhms'.
parse_dhms_generic :: (Integral n,Read n) => String -> (n,n,n,n)
parse_dhms_generic =
    let sep_elem = Split.split . Split.keepDelimsR . Split.oneOf
        sep_last x = let (e, x') = List.headTail (reverse x) in (reverse x',e)
        p x = case sep_last x of
                (n,'d') -> read n * 24 * 60 * 60
                (n,'h') -> read n * 60 * 60
                (n,'m') -> read n * 60
                (n,'s') -> read n
                _ -> error "parse_dhms"
    in sec_to_dhms_generic . sum . map p . filter (not . null) . sep_elem "dhms"

{- | Parse DHms text.
All parts are optional, order is not significant, multiple entries are allowed.

>>> parse_dhms "17d1h51m9s"
(17,1,51,9)

>>> parse_dhms "1s3d"
(3,0,0,1)

>>> parse_dhms "1h1h"
(0,2,0,0)
-}
parse_dhms :: String -> Dhms
parse_dhms = parse_dhms_generic

-- * Week

{- | Week that /t/ lies in.

>>> map (time_to_week . parse_iso8601_date) ["2017-01-01","2011-10-09"]
[52,40]
-}
time_to_week :: Time.UTCTime -> Week
time_to_week = read . format_time_str "%V"

-- * Util

-- | Given printer, pretty print time span.
span_pp :: (t -> String) -> (t,t) -> String
span_pp f (t1,t2) = concat [f t1," - ",f t2]
