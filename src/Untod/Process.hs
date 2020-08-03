module Untod.Process
    where
import Data.Char (toUpper)
import Data.Maybe
import Data.Time
import Text.Read (readMaybe)

import Untod.Data
import Untod.Formatters
import Untod.LeapSecTab
import Untod.Utils
-- =======================================================================
-- Constants
tBase = ymdToUTC 1900 1 1
pBase = ymdToUTC 1966 1 3
uBase = ymdToUTC 1970 1 1
ptDelta = diffUTCTime pBase tBase
utDelta = diffUTCTime uBase tBase

-- =======================================================================
-- Entry point:
--      Prepares any headers
--      Process all [input values] × [timezones]
--      Handle case of no input as local date/time
-- -----------------------------------------------------------------------
processAll :: [String] -> Uargs -> Uwork -> [Int] -> Int -> [String]
processAll [] a w z l
    | runmode a == DATE = processAll [uNow w] a w z l
    | otherwise = ["No data provided"]
processAll v a w z l =
    formatHeaders (chop a) (headers a) (csv a) ++
    processData v a w z l

-- =======================================================================
-- Expand calls for input data:
--      Process single input value for all timezones
-- -----------------------------------------------------------------------
processData :: [String] -> Uargs -> Uwork -> [Int] -> Int -> [String]
processData [] _ _ _ _ = []
processData (v:vs) a w z l =
    processZones v a w z l ++
    processData vs a w z l

-- =======================================================================
-- Expand calls for timezones:
--      Process single input value for single timezone
-- -----------------------------------------------------------------------
processZones :: String -> Uargs -> Uwork -> [Int] -> Int -> [String]
processZones [] _ _ _ _ = []
processZones v a w (z:zs) l =
    processOne v a w z l ++
    processZones v a w zs l
processZones _ _ _ _ _ = []

-- =======================================================================
-- Process one combinatio of input and timezone:
--      Choose which process according to run type
-- -----------------------------------------------------------------------
processOne :: String -> Uargs -> Uwork -> Int -> Int -> [String]
processOne v a w z l = [result] where
    result = case runmode a of
        TOD  -> processFromTOD  v a w z
        DATE -> processFromDATE v a w z l
        PMC  -> processFromPMC  v a w z
        UNIX -> processFromUNIX v a w z
        CSEC -> processFromCSEC v a w z

-- =======================================================================
-- Process input as decimal seconds since 1900-01-01 00:00:00
-- -----------------------------------------------------------------------
processFromCSEC :: String -> Uargs -> Uwork -> Int -> String
processFromCSEC v a w z = r where
    r = if isNothing inval
        then idata ++ " is not valid unsigned seconds value"
        else processfromcoru v a w z vsec
    idata = v
    inval = getcsec idata
    vsec  = fromJust inval - floor utDelta

-- =======================================================================
-- Process input as a date/time or julian date/time
--      Any sensible leading abbreviation of either:
--          yyyy-mm-dd@hh:mm:ss.uuuuuu
--          yyyy.ddd@hh:mm:ss.uuuuuu
-- -----------------------------------------------------------------------
processFromDATE :: String -> Uargs -> Uwork -> Int -> Int -> String
processFromDATE v a w z l = r where
    r = if isNothing xdate
        then v ++ " is not a recognisable date/time >= 1900"
        else joinRow (rSep w)
            [ formatTod ltod (tSep w)
            , formatYMD wdate
            , formatHMS wdate
            , formatZone (tickmode a) z
            , formatJul wdate
            , formatDay wdate
            , formatPmc  $ calcPmc z udate
            , formatUnix (csv a) $ calcUnix z udate
            , formatLsec (csv a) (tickmode a) lsec
            ]
              (formatAnnot a)
    xdate = getdate v
    udate = fromJust xdate
    lsec = lsSearchByDay (tickmode a) udate
    wdate = addUTCTime (fromIntegral (z-l)) udate
    ldiff = fromIntegral $ lsec - tAdj w - l
    tdate = addUTCTime ldiff udate
    ltod = round $ (1000000 *) $ diffUTCTime tdate tBase

-- =======================================================================
-- Process input as IPARS Perpetual Minute Clock (hexadecimal minutes)
-- -----------------------------------------------------------------------
processFromPMC :: String -> Uargs -> Uwork -> Int -> String
processFromPMC v a w z = r where
    r = if isNothing inval
        then v ++ " is not a valid PMC value"
        else joinRow (rSep w)
            [ formatTod  (1000000 * (tsec + lsec)) (tSep w)
            , formatYMD  ldate
            , formatHMS  ldate
            , formatZone (tickmode a) z
            , formatJul  ldate
            , formatDay  ldate
            , formatPmc  $ fixPmc pmin
            , formatUnix (csv a) rsec
            , formatLsec (csv a) (tickmode a) lsec
            ]
            (formatAnnot a)

    inval = getpmc $ padPmc (padmode a) v
    pmin = fromJust inval
    psec = 60 * pmin
    dsec = (psec +) $ round ptDelta
    tsec = dsec - z + tAdj w
    lsec = lsSearchByTOD (tickmode a) (1000000 * tsec)
    rsec = tsec + lsec - round utDelta
    ldate = addUTCTime (fromIntegral dsec) tBase

-- =======================================================================
-- Process input as System/Z (limited) extended TOD clock (hexadecimal)
-- -----------------------------------------------------------------------
processFromTOD  :: String -> Uargs -> Uwork -> Int -> String
processFromTOD  v a w z = r where
    r = if isNothing xtod
        then ptod ++ " is not valid hexadecimal"
        else joinRow (rSep w)
            [ formatTod itod (tSep w)
            , formatYMD xdate
            , formatHMS xdate
            , formatZone (tickmode a) z
            , formatJul xdate
            , formatDay xdate
            , formatPmc $ calcPmc z xdate
            , formatUnix (csv a) $ calcUnix 0 udate
            , formatLsec (csv a) (tickmode a) lsec
            ]
            (formatAnnot a)
    ptod = padTod (padmode a) (map toUpper v)
    xtod = readMaybe ("0x" ++ ptod) :: Maybe Int
    itod = fromJust xtod
    lsec = lsSearchByTOD (tickmode a) itod
    tdiff = fromIntegral $ lsec - tAdj w - z
    xdate = addUTCTime (0.000001 * fromIntegral itod - tdiff) tBase
    udiff = fromIntegral $ lsec - tAdj w
    udate = addUTCTime (0.000001 * fromIntegral itod - udiff) tBase

-- =======================================================================
-- Process input as decimal seconds Unix clock (since 1970-01-01 00:00:00)
-- -----------------------------------------------------------------------
processFromUNIX :: String -> Uargs -> Uwork -> Int -> String
processFromUNIX v a w z = r where
    r = if isNothing inval
        then idata ++ " is not a valid Unix clock value"
        else processfromcoru v a w z vsec
    idata = v
    inval = getunix idata
    vsec  = fromJust inval

-- =======================================================================
-- Common processor for Unix and centurt-based seconds common
-- -----------------------------------------------------------------------
processfromcoru :: String -> Uargs -> Uwork -> Int -> Int -> String
processfromcoru v a w z vsec = r where
    r = joinRow (rSep w)
        [ formatTod (1000000 * (tsec + lsec - z)) (tSep w)
        , formatYMD udate
        , formatHMS udate
        , formatZone (tickmode a) z
        , formatJul udate
        , formatDay udate
        , formatPmc  $ calcPmc 0 udate
        , formatUnix (csv a) vsec
        , formatLsec (csv a) (tickmode a) lsec
        ]
        (formatAnnot a)
    tsec = vsec + z + floor utDelta
    udate = addUTCTime (fromIntegral tsec) tBase
    lsec = lsSearchByTOD (tickmode a) (1000000 * tsec)

-- =======================================================================
-- Calculate Perpetual Minute Clock from UTC century seconds and timezone
-- -----------------------------------------------------------------------
calcPmc :: Int -> UTCTime -> Maybe Int
calcPmc z t = fixPmc x where
    x = quot (z + floor (diffUTCTime t pBase)) 60

-- =======================================================================
-- Calculate Unix Clock from UTC century seconds and timezone
-- -----------------------------------------------------------------------
calcUnix :: Int -> UTCTime -> Int
calcUnix z t =
    (z +) $ floor $ diffUTCTime t uBase

-- =======================================================================
-- Perpetual Minute Clock must be positive!
-- -----------------------------------------------------------------------
fixPmc :: Int -> Maybe Int
fixPmc i = if i >= 0
        then Just i
        else Nothing

-- =======================================================================
-- Convert input data as decimal 20th sentury seconds, with care
-- -----------------------------------------------------------------------
getcsec :: String -> Maybe Int
getcsec [] = Nothing
getcsec ('-':ss) = Nothing
getcsec s = readMaybe s :: Maybe Int

-- =======================================================================
-- Convert input date/time, padding with lower-order defaults
-- -----------------------------------------------------------------------
getdate :: String -> Maybe UTCTime
getdate s 
    | "19" > take 2 s = Nothing
    | isNothing d = ptime "%Y.%j@%T%Q" j
    | otherwise = d where
        t = s ++ drop (length s) "1900-01-01@00:00:00.000000000"
        j = s ++ drop (length s) "1900.001@00:00:00.000000000"
        d = ptime "%F@%T%Q" t

-- =======================================================================
-- Convert input data as unsigned hexadecimal PMC minutes, with care
-- -----------------------------------------------------------------------
getpmc :: String -> Maybe Int
getpmc [] = Nothing
getpmc ('-':ss) = Nothing
getpmc s = readMaybe ("0x" ++ s) :: Maybe Int

-- =======================================================================
-- Convert input data as decimal Unix seconds, with care
-- -----------------------------------------------------------------------
getunix :: String -> Maybe Int
getunix [] = Nothing
getunix s = r where
    x = readMaybe s :: Maybe Int
    r
      | isNothing x = Nothing
      | (0>) $ fromJust x + round utDelta = Nothing
      | otherwise = x
