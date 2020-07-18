module Untod.Process
    where
import Data.Maybe
import Data.Time
import Text.Read (readMaybe)

import Untod.Data
import Untod.Formatters
import Untod.LeapSecTab
import Untod.Utils
-- =======================================================================

tBase = ymdToUTC 1900 1 1
pBase = ymdToUTC 1966 1 3
uBase = ymdToUTC 1970 1 1
ptDelta = diffUTCTime pBase tBase
utDelta = diffUTCTime uBase tBase
-- =======================================================================
processAll :: [String] -> Uargs -> Uwork -> [Int] -> [String]
processAll [] a w z
    | runmode a == DATE = processAll [uNow w] a w z
    | otherwise = ["No data provided"]
processAll l a w z =
    formatHeaders (headers a) (csv a) ++
    processData l a w z
-- =======================================================================
processData :: [String] -> Uargs -> Uwork -> [Int] -> [String]
processData [] _ _ _ = []
processData (v:vs) a w z =
    processZones v a w z ++
    processData vs a w z
-- =======================================================================
processZones :: String -> Uargs -> Uwork -> [Int] -> [String]
processZones [] _ _ _ = []
processZones v a w (z:zs) =
    processOne v a w z ++
    processZones v a w zs
processZones _ _ _ _ = []
-- =======================================================================
processOne :: String -> Uargs -> Uwork -> Int -> [String]
processOne v a w z = [result] where
    result = case runmode a of
        TOD  -> processFromTOD  v a w z
        DATE -> processFromDATE v a w z
        PMC  -> processFromPMC  v a w z
        UNIX -> processFromUNIX v a w z
        CSEC -> processFromCSEC v a w z
-- =======================================================================
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
    ptod = padTod (padmode a) v
    xtod = readMaybe ("0x" ++ ptod) :: Maybe Int
    itod = fromJust xtod
    lsec = lsSearchByTOD (tickmode a) itod
    tdiff = fromIntegral $ lsec - tAdj w - z
    xdate = addUTCTime (0.000001 * fromIntegral itod - tdiff) tBase
    udiff = fromIntegral $ lsec - tAdj w
    udate = addUTCTime (0.000001 * fromIntegral itod - udiff) tBase
-- =======================================================================
processFromDATE :: String -> Uargs -> Uwork -> Int -> String
processFromDATE v a w z = r where
    r = if isNothing xdate
        then v ++ " is not a recognisable date/time"
        else joinRow (rSep w)
            [ formatTod ltod (tSep w)
            , formatYMD $ fromJust xdate
            , formatHMS $ fromJust xdate
            , formatZone (tickmode a) z
            , formatJul  $ fromJust xdate
            , formatDay  $ fromJust xdate
            , formatPmc  $ calcPmc z udate
            , formatUnix (csv a) $ calcUnix z udate
            , formatLsec (csv a) (tickmode a) lsec
            ]
              (formatAnnot a)

    xdate = getdate v
    udate = fromJust xdate
    lsec = lsSearchByDay (tickmode a) udate
    ldiff = fromIntegral $ lsec - tAdj w - z
    tdate = addUTCTime ldiff udate
    ltod = round $ (1000000 *) $ diffUTCTime tdate tBase
-- =======================================================================
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
processFromUNIX :: String -> Uargs -> Uwork -> Int -> String
processFromUNIX v a w z = r where
    r = if isNothing inval
        then idata ++ " is not a valid Unix clock value"
        else processfromcoru v a w z vsec
    idata = v  
    inval = getunix idata  
    vsec  = fromJust inval
-- =======================================================================
processFromCSEC :: String -> Uargs -> Uwork -> Int -> String
processFromCSEC v a w z = r where
    r = if isNothing inval
        then idata ++ " is not valid unsigned seconds value"
        else processfromcoru v a w z vsec
    idata = v  
    inval = getcsec idata
    vsec  = fromJust inval - floor utDelta
-- =======================================================================
-- =======================================================================
processfromcoru :: String -> Uargs -> Uwork -> Int -> Int -> String
processfromcoru v a w z vsec = r where
    r = joinRow (rSep w)
        [ formatTod (1000000 * (tsec + lsec)) (tSep w)
        , formatYMD udate
        , formatHMS udate
        , formatZone (tickmode a) z 
        , formatJul udate
        , formatDay udate
        , formatPmc  $ calcPmc z udate
        , formatUnix (csv a) vsec
        , formatLsec (csv a) (tickmode a) lsec
        ]
        (formatAnnot a)

    tsec = vsec + (z + tAdj w) + floor utDelta
    udate = addUTCTime (fromIntegral tsec) tBase 
    lsec = lsSearchByTOD (tickmode a) (1000000 * tsec)
-- =======================================================================
getdate :: String -> Maybe UTCTime
getdate s = if isNothing d
    then ptime "%Y.%j@%T%Q" j
    else d where
        t = s ++ drop (length s) "1900-01-01@00:00:00.000000000"
        j = s ++ drop (length s) "1900.001@00:00:00.000000000"
        d = ptime "%F@%T%Q" t

getpmc :: String -> Maybe Int
getpmc [] = Nothing
getpmc ('-':ss) = Nothing
getpmc s = readMaybe ("0x" ++ s) :: Maybe Int

getcsec :: String -> Maybe Int
getcsec [] = Nothing
getcsec ('-':ss) = Nothing
getcsec s = readMaybe s :: Maybe Int

getunix :: String -> Maybe Int
getunix [] = Nothing
getunix s = r where
    x = readMaybe s :: Maybe Int
    r
      | isNothing x = Nothing
      | (0>) $ fromJust x + round utDelta = Nothing
      | otherwise = x

calcUnix :: Int -> UTCTime -> Int
calcUnix z t = 
    (z +) $ floor $ diffUTCTime t uBase

calcPmc :: Int -> UTCTime -> Maybe Int
calcPmc z t = fixPmc x where
    -- x = toInt (((z +) $ floor $ diffUTCTime t pBase) / 60)
    x = quot (z + floor (diffUTCTime t pBase)) 60

fixPmc :: Int -> Maybe Int
fixPmc i = if i >= 0
        then Just i
        else Nothing
