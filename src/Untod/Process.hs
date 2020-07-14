module Untod.Process
    where

import Data.List
import Data.Maybe
import Data.Time
import Text.Read (readMaybe)

import Untod.Data
import Untod.Formatters
import Untod.LeapSecTab

tBase = UTCTime {
    utctDay = fromGregorian 1900 1 1 
  , utctDayTime = 0.0
} 
pBase = UTCTime {
    utctDay = fromGregorian 1966 1 3 
  , utctDayTime = 0.0
} 
uBase = UTCTime {
    utctDay = fromGregorian 1970 1 1 
  , utctDayTime = 0.0
} 

ptDelta = diffUTCTime pBase tBase
utDelta = diffUTCTime uBase tBase

processAll :: [String] -> Uargs -> Uwork -> [Int] -> [String]
processAll [] a w z
    | (runmode a) == DATE = processAll [thisDate] a w z
    | otherwise = ["No data provided"]
processAll l a w z =
    ( headerList (headers a) (csv a) ) ++
    ( processData l a w z)

thisDate = "2020-07-11"

processData :: [String] -> Uargs -> Uwork -> [Int] -> [String]
processData [] _ _ _ = []
processData (v:vs) a w z =
    ( processZones v a w z ) ++
    ( processData vs a w z )

processZones :: String -> Uargs -> Uwork -> [Int] -> [String]
processZones [] _ _ _ = []
processZones v a w (z:zs) =
    ( processOne v a w z ) ++
    ( processZones v a w zs )
processZones _ _ _ _ = []

processOne :: String -> Uargs -> Uwork -> Int -> [String]
processOne v a w z = [result] where
    result = case runmode a of
        TOD  -> processFromTOD  v a w z
        DATE -> processFromDATE v a w z
        PMC  -> processFromPMC  v a w z
        UNIX -> processFromUNIX v a w z
        CSEC -> processFromCSEC v a w z

processFromTOD  :: String -> Uargs -> Uwork -> Int -> String
processFromTOD  v a w z = r where
    r = if xtod == Nothing
      then
          ptod ++ " is not valid hexadecimal"
      else
        ( joinRow (rSep w)
        [ rtod, rdate, rtime, rzone, rjul
        , rday, rpmc, runix, rleap ])
       ++ rnote    
    rtod  = formatTod itod (tSep w)
    rdate = formatDatx xdate
    rtime = formatTimx xdate
    rjul  = formatJul xdate
    rzone = formatZone (tickmode a) z
    rday  = formatDay xdate
    rpmc  = formatPmc $ calcPMC xdate
    runix = formatUnix $ calcUnix udate
    rleap = formatLsec (tickmode a) lsec
    rnote = formatAnnot a
    xtod = readMaybe ("0x" ++ ptod) :: Maybe Integer
    itod = fromJust xtod
    ptod = padTod (padmode a) v
    lsec = lsSearchByTOD (tickmode a) itod
    tdiff = fromIntegral $ lsec - (tAdj w) - z
    udiff = fromIntegral $ lsec - (tAdj w)
    xdate = addUTCTime (0.000001 * (fromIntegral itod) - tdiff) tBase
    udate = addUTCTime (0.000001 * (fromIntegral itod) - udiff) tBase

calcPMC :: UTCTime -> Maybe Integer
calcPMC t = r where
    x = floor $ diffUTCTime t pBase
    r = if x >= 0 && x < 2^64 
        then Just $ div x 60
        else Nothing 

calcUnix :: UTCTime -> Integer
calcUnix t = floor $ diffUTCTime t uBase  

processFromDATE :: String -> Uargs -> Uwork -> Int -> String
processFromDATE v a w z = undefined

processFromPMC  :: String -> Uargs -> Uwork -> Int -> String
processFromPMC  v a w z = undefined

processFromUNIX :: String -> Uargs -> Uwork -> Int -> String
processFromUNIX v a w z = undefined

processFromCSEC :: String -> Uargs -> Uwork -> Int -> String
processFromCSEC v a w z = undefined

joinRow :: String -> [String] -> String
joinRow  _ [] =  []
joinRow x (s:ss) = s ++ x ++ joinRow x ss
