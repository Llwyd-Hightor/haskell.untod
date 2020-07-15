module Untod.Formatters
    where

import Data.Time
import Text.Printf
import Untod.Data

formatHeaders :: Bool -> Bool -> [String]
formatHeaders False _ = []
formatHeaders True  False = 
    [ "Ext       TOD              Date          Time        Zone     Julian   D    Perp        Unix      Leap"
   , "--- ----------------- : ---------- --------------- --------- -------- --- -------- -------------- ----" 
    ]                
formatHeaders True True = 
    [ "ExtTOD,Date,Time,Zone,Julian,D,Perp,Unix,Leap" 
    ]

formatTod :: Integer -> String -> String
formatTod t z = concat [a," ",b," ",c,"---",z] where
    s = printf "%016x" t
    a = take 3 s
    b = take 8 $ drop 3 s
    c = drop 11 s

formatYMD :: UTCTime -> String
formatYMD  = ftime "%F"

formatHMS :: UTCTime -> String
formatHMS u = take 15 $ ftime "%T%0Q" u

formatZone :: TickMode -> Int -> String
formatZone t s = r where
    x = quotRem s 3600
    h = printf "%+03d" (fst x)
    m = printf "%02d"  (abs (div (snd x) 60))
    r = concat [show t,h,":",m]

formatJul :: UTCTime -> String
formatJul  = ftime "%Y.%j"

formatDay :: UTCTime -> String
formatDay  = ftime "%a"

formatPmc :: Maybe Integer -> String
formatPmc (Just i) = printf "%08x" i   
formatPmc Nothing = "--------"   

formatUnix :: Bool -> Integer -> String
formatUnix False i = printf "%14d" i
formatUnix True i = printf "%d" i

formatLsec :: Bool -> TickMode -> Int -> String
formatLsec _ UTC i = printf "*%+d" i
formatLsec True _  _ = "NA"                     -- CSV mode
formatLsec False _  _ = "    "                  -- Pad Non-CSV mode

formatAnnot :: Uargs -> String
formatAnnot a = 
    if   annot a > csv a 
        then show $ runmode a
        else []

ftime :: String -> UTCTime -> String
ftime = formatTime defaultTimeLocale

padTod :: PadMode -> String -> String
padTod R v = 
    printf "%16s" $ take 16 ( v ++ zeropad )
padTod L v = take 16 v 
padTod I v = if 'B' < head v
    then printf "000%-13s" $ take 13 ( v ++ zeropad )
    else printf "00%-14s"  $ take 14 ( v ++ zeropad )
    
zeropad :: String
zeropad = "0000000000000000"
