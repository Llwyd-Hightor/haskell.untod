module Untod.Formatters
    where

import Data.Time
import Text.Printf
import Untod.Data
import Untod.Utils

formatHeaders :: Bool -> Bool -> Bool -> [String]
formatHeaders _ False _ = []
formatHeaders _ True False =
    [ "Ext       TOD              Date          Time        Zone     Julian   D    Perp        Unix      Leap"
   , "--- ----------------- : ---------- --------------- --------- -------- --- -------- -------------- ----"
    ]
formatHeaders False True True =
    [ "ExtTOD,Date,Time,Zone,Julian,D,Perp,Unix,Leap" ]
formatHeaders True True True =
    [ "ExtTOD,Date,Time,Zone," ++
      "                                     " ++
      "Julian,D,Perp,Unix,Leap" ]

formatTod :: Int -> String -> String
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

formatPmc :: Maybe Int -> String
formatPmc (Just i) = printf "%08x" i
formatPmc Nothing = "--------"

formatUnix :: Bool -> Int -> String
formatUnix False i = printf "%14d" i
formatUnix True i = printf "%d" i

formatLsec :: Bool -> TickMode -> Int -> String
formatLsec _ UTC i = printf "*%+d" i
formatLsec True _  _ = "NA"                     -- CSV mode
formatLsec False _  _ = "    "                  -- Pad Non-CSV mode

formatAnnot :: Uargs -> String
formatAnnot a =
    if annot a > csv a
        then show $ runmode a
        else []

padTod :: PadMode -> String -> String
padTod R v =
    printf "%16s" $ take 16 ( v ++ zeropad )
padTod L v = take 16 v
padTod I v = if 'B' < head v
    then printf "000%-13s" $ take 13 ( v ++ zeropad )
    else printf "00%-14s"  $ take 14 ( v ++ zeropad )

padPmc :: PadMode -> String -> String
padPmc R v = printf "%8s" $ take 8 ( v ++ zeropad )
padPmc L v = v
padPmc I v = if '0' == head v
    then v else padPmc R v

zeropad :: String
zeropad = "0000000000000000"
