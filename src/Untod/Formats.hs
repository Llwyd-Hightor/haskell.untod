module Untod.Formats
    where

import Text.Printf
import Untod.Data

formatTod :: Integer -> String
formatTod t = concat [a," ",b," ",c,"---"] where
    s = printf "%016x" t
    a = take 3 s
    b = (take 8) $ drop 3 s
    c = (drop 11 s)

formatPmc :: Integer -> String
formatPmc = printf "%08x"   

formatUnix :: Integer -> String
formatUnix = printf "%14d"

formatZone :: TickMode -> Int -> String
formatZone t s = r where
    x = quotRem s 3600
    h = printf "%+03d" (fst x)
    m = printf "%02d"  (abs (div (snd x) 60))
    r = concat [show t,h,":",m]
