module Untod.Formatters
    where

import Text.Printf
import Untod.Data

headerList :: Bool -> Bool -> [String]
headerList False _ = []
headerList True  False = 
    [ "Ext       TOD              Date          Time        Zone     Julian   D    Perp        Unix      Leap"
   , "--- ----------------- : ---------- --------------- --------- -------- --- -------- -------------- ----" 
    ]                
headerList True True = 
    [ "ExtTOD,Date,Time,Zone,Julian,D,Perp,Unix,Leap" 
    ]

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

formatAnnot :: Uargs -> String
formatAnnot u = 
    if   annot u > csv u 
        then show $ runmode u
        else []
