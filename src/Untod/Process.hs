module Untod.Process
    where

import Untod.Data
import Untod.Formatters
import Data.List

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
processOne v a w z =
    [init $ joinRow x [v,formatZone (tickmode a) z]] where
        x = if csv a then "," else " "

joinRow :: String -> [String] -> String
joinRow  _ [] =  []
joinRow x (s:ss) = s ++ x ++ joinRow x ss
