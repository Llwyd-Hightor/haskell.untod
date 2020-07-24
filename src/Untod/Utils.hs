module Untod.Utils
      where

import Data.Time
import System.Directory
import System.IO
import Control.DeepSeq
import Untod.Data

ymdToUTC :: Int -> Int -> Int -> UTCTime 
ymdToUTC y m d = UTCTime {
    utctDay = fromGregorian (toInteger y) m d
  , utctDayTime = 0.0
}

getInput :: Maybe String -> IO String
getInput Nothing = return []
getInput (Just "-") = getContents

getInput (Just s) = do
    isthere <- doesFileExist s
    if isthere then do
        handle <- openFile s ReadMode
        contents <- hGetContents handle
        contents `deepseq` hClose handle
        return contents
    else return []

getClip :: Bool -> Maybe String -> String
getClip False _      = []
getClip True Nothing = []
getClip True (Just s)  = s

nocomment :: String -> [String]
nocomment s = filter (\x -> '#' /= head x) (lines s)

flatlines :: [String] -> [String]
flatlines = concatMap words

getopt :: Maybe [String] -> [String]
getopt Nothing = []
getopt (Just s) = s

fPrin :: [String] -> IO ()
fPrin [] = return ()
fPrin (x:xs) = do
    putStrLn x
    fPrin xs

fPrinc :: Uargs -> [String] -> IO ()
fPrinc a [] = fPrin []
fPrinc a s = fPrin r where
    r 
      | chop a = map (take n) s ++ map (drop n) s
      | otherwise = s
    n 
      | csv a = 59
      | otherwise = 61  

ftime :: (FormatTime t) => String -> t -> String
ftime = formatTime defaultTimeLocale

ptime :: String -> String -> Maybe UTCTime
ptime = parseTimeM False defaultTimeLocale

joinRow :: String -> [String] -> String -> String
joinRow d s a = init (joinR d s) ++ a

joinR :: String -> [String] -> String
joinR  _ [] =  []
joinR d (s:ss) = s ++ d ++ joinR d ss
