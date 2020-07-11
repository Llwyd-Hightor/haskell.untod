module Main where

import Untod.Args
import Untod.Data
import Untod.Formats
import Untod.Zones
import Options.Applicative
import System.Environment (lookupEnv)
import Data.Time (timeZoneMinutes, getCurrentTimeZone)
import System.Clipboard
import System.Directory
import System.IO
import Control.DeepSeq

getInput :: Maybe String -> IO [String]
getInput Nothing = do
    return []
getInput (Just "-") = do
        contents <- getContents
        return $ words contents
getInput (Just s) = do
    isthere <- doesFileExist s
    if isthere then do
        handle <- openFile s ReadMode
        contents <- hGetContents handle
        contents `deepseq` hClose handle
        return $ words contents
    else do
        return []

getClip :: ( Bool, Maybe String ) -> [String]
getClip (False, _)      = []
getClip (True, Nothing) = []
getClip (True, Just s)  = words s

fPrin :: TickMode -> [Int] -> IO ()
fPrin _ [] = return ()
fPrin m (x:xs) = do
    print $ formatZone m x
    fPrin m xs

main :: IO ()
main = do

options <- execParser utOpts

aEnvZone <- lookupEnv "UNTOD_AZONE"
lEnvZone <- lookupEnv "UNTOD_LZONE"
lSysZone <- (liftA timeZoneMinutes getCurrentTimeZone)
utClip   <- getClipboardString 
utInput  <- getInput $ input options

let utwork = Uwork {
    aEnvZone = convZone aEnvZone
  , lEnvZone = convZone lEnvZone
  , lSysZone = lSysZone * 60
  , uInput   = (alist options) 
            ++ (getClip ((clip options), utClip))
            ++ utInput
}

let zList = buildZlist options utwork

print options
print utwork 
fPrin (tickmode options) zList
-- print utClip
-- print (uClip utwork)
