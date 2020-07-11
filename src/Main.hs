module Main where

import Untod.Args
import Untod.LeapSecTab
import Untod.Offsets
import Options.Applicative
import System.Environment
import Data.Time
import Text.Read
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
print zList
-- print utClip
-- print (uClip utwork)
