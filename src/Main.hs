module Main where

import Untod.Args
import Untod.Data
import Untod.Formatters
import Untod.Zones
import Untod.Process
import Options.Applicative
import System.Environment (
    lookupEnv
    )
import Data.Time (
    timeZoneMinutes
  , getCurrentTime
  , formatTime
  , getCurrentTimeZone
  , defaultTimeLocale
  )
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

fPrin :: [String] -> IO ()
fPrin [] = return ()
fPrin (x:xs) = do
    putStrLn x
    fPrin xs

main :: IO ()
main = do
    options <- execParser utOpts

    aEnvZone <- lookupEnv "UNTOD_AZONE"
    lEnvZone <- lookupEnv "UNTOD_LZONE"
    lSysZone <- (liftA timeZoneMinutes getCurrentTimeZone)
    utClip   <- getClipboardString 
    utInput  <- getInput $ input options
    utNow    <- getCurrentTime

    let utwork = Uwork {
      aEnvZone = convZone aEnvZone
    , lEnvZone = convZone lEnvZone
    , lSysZone = lSysZone * 60
    , uInput   = (alist options) 
                ++ (getClip ((clip options), utClip))
                ++ utInput
    , uNow     = (take 27) $ ftime "%F@%T%Q" utNow
    , tSep     = if (csv options) then [] else " :"
    , rSep     = if (csv options) then "," else " "
    , tAdj     = if (tickmode options == TAI) then 10 else 0
    }

    let zList = buildZlist options utwork

    -- print options 
    -- print utwork 
    fPrin (processAll (uInput utwork) options utwork zList)
    -- print utClip
    -- print (uClip utwork)
