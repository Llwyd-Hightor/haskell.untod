module Main where

import Untod.Args
import Untod.Data
import Untod.Formatters
import Untod.Zones
import Untod.Process
import Untod.Version
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
    lSysZone <- fmap timeZoneMinutes getCurrentTimeZone
    utClip   <- getClipboardString
    utInput  <- getInput $ input options
    utNow    <- getCurrentTime

    let utwork = Uwork {
      aEnvZone = convZone aEnvZone
    , lEnvZone = convZone lEnvZone
    , lSysZone = lSysZone * 60
    , uInput   = alist options
                ++ flatlines (nocomment (getClip (clip options) utClip))
                ++ flatlines (nocomment utInput)
    , uNow     = take 27 $ ftime "%F@%T%Q" utNow
    , tSep     = if csv options then [] else " :"
    , rSep     = if csv options then "," else " "
    , tAdj     = if tickmode options == TAI then 10 else 0
    }

    let zList = buildZlist options utwork

    -- print options
    -- print utwork
    -- print zList
    case vvdisp options of
        0 -> fPrin (processAll (uInput utwork) options utwork zList)
        1 -> fPrin [utVstring]
        _ -> fPrin [utGitmax]
    -- print utClip
    -- print (uClip utwork)
