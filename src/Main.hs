module Main where

import Untod.Args
import Untod.Data
import Untod.Zones
import Untod.Process
import Untod.Utils
import Untod.Version
import Options.Applicative
import System.Environment (
    lookupEnv
    )
import Data.Time (
    timeZoneMinutes
  , getZonedTime
  , getCurrentTimeZone
  )
import System.Clipboard


main = do
    options <- execParser utOpts

    aEnvZone <- lookupEnv "UNTOD_AZONE"
    lEnvZone <- lookupEnv "UNTOD_LZONE"
    lSysZone <- fmap timeZoneMinutes getCurrentTimeZone
    utClip   <- getClipboardString
    utInput  <- getInput $ input options
    ltNow    <- getZonedTime

    let utwork = Uwork {
      aEnvZone = convZone aEnvZone
    , lEnvZone = convZone lEnvZone
    , lSysZone = lSysZone * 60
    , uInput   = getopt (alist options)
                ++ flatlines (nocomment (getClip (clip options) utClip))
                ++ flatlines (nocomment utInput)
    , uNow     = take 27 $ ftime "%F@%T%Q" ltNow
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
        1 -> fPrin [utVshort]
        2 -> fPrin [utVstring]
        _ -> fPrin utGitmax
    -- print utClip
    -- print (uClip utwork)
