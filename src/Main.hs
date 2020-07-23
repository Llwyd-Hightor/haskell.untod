{- |
Module      :  $Header$
Description :  untod: A Swiss Army knife for TOD clocks
Copyright   :  (c) 2020 Brent Longborough
License     :  BSD3

Maintainer  :  brent@llwyd-consulting.cymru
Stability   :  unstable
Portability :  portable

untod is a utility for conversion of date and time values
among System/Z TOD, Date and time, 
the IPARS Perpetual Mimute Clock,
the Unix seconds clock, 
and a seconds clock based on 1900-01-01

It has many options, including up to three timezones, 
a clock reference mode (UTC, LORAN, or TAI), 
and options to read from file, standard input, 
or the system clipboard.
-}
-- =======================================================================
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
      aEnvZone = aEnvZone
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
    let localZone = buildZlocal options utwork

    -- print options
    -- print utwork
    -- print zList
    case vvdisp options of
      0 -> fPrinc options (processAll (uInput utwork) options utwork zList localZone)
      1 -> fPrin [utVshort]
      2 -> fPrin [utVstring]
      _ -> fPrin utGitmax
    -- print utClip
    -- print (uClip utwork)
