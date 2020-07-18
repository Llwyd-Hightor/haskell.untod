{-# LANGUAGE TemplateHaskell #-}
module Untod.Version(
        utVersion,
        utVshort,
        utVstring,
        utGitInf,
        utGitmax
        ) where
import GitHash
import Data.Version
import Paths_untod

gi = $$tGitInfoCwd

utDirty
    | giDirty gi = "*"
    | otherwise  = ""

utVersion = showVersion version
utGitInf  = concat  [ "(#", take 7 $ giHash gi,
                      utDirty,
                      " -- " , giCommitDate gi,
                      ")"
                    ]
utVshort  = "Version: " ++ utVersion
utVstring = utVshort ++ " " ++ utGitInf
utGitmax  =
    [ utVstring, "(" ++  giBranch gi ++ ") " ++ giCommitMessage gi ]
