{-# LANGUAGE TemplateHaskell #-}
module Untod.Version(
        utVersion,
        utVshort,
        utVstring,
        utGitInf,
        utGitmax
        ) where
import GitHash
gi = $$tGitInfoCwd

utDirty
    | giDirty gi = "*"
    | otherwise  = ""

utVersion = "0.1.0.0"
utGitInf  = concat  [ "(#", take 7 $ giHash gi,
                      utDirty,
                      " -- " , giCommitDate gi,
                      ")"
                    ]
utVshort  = "Version: " ++ utVersion
utVstring = utVshort ++ " " ++ utGitInf
utGitmax  =
    [ utVstring, "(" ++  giBranch gi ++ ") " ++ giCommitMessage gi ]
