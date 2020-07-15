{-# LANGUAGE TemplateHaskell #-}
module Untod.Version(
        utVersion, 
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
utVstring = concat  [ "Version: ",
                      utVersion,
                      " ",
                      utGitInf
                    ]
utGitmax  = concat  [ utVstring,
                      "\n[",
                      giBranch gi,
                      "] ",
                      giCommitMessage gi
                    ]
