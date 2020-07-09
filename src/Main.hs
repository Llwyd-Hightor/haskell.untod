module Main where

import Untod.Args
import Options.Applicative
-- import Data.Semigroup((<>))

main :: IO ()
main = process =<< execParser utOpts

process :: Uargs -> IO ()
process (s) = print s
