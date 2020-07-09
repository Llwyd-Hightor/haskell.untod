module Untod.Args (
    utOpts,
    Uargs
    ) where
import Untod.Version
import Options.Applicative
import Data.Semigroup ((<>))

data Mode = TOD | DATE | PMC | UNIX | CSEC 
instance Show Mode where 
      show TOD  = "From TOD"
      show DATE = "From Date/Time"
      show PMC  = "From Perpetual Minute Clock"
      show UNIX = "From UNIX Seconds"
      show CSEC = "From 20th Century seconds"

data Uargs = Uargs
  { mode     :: Mode
  , clip     :: Bool
  , csv      :: Bool
  , headers  :: Bool
  , zulu     :: Bool
  , lor      :: Bool
  , tai      :: Bool
  , lpad     :: Bool
  , rpad     :: Bool
  , input    :: Maybe String
  , lzone    :: Maybe Float
  , azone    :: Maybe Float
  } deriving Show

utargs :: Parser Uargs
utargs = Uargs
      <$> ( 
            flag' TOD
          ( long "tod"
            <> short 'o'
            <> help "Convert from TOD (default)" )
        <|> flag' DATE
          ( long "date"
            <> short 'd'
            <> help "Convert from Date/Time" )
        <|> flag' PMC
          ( long "pmc"
            <> short 'p'
            <> help "Convert from PMC" )
        <|> flag' UNIX
          ( long "unix"
            <> short 'u'
            <> help "Convert from Unix seconds" )
        <|> flag' CSEC
          ( long "seconds"
            <> short 's'
            <> help "Convert from 20th Century seconds" )
        <|> pure TOD
          ) 
      <*> switch
          ( long "clipboard"
            <> short 'c'
            <> help "Input values from clipboard" )
      <*> switch
          ( long "csv"
            <> help "Output in CSV format" )
      <*> switch
          ( long "headers"
            <> help "Output column headers" )
      <*> switch
          ( long "zulu"
            <> short 'z'
            <> help "Suppress Zulu offset result if others given" )
      <*> switch
          ( long "loran"
            <> short 'l'
            <> help "Ignore leap-seconds -- LORAN/IBM" )
      <*> switch
          ( long "tai"
            <> short 't'
            <> help "Ignore leap-seconds -- TAI (International Atomic Clock" )
      <*> switch
          ( long "lpad"
            <> help "Pad TOD with zeroes on left" )
      <*> switch
          ( long "rpad"
            <> help "Pad TOD with zeroes on right (default is intelligent padding)" )
      <*> ( optional $ strOption
            (  long "input"
            <> short 'i'
            <> metavar "<filename>"
            <> help "Input values from a file ( - for STDIN )" )
            )
      <*> ( optional $ option auto
            (  long "lzone"
            <> metavar "<offset>"
            <> help "Override local time offset ([-+]n.n) [env: UNTOD_LZONE=]" )
            )
      <*> ( optional $ option auto
            (  long "azone"
            <> metavar "<offset>"
            <> help "Alternative time offset ([-+]n.n) [env: UNTOD_AZONE=]" )
            )

vOpts = infoOption utVstring 
    (long "version" <> short 'v' <> help "Show version")

vOgit = infoOption utGitmax 
    (long "vv" <> help "Show commit information")

utOpts = info (utargs <**> vOpts <**> vOgit <**> helper)
    ( fullDesc
    <> progDesc ("Converts among TOD, Date/Time, PARS Perpetual Minute Clock," 
    ++ "Unix seconds, and 20th century seconds for UTC, TAI or LORAN/IBM")
    <> header "untod - a Swiss Army knife for TOD and other clocks" 
    )
