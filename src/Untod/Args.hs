module Untod.Args (
    utOpts
  , Uargs(..)
  , Uwork(..)
  , RunMode(..)
  , TickMode(..)
    ) where
import Untod.Version
import Options.Applicative
import Data.Semigroup ((<>))

data RunMode = TOD | DATE | PMC | UNIX | CSEC 
instance Show RunMode where 
      show TOD  = "<== TOD"
      show DATE = "<== Date/Time"
      show PMC  = "<== Perpetual Minute Clock"
      show UNIX = "<== UNIX Seconds"
      show CSEC = "<== 20th Century seconds"

data TickMode = UTC | LOR | TAI 
instance Show TickMode where 
      show UTC = "UTC"
      show LOR = "LOR"
      show TAI = "TAI"

data PadMode = L | R | I 
instance Show PadMode where 
      show L = "lpad"
      show R = "rpad"
      show I = "intel"

data Uwork = Uwork {
    aEnvZone  :: Maybe Int
  , lSysZone  :: Int
  , lEnvZone  :: Maybe Int
  , uInput    :: [String]
} deriving Show

data Uargs = Uargs {
    runmode  :: RunMode           -- Calculation type
  , clip     :: Bool              -- Include data from clipboard
  , csv      :: Bool              -- Output in CSV format
  , headers  :: Bool              -- Output includes column headers
  , zulu     :: Bool              -- Suppress GMT if any other zones
  , tickmode :: TickMode          -- Clock convention
  , padmode  :: PadMode           -- How to pad TOD clocks
  , input    :: Maybe String      -- Include input from file or STDIN
  , lzone    :: Maybe Float       -- Override local time zone
  , azone    :: Maybe Float       -- Provide one additional timezone
  , alist    :: [String]          -- Value parameter list from command line 
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
      <*> ( 
            flag' LOR
          ( long "loran"
            <> short 'l'
            <> help "Ignore leap-seconds -- LORAN/IBM" )
        <|> flag' TAI
          ( long "tai"
            <> short 't'
            <> help "Ignore leap-seconds -- TAI (International Atomic Tick" )
        <|> pure UTC
          )
      <*> (
            flag' L
          ( long "lpad"
            <> help "Pad TOD with zeroes on left" )
        <|> flag' R
          ( long "rpad"
            <> help "Pad TOD with zeroes on right (default is intelligent padding)" )
        <|> pure I
          )
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
      <*> ( many $ argument str
            (  metavar "<value...>"
            <> help "Values for conversion" )
            )

vOpts = infoOption utVstring 
    (long "version" <> short 'v' <> help "Show version")

vOgit = infoOption utGitmax 
    (long "vv" <> help "Show commit information")

utOpts = info (utargs <**> vOpts <**> vOgit <**> helper)
    ( fullDesc
    <> progDesc ("Converts among TOD, Date/Time, PARS Perpetual Minute Tick," 
    ++ "Unix seconds, and 20th century seconds for UTC, TAI or LORAN/IBM")
    <> header "untod - a Swiss Army knife for TOD and other clocks" 
    )
