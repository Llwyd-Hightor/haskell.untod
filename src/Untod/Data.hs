module Untod.Data (
    Uargs(..)
  , Uwork(..)
  , PadMode(..)
  , RunMode(..)
  , TickMode(..)

) where

data RunMode = TOD | DATE | PMC | UNIX | CSEC
      deriving Eq
instance Show RunMode where
      show TOD  = " <== TOD"
      show DATE = " <== Date/Time"
      show PMC  = " <== PMC"
      show UNIX = " <== UNIX"
      show CSEC = " <== 20th Csec"

data TickMode = UTC | LOR | TAI
      deriving Eq
instance Show TickMode where
      show UTC = "UTC"
      show LOR = "LOR"
      show TAI = "TAI"

data PadMode = L | R | I
      deriving Eq
instance Show PadMode where
      show L = "lpad"
      show R = "rpad"
      show I = "intel"

data Uwork = Uwork {
    aEnvZone  :: Maybe String -- Possible alternate timezone from environment
  , lSysZone  :: Int          -- Local timezone (system)
  , lEnvZone  :: Maybe Int    -- Possible local timezone from environment
  , uInput    :: [String]     -- All input data, munged together as words
  , uNow      :: String       -- Currebt Date/Time
  , tSep      :: String       -- Separator for formatting the TOD clock
  , rSep      :: String       -- Separator for output fields (space or comma)
  , tAdj      :: Int          -- Seconds adjustment for tickmode TAI
} deriving Show

data Uargs = Uargs {
    runmode  :: RunMode        -- Calculation type
  , clip     :: Bool           -- Include data from clipboard
  , csv      :: Bool           -- Output in CSV format
  , headers  :: Bool           -- Output includes column headers
  , annot    :: Bool           -- Annotate plain output with run mode
  , zulu     :: Bool           -- Suppress GMT if any other zones
  , tickmode :: TickMode       -- Clock convention
  , padmode  :: PadMode        -- How to pad TOD clocks
  , input    :: Maybe String   -- Include input from file or STDIN
  , lzone    :: Maybe Float    -- Override local time zone
  , azone    :: Maybe Float    -- Provide one additional timezone
  , vvdisp   :: Int            -- Version flag(s)
  , alist    :: Maybe [String] -- Value parameter list from command line
  , chop     :: Bool           -- Chop output into two blocks
  } deriving Show
