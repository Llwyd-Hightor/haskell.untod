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
