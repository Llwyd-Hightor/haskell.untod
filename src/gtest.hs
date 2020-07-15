import Options.Applicative
import System.Environment
import Data.Time
import Text.Read

pureCode :: (Maybe String, Maybe Int) -> (String, Int)
pureCode (Nothing, Nothing) = ([], 0)
pureCode (Just x, Nothing) = (x ++ "!", 0)
pureCode (Nothing, Just y) = ([], y + 1)
pureCode (Just x, Just y) = (x ++ "!", y + 1)

main = do
  env <- lookupEnv "UNTOD_LZONE"
  print env
  tz <- (liftA timeZoneMinutes getCurrentTimeZone)
  let result = pureCode (env, Just tz)
  print result
