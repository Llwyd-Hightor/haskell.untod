module Untod.Zones
      where
import Untod.Data
import Text.Read

int = round

convZone :: Maybe String -> Maybe Int
convZone Nothing = Nothing
convZone (Just s) = convhelper (readMaybe s :: Maybe Float)
convhelper :: Maybe Float -> Maybe Int
convhelper Nothing = Nothing
convhelper (Just f)  = Just $ int $ 60 * f  

buildZlist :: Uargs -> Uwork -> [Int]
buildZlist a w = zgmt ++ zlocal ++ zaltern where
      zgmt = buildgmt (zulu a) zlocal zaltern
      zlocal = buildlocal (lzone a) (lEnvZone w) (lSysZone w) zaltern
      zaltern = buildaltern (azone a) (aEnvZone w)

buildgmt    :: Bool -> [Int] -> [Int] -> [Int]
buildgmt False [0] _ = []
buildgmt False _ [0] = []
buildgmt False _ _   = [0]
buildgmt True [] []  = [0]
buildgmt True _ _    = []

buildlocal  :: Maybe Float -> Maybe Int -> Int -> [Int] -> [Int]
buildlocal Nothing Nothing s [] = [s]
buildlocal Nothing Nothing s [a]
      | s == a = []
      | otherwise = [s]
buildlocal Nothing Nothing _  _ = [0]
buildlocal Nothing (Just e) _ [a]
      | e == a = []
      | otherwise = [e]
buildlocal Nothing (Just e) _ _ = [e]
buildlocal (Just p) _ _ [a]
      | p' == a = []
      | otherwise = [p'] where
            p' = int $ 60 * p :: Int
buildlocal (Just p) _ _ _ = [int $ 3600 * p]

buildaltern :: Maybe Float -> Maybe Int -> [Int]
buildaltern Nothing Nothing = []
buildaltern Nothing (Just o) = [o]
buildaltern (Just o) _ = [int $ 3600 * o]

-- zoneOffsets :: Uargs -> [Maybe Int, Maybe Int, Maybe Int]
-- zoneOffsets x = map ($ x) [zonegmt, zoneLocal, zoneAlt]

-- zonegmt :: Uargs -> Maybe Int
-- zonegmt x = return Just 0

-- zoneLocal :: Uargs -> Maybe Int
-- zoneLocal x = getZone (lzone x) "UNTOD_LZONE" utLocal

-- zoneAlt :: Uargs -> IO Maybe Int
-- zoneAlt x = getZone (azone x) "UNTOD_AZONE" utnone

-- getZone :: Maybe Int -> String -> IO Maybe Int -> IO Maybe Int
-- getzone None env default = getzonee env default
-- getzone Just x = result where
--       result <- Just $ 60 * x

-- getZoneE :: String -> IO Maybe Int -> IO Maybe Int
-- getZoneE env default = do
--       let ez = lookupEnv env
--       let result =


-- main :: IO ()
-- main = do
--       let process =<< execParser utOpts

-- process :: Uargs -> IO ()
-- process (s) = print s
