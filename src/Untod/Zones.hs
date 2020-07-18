module Untod.Zones
      where
import Untod.Data
import Untod.Utils
import Text.Read
-- =======================================================================
convZone :: Maybe String -> Maybe Int
convZone Nothing = Nothing
convZone (Just s) = 
      fmap (\x -> round (3600 * x)) (readMaybe s :: Maybe Float)
-- =======================================================================
buildZlist :: Uargs -> Uwork -> [Int]
buildZlist a w = zgmt ++ zlocal ++ zaltern where
      zgmt = buildgmt (zulu a) zlocal zaltern
      zlocal = buildlocal (lzone a) (lEnvZone w) (lSysZone w) zaltern
      zaltern = buildaltern (azone a) (aEnvZone w)
-- =======================================================================
buildgmt    :: Bool -> [Int] -> [Int] -> [Int]
buildgmt True  [0] _   = []
buildgmt True  _   [0] = []
buildgmt True  _   _   = [0]
buildgmt False []  []  = [0]
buildgmt False _   _   = []
-- =======================================================================
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
            p' = round $ 3600 * p :: Int
buildlocal (Just p) _ _ _ = [round $ 3600 * p]
-- =======================================================================
buildaltern :: Maybe Float -> Maybe Int -> [Int]
buildaltern Nothing Nothing = []
buildaltern Nothing (Just o) = [o]
buildaltern (Just o) _ = [round $ 3600 * o]
-- =======================================================================
