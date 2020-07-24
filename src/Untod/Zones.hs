module Untod.Zones
      where
import Untod.Data
-- import Untod.Util
import Data.List
import Text.Read
-- =======================================================================
convZone :: Maybe String -> Maybe Int
convZone Nothing = Nothing
convZone (Just s) = 
      fmap (\x -> round (3600 * x)) (readMaybe s :: Maybe Float)
-- =======================================================================
buildZlist :: Uargs -> Uwork -> [Int]
buildZlist a w = nub $ zgmt ++ zlolalt where
      zlocal = buildlocal (lzone a) (lEnvZone w) (lSysZone w)
      zaltern = buildaltern (azone a) (aEnvZone w)
      zlolalt = zlocal:zaltern
      zgmt = buildgmt (zulu a) zlolalt

buildZlocal :: Uargs -> Uwork -> Int
buildZlocal a w = buildlocal (lzone a) (lEnvZone w) (lSysZone w) 
-- =======================================================================
buildgmt :: Bool -> [Int] -> [Int]
buildgmt True  _   = [0]
buildgmt False l = [0 | 0 `elem` l]
-- =======================================================================
buildlocal  :: Maybe Float -> Maybe Int -> Int -> Int
buildlocal Nothing Nothing s = s
buildlocal Nothing (Just e) _ = e
buildlocal (Just p) _ _ = round $ 3600 * p :: Int
-- =======================================================================
buildaltern :: Maybe Float -> Maybe String -> [Int]
buildaltern Nothing Nothing = []
buildaltern Nothing (Just o) = altaux $ words o
buildaltern (Just o) _ = [round $ 3600 * o]
-- =======================================================================
altaux :: [String] -> [Int]
altaux
  = foldr (\ s -> (++) (altaux' (readMaybe s :: Maybe Float))) []
-- =======================================================================
altaux' :: Maybe Float -> [Int]
altaux' Nothing = []
altaux' (Just f) = [round $ 3600 * f]
-- =======================================================================
