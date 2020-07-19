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
buildZlist a w = nub (zgmt ++ zlocal ++ zaltern) where
      zgmt = buildgmt (zulu a) zlocal zaltern
      zlocal = buildlocal (lzone a) (lEnvZone w) (lSysZone w) zaltern
      zaltern = buildaltern (azone a) (aEnvZone w)

buildZlocal :: Uargs -> Uwork -> Int
buildZlocal a w = head $ buildlocal (lzone a) (lEnvZone w) (lSysZone w) []
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
buildlocal Nothing Nothing s a
      | s `elem` a = []
      | otherwise = [s]
-- buildlocal Nothing Nothing _  _ = [0]
buildlocal Nothing (Just e) _ a
      | e `elem` a = []
      | otherwise = [e]
-- buildlocal Nothing (Just e) _ _ = [e]
buildlocal (Just p) _ _ a
      | p' `elem` a = []
      | otherwise = [p'] where
            p' = round $ 3600 * p :: Int
-- buildlocal (Just p) _ _ _ = [round $ 3600 * p]
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
