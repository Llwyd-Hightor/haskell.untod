module Untod.LeapSecTab (
    lsSearchByDay
  , lsSearchByTOD
  , leapSecTable
  ) where

import Untod.Data
import Untod.Utils
import Data.Time

data LeapSec = LeapSec {
    day   :: UTCTime ,
    tod   :: Int ,
    count :: Int
} deriving Show

lsEntry :: Int -> Int -> Int -> Int -> Int -> LeapSec
lsEntry y m d = LeapSec (ymdToUTC y m d)

leapSecTable :: [LeapSec]
leapSecTable = [
      lsEntry 2017 1 1 0x000D1E0D68173CC0 27
    , lsEntry 2015 7 1 0x000CF2D54B4FBA80 26
    , lsEntry 2012 7 1 0x000C9CC9A704D840 25
    , lsEntry 2009 1 1 0x000C3870CB9BB600 24
    , lsEntry 2006 1 1 0x000BE251097973C0 23
    , lsEntry 1999 1 1 0x000B1962F9305180 22
    , lsEntry 1997 7 1 0x000AEE3EFA402F40 21
    , lsEntry 1996 1 1 0x000AC34336FECD00 20
    , lsEntry 1994 7 1 0x000A981F380EAAC0 19
    , lsEntry 1993 7 1 0x000A7B70ABEB8880 18
    , lsEntry 1992 7 1 0x000A5EC21FC86640 17
    , lsEntry 1991 1 1 0x000A33C65C870400 16
    , lsEntry 1990 1 1 0x000A1717D063E1C0 15
    , lsEntry 1988 1 1 0x0009DDA69A557F80 14
    , lsEntry 1985 7 1 0x000995D40F517D40 13
    , lsEntry 1983 7 1 0x00095C62D9431B00 12
    , lsEntry 1982 7 1 0x00093FB44D1FF8C0 11
    , lsEntry 1981 7 1 0x00092305C0FCD680 10
    , lsEntry 1980 1 1 0x0008F809FDBB7440  9
    , lsEntry 1979 1 1 0x0008DB5B71985200  8
    , lsEntry 1978 1 1 0x0008BEACE5752FC0  7
    , lsEntry 1977 1 1 0x0008A1FE59520D80  6
    , lsEntry 1976 1 1 0x0008853BAF578B40  5
    , lsEntry 1975 1 1 0x0008688D23346900  4
    , lsEntry 1974 1 1 0x00084BDE971146C0  3
    , lsEntry 1973 1 1 0x00082F300AEE2480  2
    , lsEntry 1972 7 1 0x000820BA9811E240  1
    , lsEntry 0000 1 1 0x0000000000000000  0
      ]

lsSearchByDay :: TickMode -> UTCTime -> Int
lsSearchByDay UTC d = lsSearchByDay' d 0
lsSearchByDay _ _ = 0

lsSearchByDay' :: UTCTime -> Int -> Int
lsSearchByDay' d l = r where
      d' = addUTCTime (fromIntegral l) d
      x = count $ head $ filter (\x -> d' >= day x) leapSecTable
      r = if l == x
            then l
            else lsSearchByDay' d x

lsSearchByTOD:: TickMode -> Int -> Int
lsSearchByTOD UTC d = lsSearchByTOD' d 0
lsSearchByTOD _ _ = 0

lsSearchByTOD' :: Int -> Int -> Int
lsSearchByTOD' d l = r where
      x = count $ head $ filter (\z -> tryls z d l) leapSecTable
      r = if l == x
            then l
            else lsSearchByTOD' d x

tryls :: LeapSec -> Int -> Int -> Bool 
tryls z d l = (d + l) >= tod z - 1000000 * count z
