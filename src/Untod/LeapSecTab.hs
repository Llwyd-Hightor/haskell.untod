module Untod.LeapSecTab (
    lsSearchByDay
  , lsSearchByTOD 
  ) where

import Untod.Data
import Data.Time

data LeapSec = LeapSec {
       day   :: Day     ,
       tod   :: Integer ,
       count :: Int
} deriving Show

leapSecTable = [
      LeapSec (fromGregorian 2017 1 1) 0x000D1E0D68173CC0 27
    , LeapSec (fromGregorian 2015 7 1) 0x000CF2D54B4FBA80 26
    , LeapSec (fromGregorian 2012 7 1) 0x000C9CC9A704D840 25
    , LeapSec (fromGregorian 2009 1 1) 0x000C3870CB9BB600 24
    , LeapSec (fromGregorian 2006 1 1) 0x000BE251097973C0 23
    , LeapSec (fromGregorian 1999 1 1) 0x000B1962F9305180 22
    , LeapSec (fromGregorian 1997 7 1) 0x000AEE3EFA402F40 21
    , LeapSec (fromGregorian 1996 1 1) 0x000AC34336FECD00 20
    , LeapSec (fromGregorian 1994 7 1) 0x000A981F380EAAC0 19
    , LeapSec (fromGregorian 1993 7 1) 0x000A7B70ABEB8880 18
    , LeapSec (fromGregorian 1992 7 1) 0x000A5EC21FC86640 17
    , LeapSec (fromGregorian 1991 1 1) 0x000A33C65C870400 16
    , LeapSec (fromGregorian 1990 1 1) 0x000A1717D063E1C0 15
    , LeapSec (fromGregorian 1988 1 1) 0x0009DDA69A557F80 14
    , LeapSec (fromGregorian 1985 7 1) 0x000995D40F517D40 13
    , LeapSec (fromGregorian 1983 7 1) 0x00095C62D9431B00 12
    , LeapSec (fromGregorian 1982 7 1) 0x00093FB44D1FF8C0 11
    , LeapSec (fromGregorian 1981 7 1) 0x00092305C0FCD680 10
    , LeapSec (fromGregorian 1980 1 1) 0x0008F809FDBB7440  9
    , LeapSec (fromGregorian 1979 1 1) 0x0008DB5B71985200  8
    , LeapSec (fromGregorian 1978 1 1) 0x0008BEACE5752FC0  7
    , LeapSec (fromGregorian 1977 1 1) 0x0008A1FE59520D80  6
    , LeapSec (fromGregorian 1976 1 1) 0x0008853BAF578B40  5
    , LeapSec (fromGregorian 1975 1 1) 0x0008688D23346900  4
    , LeapSec (fromGregorian 1974 1 1) 0x00084BDE971146C0  3
    , LeapSec (fromGregorian 1973 1 1) 0x00082F300AEE2480  2
    , LeapSec (fromGregorian 1972 7 1) 0x000820BA9811E240  1
    , LeapSec (fromGregorian 0000 1 1) 0x0000000000000000  0
      ]

lsSearchByDay:: TickMode -> Day -> Int
lsSearchByDay UTC d = count ls where
      ls = head $ filter (\x -> d >= day x) leapSecTable
lsSearchByDay _ d = 0

lsSearchByTOD:: TickMode -> Integer -> Int
lsSearchByTOD UTC d = count ls where
      ls = head $ filter (\x -> d >= tod x) leapSecTable
lsSearchByTOD _ d = 0
