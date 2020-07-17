module Untod.Utils
      where

import Data.Time

ymdToUTC :: Int -> Int -> Int -> UTCTime 
ymdToUTC y m d = UTCTime {
    utctDay = fromGregorian (toInteger y) m d
  , utctDayTime = 0.0
}
