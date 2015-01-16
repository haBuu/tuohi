module Competition.Handicap
( handicap
)
where

import Prelude

handicap :: [Int] -> Int -> Double
handicap roundTotals courseAvgPar =
  let
    totalAvg = avg roundTotals
  in
    totalAvg - fromIntegral courseAvgPar

avg :: [Int] -> Double
avg l = (fromIntegral $ sum l) / (fromIntegral $ length l)