module Competition.Handicap
( handicap
)
where

import Import
import Data.List(sort)
import Competition.Competition
import Helpers

handicap :: [(Int, [(Round, [Score])])] -> Double
handicap roundsAndPars =
  let
    toPars = for roundsAndPars $ \(par, rounds) ->
      map (\r -> r - par) $ map countRoundTotal $ map snd rounds
  in
    avg $ hcFilter $ concat toPars

avg :: [Int] -> Double
avg l = (fromIntegral $ sum l) / (fromIntegral $ length l)

hcFilter :: [Int] -> [Int]
hcFilter roundTotals =
  case length roundTotals of
    0 -> roundTotals
    1 -> roundTotals
    2 -> drop 1 reversed
    3 -> drop 1 reversed
    4 -> drop 1 reversed
    5 -> drop 2 reversed
    6 -> drop 2 reversed
    7 -> drop 2 reversed
    8 -> drop 3 reversed
    9 -> drop 3 reversed
    _ -> take 7 sorted
  where
    sorted = sort roundTotals
    reversed = reverse sorted