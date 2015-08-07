module Competition.Handicap
( handicap
, countHandicapTotal
)
where

import Import hiding(for)

import Data.List(foldl)

import Competition.Competition
import Helpers

countHandicapTotal :: ([(Round, [Score])], Double) -> Double
countHandicapTotal (rounds, hc) =
  let
    total = foldl (\n (round_, scores) -> n + countRoundTotal round_ scores) 0 rounds
  in
    (fromIntegral total) - (fromIntegral (length rounds) * hc)

handicap :: [(Int, [(Round, [Score])])] -> Maybe Double
handicap [] = Nothing
handicap roundsAndPars =
  let
    toPars = for roundsAndPars $ \(par, rounds) ->
      map (\r -> r - par) $ map (uncurry countRoundTotal) rounds
  in
    Just $ avg $ hcFilter $ concat toPars

avg :: [Int] -> Double
avg [] = 0.0
avg l = (fromIntegral $ sum l) / (fromIntegral $ length l)

hcFilter :: [Int] -> [Int]
hcFilter handicaps =
  case length handicaps of
    0 -> handicaps
    1 -> handicaps
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
    sorted = sort handicaps
    reversed = reverse sorted