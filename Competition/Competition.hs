module Competition.Competition
( countPar
, countTotal
, countToPar
, countRoundTotal
, countRoundToPar
, playerSort
)
where

import Prelude
import Data.List(nub, nubBy, find, sortBy)
import Import

countPar :: [Entity Hole] -> Int
countPar = foldl (\n (Entity _ hole) -> n + holePar hole) 0

countTotal :: [(Round, [Entity Score])] -> Int
countTotal = foldl (\n (_, scores) -> n + countRoundTotal scores) 0

countToPar :: [Entity Hole] -> [(Round, [Entity Score])] -> Int
countToPar holes rounds =
  foldl (\n (_, scores) -> n + countRoundToPar holes scores) 0 rounds

countRoundTotal :: [Entity Score] -> Int
countRoundTotal = foldl (\n (Entity _ score) -> n + scoreScore score) 0

countRoundToPar :: [Entity Hole] -> [Entity Score] -> Int
countRoundToPar holes scores =
  let
    toPar = foldl (\n (Entity _ score) -> n + holePar' holes score) 0 scores
  in
    (countRoundTotal scores) -  toPar

holePar' :: [Entity Hole] -> Score -> Int
holePar' holes score = maybe 0 (\(Entity _ hole) -> holePar hole) $
  find (\(Entity hid _) -> hid == scoreHoleId score) holes

-- sorts players according to their current to par result
playerSort :: [Entity Hole] -> [(a, [(Round, [Entity Score])])]
  -> [(a, [(Round, [Entity Score])])]
playerSort holes players = flip sortBy players $
  \(_, rounds1) (_, rounds2) ->
    let
      total1 = countToPar holes rounds1
      total2 = countToPar holes rounds2
    in
      if total1 >= total2 then GT else LT


f1 = func South 0 0
f2 = func 1 0 0
f3 = func North 0 0
f4 = func 2 0 0

data MagnetPole = North | South

data Magnet = Magnet
  { x :: Double
  , y :: Double
  , pole :: MagnetPole
}

instance Num MagnetPole where
  fromInteger 1 = South
  fromInteger 2 = North
  fromInteger _ = error "fromInteger: MagnetPole"

func :: MagnetPole -> Double -> Double -> Magnet
func pole x y = Magnet x y pole