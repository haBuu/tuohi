module Competition.Competition
( countPar
, countTotal
, countToPar
, countRoundTotal
, countRoundToPar
, playerSort
, playerSortByDivision
, addPlacements
, dnf
)
where

import Import

import Data.List(foldl)

import Model.RoundState
import Handler.Division
import Helpers

countPar :: [Entity Hole] -> Int
countPar = foldl (\n (Entity _ hole) -> n + holePar hole) 0

countTotal :: [(Round, [Score])] -> Int
countTotal = foldl (\n (_, scores) -> n + countRoundTotal scores) 0

countToPar :: [Entity Hole] -> [(Round, [Score])] -> Int
countToPar holes rounds =
  foldl (\n (_, scores) -> n + countRoundToPar holes scores) 0 rounds

countRoundTotal :: [Score] -> Int
countRoundTotal = foldl (\n score -> n + scoreScore score) 0

countRoundToPar :: [Entity Hole] -> [Score] -> Int
countRoundToPar holes scores =
  let
    toPar = foldl (\n score -> n + holePar' holes score) 0 scores
  in
    (countRoundTotal scores) - toPar

holePar' :: [Entity Hole] -> Score -> Int
holePar' holes score = maybe 0 (\(Entity _ hole) -> holePar hole) $
  find (\(Entity hid _) -> hid == scoreHoleId score) holes

-- sorts players according to their current to par result
playerSort :: [Entity Hole] -> [(a, Division, [(Round, [Score])])]
  -> [(a, Division, [(Round, [Score])])]
playerSort holes players = sortBy (byResult holes) players

-- sorts players according to their current to par result and division
-- NOTE: use only for sorting players to groups after first round
playerSortByDivision :: [Entity Hole]
  -> [(a, Division, [(Round, [Score])])]
  -> [(a, Division, [(Round, [Score])])]
playerSortByDivision holes players =
  sortBy byDivision $ sortBy (byResult holes) players

-- sorts players only by to par result
byResult :: [Entity Hole] -> (a, b, [(Round, [Score])])
  -> (a, b, [(Round, [Score])]) -> Ordering
byResult holes (_, _, rounds1) (_, _, rounds2) =
  let
      total1 = countToPar holes rounds1
      total2 = countToPar holes rounds2
    in
      -- if both or neither have dnf then result will determine order
      -- if only one has dnf then that will determine order
      if ((dnf rounds1 && dnf rounds2 -- both dnf
            || not (dnf rounds1) && not (dnf rounds2)) -- neither dnf
            && total1 >= total2) -- order by result
            || (dnf rounds1 && not (dnf rounds2)) -- order by dnf
        then GT else LT

-- sorts players only by division
-- NOTE: does not compare dnf so don't call
-- this with results that have dnf players
-- use only for sorting players to groups after first round
byDivision :: (a, Division, [(Round, [Score])])
  -> (a, Division, [(Round, [Score])]) -> Ordering
byDivision = compare `on` snd3

addPlacements :: [Entity Hole] -> [(a, b, [(Round, [Score])])]
  -> [(Int, (a, b, [(Round, [Score])]))]
addPlacements _ [] = []
addPlacements holes (x:xs) = loop 2 (1, x) xs
  where
    loop _ previous [] = [previous]
    loop idx previous (x:xs) =
      if countToPar holes (thd x) == countToPar holes (thd $ snd previous)
        && not (dnf $ thd x)
        -- placement remains the same
        then [previous] ++ loop (idx + 1) (fst previous, x) xs
        -- next placement from index
        else [previous] ++ loop (idx + 1) (idx, x) xs

dnf :: [(Round, [Score])] -> Bool
dnf rounds = flip any rounds $
  \(round_, _) -> roundState round_ == DidNotFinish