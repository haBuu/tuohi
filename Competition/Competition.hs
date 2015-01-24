module Competition.Competition
( countPar
, countTotal
, countToPar
, countRoundTotal
, countRoundToPar
, playerSort
, addPlacements
, dnf
)
where

import Prelude
import Data.List(nub, nubBy, find, sortBy)
import Import

import Data.List

import Handler.RoundState
import Handler.Division

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
    (countRoundTotal scores) -  toPar

holePar' :: [Entity Hole] -> Score -> Int
holePar' holes score = maybe 0 (\(Entity _ hole) -> holePar hole) $
  find (\(Entity hid _) -> hid == scoreHoleId score) holes

-- sorts players according to their current to par result
playerSort :: [Entity Hole] -> [(a, b, [(Round, [Score])])]
  -> [(a, b, [(Round, [Score])])]
playerSort holes players = flip sortBy players $
  \(_, _, rounds1) (_, _, rounds2) ->
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

addPlacements :: [Entity Hole] -> [(a, b, [(Round, [Score])])]
  -> [(Int, (a, b, [(Round, [Score])]))]
addPlacements holes [] = []
addPlacements holes (x:xs) = loop 2 (1, x) xs
  where
    loop index previous [] = [previous]
    loop index previous (x:xs) =
      if countToPar holes (thd x) == countToPar holes (thd $ snd previous)
        && not (dnf $ thd x)
        -- placement remains the same
        then [previous] ++ loop (index + 1) (fst previous, x) xs
        -- next placement from index
        else [previous] ++ loop (index + 1) (index, x) xs

dnf :: [(Round, [Score])] -> Bool
dnf rounds = flip any rounds $
  \(round_, _) -> roundState round_ == DidNotFinish