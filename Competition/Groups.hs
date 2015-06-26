module Competition.Groups
( groups, divide
)
where

import Import hiding(group, for)

import qualified Data.List as L

import Handler.Division
import Helpers

type GroupNumber = Int
type GroupNumbers = [GroupNumber]

minGroupSize :: Int
minGroupSize = 4

maxGroupSize :: Int
maxGroupSize = 5

-- if condition is true decrease size by one
decreaseSize :: Bool -> Int -> Int
decreaseSize cond size = if cond then size - 1 else size

groupSize :: Int -> Int -> Int
groupSize holes players =
  if players <= maxGroupSize
    then players
    else
      -- size can't be smaller than min group size
      max (decreaseSize cond minGroupSize) $
      ceiling $ ((fromIntegral players) / (fromIntegral holes) :: Double)
  where
    -- i don't know... this just works
    cond = (mod players minGroupSize) == 2

groups :: Int -> Int -> GroupNumbers
groups holes players =
  if holes > 0 && players >= 0
    then group 1 holes players
    else []

group :: GroupNumber -> Int -> Int -> GroupNumbers
group groupNumber 1 players = replicate players groupNumber
group groupNumber holes players = (replicate size groupNumber) ++
  (group (groupNumber + 1) (holes - 1) (players - size))
  where
    size = groupSize holes players

-- internal helper type alias
type Player = (UserId, Division, [(Round, [Score])])

-- TODO: ugly
-- divide players to groups by divisions
-- dividing the groups is done in three stages
-- first we assing full groups from each division
-- meaning that one group will get players only from
-- the same division
-- then we assing partial groups starting from the largest
-- remaining division etc.
-- in the third stage we randomly assign any remaining players
-- to positions that are left
divide :: Int -> [Player] -> [(Int, Player)]
divide holes players =
  let
    -- list of group positions
    -- e.g. [[1,1,1,1],[2,2,2]]
    -- would mean 4 positions in group 1 and 3 in group 2
    groups_ = L.group $ groups holes $ length players
    -- list of divisions of players
    -- e.g [[MPO players],[FPO players]]
    grouped = groupByDivision players
    -- first stage of groups
    -- rest is players and remaining is positions
    (divided, rest, remaining) = loop True [] [] groups_ grouped
    -- sort for second stage (players)
    -- largest division goes first etc.
    restInOrder = L.reverse $ L.sortBy (compare `on` length) $
      groupByDivision rest
    -- sort for second stage (positions)
    -- largest group goes first etc.
    remaingInOrder = L.reverse $ L.sortBy (compare `on` length) remaining
    -- second stage of groups
    (restDivided, deadLast, lastRemaining) = loop False [] []
      remaingInOrder restInOrder
    -- third stage of groups
    -- there should not be anything left after this
    deadLastDivided = zip (concat lastRemaining) deadLast
  in
    divided ++ restDivided ++ deadLastDivided

-- helper
groupByDivision :: [Player] -> [[Player]]
groupByDivision = L.groupBy (\(_, d1, _) (_, d2, _) -> d1 == d2)

-- helper
-- divideGroups does actual assigning of the groups
-- this just loops through divisions and combines divided
-- players together
loop :: Bool -> [(Int, Player)] -> [Player] -> [GroupNumbers]
  -> [[Player]] -> ([(Int, Player)], [Player], [GroupNumbers])
loop onlyFull divided rest remaining [] = (divided, rest, remaining)
loop onlyFull divided rest remaining (division:others) =
  let
    (newDivided, newRrest, newRemaining) = divideGroups onlyFull
      remaining division []
  in
    loop onlyFull (divided ++ newDivided) (rest ++ newRrest)
      newRemaining others

-- takes in one division of players and divides one group of players at
-- a time and returns players who got a group, players who didn't
-- and remaining positions that has not been given to anyone
-- if onlyFull is true assigns only full groups
-- it is used to guarantee that we have little as possible of
-- players in mixed groups
divideGroups :: Bool -> [GroupNumbers] -> [Player] -> [(Int, Player)]
  -> ([(Int, Player)], [Player], [GroupNumbers])
divideGroups _ remaining [] done = (done, [], remaining)
divideGroups _ [] players done = (done, players, [])
divideGroups onlyFull (largest:rest) players done =
  let
    -- size of the group
    size = min (length largest) (length players)
    -- group number
    -- this won't ever default to one
    n = fromMaybe 1 $ safeHead largest
    -- remaining positions in case that all of the
    -- positions were not given
    remaining = if size < length largest
                  -- only some were given
                  then rest ++ [drop size largest]
                  -- all were given
                  else rest
  in
    -- if not enough remaining players return assigned
    -- players, not assgined and remaining positions
    -- onlyFull can be used to assign only partial groups
    if (not onlyFull || length players >= length largest)
      then divideGroups onlyFull remaining (drop size players)
        (done ++ (map (\p -> (n, p)) $ take size players))
      -- all groups were given that were supposed to
      else (done, players, (largest:rest))