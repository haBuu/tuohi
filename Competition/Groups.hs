module Competition.Groups
( groups
)
where

import Prelude
import Data.Time
import Data.List hiding(group)
import Helpers
import Control.Monad

type GroupSize = Int
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
      ceiling $ (fromIntegral players) / (fromIntegral holes)
  where
    -- i don't know... this just works
    cond = (mod players minGroupSize) == 2

--groups :: Int -> Int -> [(GroupNumber, GroupSize)]
--groups :: Int -> Int -> [GroupSize]
groups :: Int -> Int -> GroupNumbers
groups holes players =
  if holes > 0 && players >= 0
    then group 1 holes players
    else []

--group :: GroupNumber -> Int -> Int -> [(GroupNumber, GroupSize)]
--group :: GroupNumber -> Int -> Int -> [GroupSize]
group :: GroupNumber -> Int -> Int -> GroupNumbers
--group groupNumber 1 players = [(groupNumber, players)]
--group groupNumber 1 players = [players]
group groupNumber 1 players = replicate players groupNumber
--group groupNumber holes players = (groupNumber, size) :
--group groupNumber holes players = size :
group groupNumber holes players = (replicate size groupNumber) ++
  (group (groupNumber + 1) (holes - 1) (players - size))
  where
    size = groupSize holes players

test9 = forM_ [0..70] $ print . groups 9
test10 = forM_ [0..70] $ print . groups 10
test11 = forM_ [0..70] $ print . groups 11
test12 = forM_ [0..70] $ print . groups 12
test13 = forM_ [0..70] $ print . groups 13
test14 = forM_ [0..70] $ print . groups 14
test15 = forM_ [0..70] $ print . groups 15
