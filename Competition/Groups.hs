module Competition.Groups
( groups
)
where

import Import hiding(group)

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