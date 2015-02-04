module Helpers
( for
, showDay
, textualScore
, safeHead
, thd
, language
)
where

import Prelude
import Data.Time
import Data.List(find)
import Data.Text(Text)
import qualified Data.Text

thd :: (t1,t2,t3) -> t3
thd (_, _, x) = x

for :: [a] -> (a -> b) -> [b]
for = flip map

showDay :: Day -> String
showDay date =
  (show d) ++ "." ++ (show m) ++ "." ++ (show y)
  where
    (y,m,d) = toGregorian date

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

textualScore :: Int -> Int -> Text
textualScore _ 1 = "hole-in-one"
textualScore par score =
  case score - par of
    -4 -> "condor"
    -3 -> "albatross"
    -2 -> "eagle"
    -1 -> "birdie"
    0 -> "par"
    1 -> "bogey"
    2 -> "double-bogey"
    3 -> "triple-bogey"
    _ -> "other"

-- choose first 2 letter language
language :: [Text] -> String
language langs = maybe "en" Data.Text.unpack $
  find (\l -> (Data.Text.length l) == 2) langs