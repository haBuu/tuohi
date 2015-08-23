module Helpers
( for
, showDay
, textualScore
, safeHead
, thd, snd3, fst3
, language
, showTime
, today
, showHandicap
)
where

import Prelude
import Data.Time
import Data.List(find)
import Data.Text(Text)
import Text.Printf
import qualified Data.Text

today :: IO Day
today = getCurrentTime
  >>= return . utctDay

thd :: (t1,t2,t3) -> t3
thd (_, _, x) = x

fst3 :: (t1,t2,t3) -> t1
fst3 (x, _, _) = x

snd3 :: (t1,t2,t3) -> t2
snd3 (_, x, _) = x

for :: [a] -> (a -> b) -> [b]
for = flip map

showDay :: Day -> String
showDay date =
  (show d) ++ "." ++ (show m) ++ "." ++ (show y)
  where
    (y,m,d) = toGregorian date

showTime :: TimeZone -> UTCTime -> String
showTime tz time =
  let
    local = utcToLocalTime tz time
    day = showDay $ localDay local
  in
    day ++ " " ++ formatTime defaultTimeLocale "%H:%M" local

showHandicap :: Double -> String
showHandicap = trimZeroes . printf "%.2f"

-- remove multiple trailing zeroes
-- e.g.
-- 1.00 -> 1.0
-- 1.30 -> 1.3
trimZeroes :: String -> String
trimZeroes str =
  let
    integer = takeWhile (/= '.') str
    decimals = dropWhile (/= '.') str
    trimmed = reverse $ dropWhile (== '0') $ reverse decimals
    trimmedDecimals = if trimmed == "." then ".0" else trimmed
  in
    integer ++ trimmedDecimals

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