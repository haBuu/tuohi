module Handler.ExportScores where

import Import

import Data.Char(isSpace)

import Handler.Division
import Database
import Competition.Competition
import Helpers

getExportScoresR :: CompetitionId -> Handler Html
getExportScoresR cid = do
  competition <- runDB $ get404 cid
  -- allow export only from PDGA competitions
  requirePDGACompetition competition
  let lid = competitionLayoutId competition
  holes <- runDB $ selectList
    [HoleLayoutId ==. lid] [Asc HoleNumber]
  players <- playersAndScores cid
  let sortedPlayers = playerSortByDivision holes players
      sortedInDivisions = groupByDivision sortedPlayers
      withPlaces = concatMap (addPlacements holes) sortedInDivisions
      rows = map toRow withPlaces
      rounds = maybe 1 (\(_, _, rounds) -> length rounds) $
        safeHead players
  defaultLayout $ do
    setTitleI MsgPDGAScores
    $(widgetFile "export-scores")

groupByDivision = groupBy (\(_, d1, _) (_, d2, _) -> d1 == d2)

requirePDGACompetition :: Competition -> Handler ()
requirePDGACompetition comp = unless (competitionPdga comp) $
  permissionDeniedI MsgNotPDGACompetition

header :: Int -> Text
header n =
  "Division\tPlace\tFirst name\tLast name\tPDGA\t" ++ rounds ++ "\n"
  where
    rounds = foldl' (\a i -> a ++ "R" ++ tshow i ++ "\t") "" [1..n]

toRow :: (Int, (User, Division, [(Round, [Score])])) -> Text
toRow (place, (user, division, rounds)) =
  tshow division ++ "\t" ++
    tshow place ++ "\t" ++
    firstName ++ "\t" ++
    lastName ++ "\t" ++
    pdga ++ "\t" ++
    scores ++ "\n"
  where
    pdga = maybe "" tshow $ userPdgaNumber user
    (firstName, lastName) = break isSpace $ userName user
    roundResults = map showRound rounds
    scores = foldl' (\a r -> a ++ r ++ "\t") "" roundResults

showRound :: (Round, [Score]) -> Text
showRound r =
  if roundDnf r
    then "999"
    else tshow $ countRoundTotal $ snd r