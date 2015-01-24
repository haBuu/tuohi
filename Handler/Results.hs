module Handler.Results where

import Import

import Data.List(nub)

import Competition.Competition
import Handler.Division
import Database
import qualified DivisionMessages as D

getResultsR :: CompetitionId -> Handler Html
getResultsR cid = do
  competition <- runDB $ get404 cid
  let lid = competitionLayoutId competition
  holes <- runDB $ selectList
    [HoleLayoutId ==. lid] [Asc HoleNumber]
  players <- playersAndScores cid
  let sortedPlayers = playerSort holes players
  -- for hamlet template so it will create list only for
  -- divisions which had competitors
  let divisions = map (\d -> (d, D.divisionMsg d)) $ nub $ for sortedPlayers $ \(_, d, _) -> d
  muser <- maybeAuthUser
  defaultLayout $ do
    $(widgetFile "style")
    let headerWidget = $(widgetFile "header")
    $(widgetFile "results")