module Handler.Results where

import Import

import Competition.Competition
import Handler.Division
import Database
import DivisionMessages

getResultsR :: CompetitionId -> Handler Html
getResultsR cid = do
  competition <- runDB $ get404 cid
  let lid = competitionLayoutId competition
  holes <- runDB $ selectList
    [HoleLayoutId ==. lid] [Asc HoleNumber]
  players <- playersAndScores cid
  let sortedPlayers = playerSort holes players
  muser <- maybeAuthUser
  defaultLayout $ do
    $(widgetFile "style")
    let headerWidget = $(widgetFile "header")
    $(widgetFile "results")