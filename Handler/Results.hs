module Handler.Results where

import Import

import Data.List(nub, sortBy)
import Data.Ord (comparing)
import Control.Monad

import Competition.Competition
import Handler.Division
import Database
import qualified DivisionMessages as D
import qualified Competition.Handicap as H

getResultsR :: CompetitionId -> Handler Html
getResultsR cid = do
  competition <- runDB $ get404 cid
  let lid = competitionLayoutId competition
  layout <- runDB $ get404 lid
  holes <- runDB $ selectList
    [HoleLayoutId ==. lid] [Asc HoleNumber]
  players <- playersAndScores cid
  let sortedPlayers = playerSort holes players
  let date = competitionDate competition
  handicaps <- forM sortedPlayers $ \u@(user, _, r) -> do
    entity <- runDB $ getBy404 $ UniqueUser $ userEmail user
    handicapScores <- handicapScores (entityKey entity)
      (layoutCourseId layout) date
    let hc = H.handicap handicapScores
    return (u, H.countHandicapTotal (r, hc))
  -- sort by handicap results
  let sortedHandicaps = sortBy (comparing snd) handicaps
      filtered = filter (\((_, _, rounds), _) -> not $ dnf rounds) sortedHandicaps
  -- for hamlet template so it will create list only for
  -- divisions which had competitors
  let divisions = map (\d -> (d, D.divisionMsg d)) $ nub $
                    for sortedPlayers $ \(_, d, _) -> d
  defaultLayout $ do
    setTitleI MsgResults
    $(widgetFile "results")