module Handler.Results where

import Import hiding(for)

import Data.List(nub)

import Competition.Competition
import Database
import qualified DivisionMessages as D
import qualified Competition.Handicap as H
import Model.CompetitionState
import Helpers

getResultsR :: CompetitionId -> Handler Html
getResultsR cid = do
  competition <- runDB $ get404 cid
  let lid = competitionLayoutId competition
  holes <- runDB $ selectList
    [HoleLayoutId ==. lid] [Asc HoleNumber]
  players <- playersAndScores cid
  let sortedPlayers = playerSort holes players
  let date = competitionDate competition

  -- <handicap>
  -- filter out dnfs
  let finished = filter (\(_, _, rounds) -> not $ dnf rounds) players
      state = competitionState competition
      msid = competitionSerieId competition
  -- calculate handicaps if the competition is finished
  -- and belongs to a serie
  handicapsAll <- case (msid, state) of
    (Just sid, Finished) ->
      forM finished $ \u@(user, _, _) -> do
        entity <- runDB $ getBy404 $ UniqueUser $ userEmail user
        handicapScores_ <- handicapScores (entityKey entity) sid date
        let mhc = H.handicap handicapScores_
        return (u, mhc)
    _ -> return []
  -- filter out handicaps that are Nothing and count to
  -- total to handicap results for rest
  let handicaps = mapMaybe countOrFilter handicapsAll
  -- sort by handicap results
  let sortedHandicaps = sortBy (comparing snd) handicaps
  -- </handicap>

  -- for hamlet template so it will create list only for
  -- divisions which had competitors
  let divisions = map (\d -> (d, D.divisionMsg d)) $ nub $
                    for sortedPlayers $ \(_, d, _) -> d
  defaultLayout $ do
    setTitleI MsgResults
    $(widgetFile "results")

-- helper
countOrFilter :: ((a, b, [(Round, [Score])]), Maybe Double)
 -> Maybe ((a, b, [(Round, [Score])]), Double)
countOrFilter (u, mhc) =
  case mhc of
    Just hc -> Just (u, H.countHandicapTotal (thd u, hc))
    Nothing -> Nothing