module Handler.Results where

import Import hiding(for)

import Data.List(nub)
import Database.Persist.Sql (Single, rawSql, unSingle)

import Competition.Competition
import Database
import qualified DivisionMessages as D
import qualified Competition.Handicap as H
import Model.CompetitionState
import Handler.Division
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
    (Just sid, Finished) -> do
      forM finished $ \u@(user, _, _) -> do
        entity <- runDB $ getBy404 $ UniqueUser $ userEmail user
        handicapScores <- handicapSelect sid (entityKey entity) date
        let mhc = H.handicap $ map unSingle handicapScores
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

-- helpers
countOrFilter :: ((a, b, [(Round, [Score])]), Maybe Double)
 -> Maybe ((a, b, [(Round, [Score])]), Double)
countOrFilter (u, mhc) =
  case mhc of
    Just hc -> Just (u, H.countHandicapTotal (thd u, hc))
    Nothing -> Nothing

filterByDivision :: Division -> [(a, Division, [(Round, [Score])])]
  -> [(a, Division, [(Round, [Score])])]
filterByDivision d = filter (\(_, d1, _) -> d == d1)

handicapSelect :: SerieId -> UserId -> Day -> Handler [Single Int]
handicapSelect sid uid day = runDB $ rawSql s [toPersistValue sid, toPersistValue uid, toPersistValue day]
  where s = "SELECT result FROM handicap WHERE sid = ? and uid = ? and date < ?"
