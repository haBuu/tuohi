{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Handicaps where

import Import hiding (for)

import Data.List (nub)
import Database.Persist.Sql (Single, rawSql, unSingle)

import Database
import Model.CompetitionState
import qualified Competition.Handicap as H
import Helpers

getHandicapsR :: SerieId -> Handler Html
getHandicapsR sid = do
  serie <- runDB $ get404 sid
  -- every round in the serie
  handicaps <- handicapSelect sid
  -- players that have handicap
  let players = nub $ map (\(uid, name, _) -> (unSingle uid, unSingle name)) handicaps
  -- calculate handicap for every player
  let handicapsNotSorted = for players $ \(userId, name) ->
        let
          playerHandicaps = filter (\(uid, _, _) -> unSingle uid == userId) handicaps
        in
          (name, H.handicap $ map (unSingle . thd) playerHandicaps)

  -- sort players by handicap and remove if handicap is nothing
  let handicaps = sortBy (comparing snd) $ mapMaybe justHc handicapsNotSorted
  defaultLayout $ do
    setTitleI MsgHandicaps
    $(widgetFile "handicaps")

-- helper for removing users that do not have handicap
justHc :: (Text, Maybe Double) -> Maybe (Text, Double)
justHc (user, Just hc) = Just (user, hc)
justHc (_, Nothing) = Nothing

handicapSelect :: SerieId -> Handler [(Single UserId, Single Text, Single Int)]
handicapSelect sid = runDB $ rawSql s [toPersistValue sid]
  where s = "SELECT uid, name, result FROM handicap WHERE sid = ?"
