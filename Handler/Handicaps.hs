module Handler.Handicaps where

import Import
import Data.Time
import Data.Ord(comparing)
import Data.Maybe(mapMaybe)

import Database
import qualified Competition.Handicap as H
import Helpers

getHandicapsR :: SerieId -> Handler Html
getHandicapsR sid = do
  serie <- runDB $ get404 sid
  -- <handicap>
  -- not tested yet
  date <- liftIO today
  users <- runDB $ selectList [] []
  handicapsAll <- forM users $ \user -> do
    handicapScores <- handicapScores (entityKey user) sid date
    return (entityVal user, H.handicap handicapScores)
  -- filter out handicaps that are Nothing meaning players that
  -- did not have any results in the serie
  let filtered = mapMaybe justHc handicapsAll
      handicaps = sortBy (comparing snd) filtered
  -- </handicap>
  defaultLayout $ do
    setTitleI MsgHandicaps
    $(widgetFile "handicaps")

justHc (user, mhc) =
  case mhc of
    Just hc -> Just (user, hc)
    Nothing -> Nothing