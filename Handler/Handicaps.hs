module Handler.Handicaps where

import Import

import Database
import qualified Competition.Handicap as H
import Helpers

getHandicapsR :: SerieId -> Handler Html
getHandicapsR sid = do
  serie <- runDB $ get404 sid
  users <- runDB $ selectList [] []
  date <- liftIO today
  handicapsAll <- forM users $ \user -> do
    handicapScores_ <- handicapScores (entityKey user) sid date
    return (entityVal user, H.handicap handicapScores_)
  -- filter out handicaps that are Nothing meaning players that
  -- did not have any results in the serie
  let filtered = mapMaybe justHc handicapsAll
      handicaps = sortBy (comparing snd) filtered
  defaultLayout $ do
    setTitleI MsgHandicaps
    $(widgetFile "handicaps")

justHc :: (User, Maybe Double) -> Maybe (User, Double)
justHc (user, mhc) =
  case mhc of
    Just hc -> Just (user, hc)
    Nothing -> Nothing