module Handler.Handicaps where

import Import
import Data.Time
import Control.Monad(forM)
import Data.Ord(comparing)
import Data.List(sortBy)

import Database
import qualified Competition.Handicap as H

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
  let handicaps = sortBy (comparing snd) handicapsAll
  -- </handicap>
  defaultLayout $ do
    setTitleI MsgHandicaps
    $(widgetFile "handicaps")