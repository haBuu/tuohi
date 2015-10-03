module Handler.CompetitionEventLog where

import Import

import Data.Time.LocalTime
import Helpers

getCompetitionEventLogR :: CompetitionId -> Handler Html
getCompetitionEventLogR cid = do
  events <- runDB $ selectList
    [ CompetitionEventLogCompetitionId ==. cid
    ]
    [Asc CompetitionEventLogTime]
  tz <- liftIO getCurrentTimeZone
  defaultLayout $ do
    setTitleI MsgCompetitionEventLog
    $(widgetFile "competition-eventlog")