module Handler.EventLog where

import Import

import qualified Database.Esqueleto as E
import Database.Esqueleto((^.))

import Data.Time.LocalTime
import Helpers

-- how many events gets displayed in the page
eventLogDisplayLimit :: Int64
eventLogDisplayLimit = 100

getEventLogR :: Handler Html
getEventLogR = do
  events <- eventLogWithNames
  tz <- liftIO getCurrentTimeZone
  defaultLayout $ do
    setTitleI MsgEventLog
    $(widgetFile "event-log")

eventLogWithNames :: Handler [(E.Value Text, Entity EventLog)]
eventLogWithNames = runDB $ E.select $
  E.from $ \(eventLog, user) -> do
    E.where_ $ eventLog ^. EventLogUserId E.==. user ^. UserId
    E.orderBy [E.desc (eventLog ^. EventLogTime)]
    E.limit eventLogDisplayLimit
    return
      ( user ^. UserName
      , eventLog
      )