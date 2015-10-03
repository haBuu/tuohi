{-# LANGUAGE TemplateHaskell #-}
module Model.CompetitionEventLog where

import Import
import Model.EventLog
import Database(DB)

logEvent :: Level -> CompetitionId -> Text -> DB ()
logEvent level cid event = do
  time <- liftIO getCurrentTime
  insert_ $ CompetitionEventLog cid time level event

logError :: CompetitionId -> Text -> DB ()
logError = logEvent Error

logWarn :: CompetitionId -> Text -> DB ()
logWarn = logEvent Warning

logInfo :: CompetitionId -> Text -> DB ()
logInfo = logEvent Info