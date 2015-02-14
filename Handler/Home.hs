{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Handler.CompetitionState
import Database
import Data.Time.LocalTime

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  tz <- liftIO getCurrentTimeZone
  competitions <- runDB $ selectList [] [Desc CompetitionDate]
  let finished = filter isFinished competitions
  notifications <- getNotifications
  series <- runDB $ selectList [] [Asc SerieName]
  defaultLayout $ do
    setTitle "WeeklyApp"
    let languageWidget = $(widgetFile "language")
    $(widgetFile "home")

-- how many finished competitions gets displayed
-- in the home page
finishedLimit :: Int
finishedLimit = 5