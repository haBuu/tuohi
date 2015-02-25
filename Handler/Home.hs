{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Handler.CompetitionState
import Database
import Data.Time.LocalTime
import qualified Database.Esqueleto as E
import Helpers

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  activeSignUps <- maybeAuthId >>= maybe (return []) getActiveSignUps
  tz <- liftIO getCurrentTimeZone
  competitions <- runDB $ selectList [] [Desc CompetitionDate]
  let finished = filter isFinished competitions
  notifications <- getNotifications
  series <- runDB $ selectList [] [Asc SerieName]
  defaultLayout $ do
    setTitle "WeeklyApp"
    let languageWidget = $(widgetFile "language")
    let signUpWidget = $(widgetFile "signup-modal")
    $(widgetFile "home")

-- how many finished competitions gets displayed
-- in the home page
finishedLimit :: Int
finishedLimit = 2

-- helper for hamlet
-- returns true if competition is found in signups
findMatch :: CompetitionId
  -> [(E.Value SignUpId, E.Value CompetitionId, E.Value Text, E.Value Day)]
  -> Bool
findMatch cid signups = flip any signups $
  \(_, E.Value signUpCid, _, _) -> cid == signUpCid