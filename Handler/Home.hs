{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Model.CompetitionState
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
  maid <- maybeAuthId
  activeSignUps <- maybe (return []) getActiveSignUps maid
  mactiveRound <- maybe (return Nothing) getActiveRound maid
  tz <- liftIO getCurrentTimeZone
  activeCompetitions <- runDB $ selectList
    [ CompetitionState !=. Finished
    , CompetitionPrivate !=. True
    ]
    [Desc CompetitionDate]
  finished <- runDB $ selectList
    [ CompetitionState ==. Finished
    , CompetitionPrivate !=. True
    ]
    [Desc CompetitionDate, LimitTo (finishedLimit + 1)]
  notifications <- runDB $ selectList []
    [Desc NotificationDate, LimitTo notificationLimit]
  series <- runDB $ selectList [] [Asc SerieName]
  defaultLayout $ do
    setTitle "WeeklyApp"
    let languageWidget = $(widgetFile "language")
    let signUpWidget = $(widgetFile "signup-modal")
    $(widgetFile "home")

-- how many finished competitions gets displayed
finishedLimit :: Int
finishedLimit = 2

-- how many notifications gets displayed
notificationLimit :: Int
notificationLimit = 3

-- helper for hamlet
-- returns true if competition is found in signups
findMatch :: CompetitionId
  -> [(E.Value SignUpId, E.Value CompetitionId, E.Value Text, E.Value Day)]
  -> Bool
findMatch cid signups = flip any signups $
  \(_, E.Value signUpCid, _, _) -> cid == signUpCid
