{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.CompetitionAuth where

import Import

import Data.Time
import Data.Text
import Control.Monad

import Handler.Forms
import Database

-- name of the temp session
-- this can be anything
sessionName :: Text
sessionName = "_COMPETITION_AUTH"

getCompetitionAuthR :: CompetitionId -> Handler Html
getCompetitionAuthR cid = do
  ((_, formWidget), formEnctype) <- tempAuthForm
  defaultLayout $ do
    setTitle "WeeklyApp"
    $(widgetFile "competitionauth")

postCompetitionAuthR :: CompetitionId -> Handler Html
postCompetitionAuthR cid = do
  ((result, _), _) <- tempAuthForm
  formHandler result $ competitionAuth cid
  -- redirect to ultimate destination what should be
  -- the input page where the user wanted to go
  -- default to HomeR if destination is not set
  redirectUltDest HomeR


competitionAuth :: CompetitionId -> Text -> Handler ()
competitionAuth cid pw = do
  -- session can be created only with the current password
  competition <- runDB $ get404 cid
  let password = competitionPassword competition
  when (pw == password) $ do
    setSession sessionName pw

isCompetitionAuth :: CompetitionId -> Handler Bool
isCompetitionAuth cid = do
  competition <- runDB $ get404 cid
  let password = competitionPassword competition
  mSession <- lookupSession sessionName
  return $ case mSession of
    Just pw -> if pw == password then True else False
    _ -> False