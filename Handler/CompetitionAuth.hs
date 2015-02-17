{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.CompetitionAuth where

import Import

import Handler.Forms

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
  competition <- runDB $ get404 cid
  let password = competitionPassword competition
  when (pw == password) $ do
    setSession sessionName pw

isCompetitionAuth :: CompetitionId -> Handler Bool
isCompetitionAuth cid = do
  mSession <- lookupSession sessionName
  competition <- runDB $ get404 cid
  let password = competitionPassword competition
  return $ case mSession of
    Just pw -> if pw == password then True else False
    _ -> False