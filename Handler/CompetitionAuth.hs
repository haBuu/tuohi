{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.CompetitionAuth where

import Import

import Handler.Forms
import Model.CompetitionState

-- name of the temp session
-- this can be anything
sessionName :: Text
sessionName = "_COMPETITION_AUTH"

getCompetitionAuthR :: CompetitionId -> Handler Html
getCompetitionAuthR cid = do
  ((_, formWidget), formEnctype) <- competitionAuthForm
  defaultLayout $ do
    setTitle "WeeklyApp"
    $(widgetFile "competitionauth")

postCompetitionAuthR :: CompetitionId -> Handler Html
postCompetitionAuthR cid = do
  ((result, _), _) <- competitionAuthForm
  formHandler result $ competitionAuth cid
  -- redirect to ultimate destination what should be
  -- the input page where the user wanted to go
  -- default to HomeR if destination is not set
  redirectUltDest HomeR

competitionAuth :: CompetitionId -> Text -> Handler ()
competitionAuth cid pw = do
  competition <- runDB $ get404 cid
  let password = competitionPassword competition
      state = competitionState competition
  -- allow auth only for competitions that are in state started
  when (pw == password && state == Started) $ do
    setSession sessionName pw

-- returns true if the player has valid auth for the competition
-- or he is logged in and signed up for the competition
isCompetitionAuth :: CompetitionId -> Handler Bool
isCompetitionAuth cid = do
  maid <- maybeAuthId
  mSession <- lookupSession sessionName
  competition <- runDB $ get404 cid
  let password = competitionPassword competition
      state = competitionState competition
      -- does the player have valid competition auth
      compAuth = checkCompetitionAuth mSession password
  -- is the player logged in and signed up for the competition
  loggedInAndSignedUp <- maybe (return False) (checkSignUp cid) maid
  return $ state == Started && (compAuth || loggedInAndSignedUp)

checkCompetitionAuth :: Maybe Text -> Text -> Bool
checkCompetitionAuth mSessionPw competitionPw =
  case mSessionPw of
    Just sessionPw -> sessionPw == competitionPw
    _ -> False

checkSignUp :: CompetitionId -> UserId -> Handler Bool
checkSignUp cid uid = do
  mSignUp <- runDB $ getBy $ UniqueSignUp uid cid
  return $ case mSignUp of
    Nothing -> False
    Just (Entity _ signUp) -> signUpConfirmed signUp
