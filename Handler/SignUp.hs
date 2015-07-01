module Handler.SignUp where

import Import

import Handler.Forms
import qualified Database.Esqueleto as E
import Database
import DivisionMessages
import Model.CompetitionState

deleteSignUpR :: SignUpId -> Handler Html
deleteSignUpR sid = do
  (Entity uid _) <- requireAuth
  runDB $ do
    signup <- get404 sid
    -- check that the user is trying to delete his own sign up
    unless (signUpUserId signup == uid) $ permissionDeniedI MsgForbidden
    competition <- get404 $ signUpCompetitionId signup
    -- allow deleting of signup only from competitions
    -- that are in state Init
    when (competitionState competition == Init) $ delete sid
  redirect HomeR

getSignUpsR :: CompetitionId -> Handler Html
getSignUpsR cid = do
  -- if the competition does not exist return 404
  competition <- runDB $ get404 cid
  requireInit competition
  full <- competitionFull cid
  muser <- maybeAuthUser
  ((_, formWidget), formEnctype) <- case muser of
    -- user is logged in
    Just user -> runFormPost $ signUpFormLoggedIn cid user
    -- user is not logged in
    Nothing -> runFormPost $ signUpForm cid
  signups <- signUpsWithName cid
  defaultLayout $ do
    $(widgetFile "signup")

-- TODO: check that the division given is allowed in the competition
postSignUpsR :: CompetitionId -> Handler Html
postSignUpsR cid = do
  competition <- runDB $ get404 cid
  requireInit competition
  muser <- maybeAuthUser
  ((result, _), _) <- case muser of
    -- user is logged in
    Just user -> runFormPost $ signUpFormLoggedIn cid user
    -- user is not logged in
    Nothing -> runFormPost $ signUpForm cid
  formHandler result $ \(name, email, division, pw) -> do
    -- check that the user gave correct password
    if (competitionPassword competition == pw)
      then do
        let checkFull = True
        msid <- maybeInsertSignUp checkFull cid name email division
        case msid of
          Just _ -> setMessageI MsgSignUpSuccess
          Nothing -> setMessageI MsgSignUpFail
      else
        setMessageI MsgSignUpWrongPassword
  redirect $ SignUpsR cid

requireInit :: Competition -> Handler ()
requireInit comp = unless (competitionState comp == Init) $
  permissionDeniedI MsgSignUpNotAllowed

count d signups =
  length $ filter (\(_, _, E.Value d1, _) -> d == d1) signups