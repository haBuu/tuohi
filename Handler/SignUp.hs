{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.SignUp where

import Import

import Handler.Forms
import Handler.Division
import qualified Database.Esqueleto as E
import Database
import DivisionMessages

getSignUpR :: CompetitionId -> Handler Html
getSignUpR cid = do
  -- if the competition does not exist return 404
  competition <- runDB $ get404 cid
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

postSignUpR :: CompetitionId -> Handler Html
postSignUpR cid = do
  muser <- maybeAuthUser
  ((result, _), _) <- case muser of
    -- user is logged in
    Just user -> runFormPost $ signUpFormLoggedIn cid user
    -- user is not logged in
    Nothing -> runFormPost $ signUpForm cid
  formHandler result $ \(name, email, division, _) -> do
    msid <- maybeInsertSignUp cid name email division
    case msid of
      Just _ -> setMessageI MsgSignUpSuccess
      Nothing -> setMessageI MsgSignUpFail
  redirect $ SignUpR cid