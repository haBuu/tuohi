{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Profile where

import Import

import Handler.Forms
import qualified Database.Esqueleto as E
import Database
import Helpers
import qualified Error as E
import Model.User

getProfileR :: Handler Html
getProfileR = do
  (Entity uid user) <- requireAuth
  requireReal user
  activeSignUps <- getActiveSignUps uid
  ((_, formWidget), formEnctype) <- profileForm user
  defaultLayout $ do
    setTitleI MsgProfile
    let signUpWidget = $(widgetFile "signup-modal")
    $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = do
  (Entity uid user) <- requireAuth
  requireReal user
  ((result, _), _) <- profileForm user
  formHandler result $ \(name, email) -> do
    -- check that the email does not exist
    handle E.emailExists $ do
      updateProfile uid name email
      logInfo "Updated profile"
      setMessageI MsgProfileUpdated
  redirect ProfileR