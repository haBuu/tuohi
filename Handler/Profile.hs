{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Profile where

import Import

import Handler.Forms
import qualified Database.Esqueleto as E
import Database
import Helpers

getProfileR :: Handler Html
getProfileR = do
  (Entity uid user) <- requireAuth
  activeSignUps <- getActiveSignUps uid
  ((_, formWidget), formEnctype) <- profileForm user
  defaultLayout $ do
    setTitleI MsgProfile
    let signUpWidget = $(widgetFile "signup-modal")
    $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = do
  (Entity uid user) <- requireAuth
  ((result, _), _) <- profileForm user
  formHandler result $ \(name, email) -> do
    updateUser uid name email
    setMessageI MsgProfileUpdated
  redirect ProfileR