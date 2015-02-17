{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Profile where

import Import

import Handler.Forms
import Database

getProfileR :: Handler Html
getProfileR = do
  user <- liftM entityVal requireAuth
  ((_, formWidget), formEnctype) <- profileForm user
  defaultLayout $ do
    setTitleI MsgProfile
    $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = do
  (Entity uid user) <- requireAuth
  ((result, _), _) <- profileForm user
  formHandler result $ \(name, email) -> do
    updateUser uid name email
    setMessageI MsgProfileUpdated
  redirect ProfileR