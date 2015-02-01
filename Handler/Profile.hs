{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Profile where

import Import

import Yesod.Auth
import Yesod.Auth.Email

import Handler.Forms
import Database

getProfileR :: Handler Html
getProfileR = do
  muser <- maybeAuthUser
  case muser of
    Just user -> do
      ((_, formWidget), formEnctype) <- runFormPost $ profileForm user
      defaultLayout $ do
        mmsg <- getMessage
        setTitleI MsgProfile
        $(widgetFile "profile")
    -- this can't never happen because this handler
    -- is never reached if the user is not authenticated
    Nothing -> redirect HomeR

postProfileR :: Handler Html
postProfileR = do
  maid <- maybeAuthId
  muser <- maybeAuthUser
  case muser of
    Just user -> do
      ((result, _), _) <- runFormPost $ profileForm user
      formHandler result $ \(name, email) ->
        case maid of
          Just aid -> do
            updateUser aid name email
            setMessageI MsgProfileUpdated
          Nothing -> setMessageI MsgProfileUpdateFailure
    -- see getProfileR
    Nothing -> return ()
  redirect ProfileR