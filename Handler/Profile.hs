{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Profile where

import Import

import Handler.Forms
import qualified Database.Esqueleto as E
import Database
import Helpers
import qualified Error as E

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
    -- check that the email does not exist
    handle E.emailExists $ do
      runDB $ update uid
        [ UserName =. name
        , UserEmail =. email
        ]
      setMessageI MsgProfileUpdated
  redirect ProfileR