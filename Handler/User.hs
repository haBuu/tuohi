{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.User where

import Import
import Handler.Forms
import Database
import qualified Error as E
import Model.Permission
import Model.User

getUserR :: UserId -> Handler Html
getUserR uid = do
  user <- runDB $ get404 uid
  let verified = userVerified user
  let types = [minBound..]
  permissions <- runDB $ selectList
    [PermissionUserId ==. uid] [Asc PermissionType]
  ((_, formWidget), formEnctype) <- runFormPost $ userForm user
  defaultLayout $ do
    setTitleI MsgUser
    $(widgetFile "user")

postUserR :: UserId -> Handler Html
postUserR uid = do
  user <- runDB $ get404 uid
  ((result, _), _) <- runFormPost $ userForm user
  formHandler result $ \(name, email, admin) -> do
    -- check that the email does not exist
    handle E.emailExists $ do
      updateUser uid name email admin
      setMessageI MsgUserUpdatedSuccess
  redirect $ UserR uid