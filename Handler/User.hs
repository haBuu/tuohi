{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.User where

import Import
import Handler.Forms

getUserR :: UserId -> Handler Html
getUserR uid = do
  user <- runDB $ get404 uid
  ((_, formWidget), formEnctype) <- runFormPost $ userForm user
  defaultLayout $ do
    setTitleI MsgUser
    $(widgetFile "user")

postUserR :: UserId -> Handler Html
postUserR uid = do
  user <- runDB $ get404 uid
  ((result, _), _) <- runFormPost $ userForm user
  formHandler result $ \(name, email, admin, permissions) -> do
    runDB $ update uid
      [ UserName =. name
      , UserEmail =. email
      , UserAdmin =. admin
      , UserPermissions =. permissions
      ]
    setMessageI MsgUserUpdatedSuccess
  redirect $ UserR uid