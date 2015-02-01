{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.User where

import Import
import Handler.Forms
import Database

getUserR :: UserId -> Handler Html
getUserR uid = do
  user <- runDB $ get404 uid
  ((_, formWidget), formEnctype) <- runFormPost $ userForm user
  muser <- maybeAuthUser
  defaultLayout $ do
    mmsg <- getMessage
    setTitle "WeeklyApp"
    $(widgetFile "user")

postUserR :: UserId -> Handler Html
postUserR uid = do
  user <- runDB $ get404 uid
  ((result, _), _) <- runFormPost $ userForm user
  formHandler result $ \(name, email, admin) -> do
    runDB $ update uid
      [ UserName =. name
      , UserEmail =. email
      , UserAdmin =. admin
      ]
    setMessageI MsgUserUpdatedSuccess
  redirect $ UserR uid