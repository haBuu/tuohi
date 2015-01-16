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
    let headerWidget = $(widgetFile "header")
    $(widgetFile "user")

postUserR :: UserId -> Handler Html
postUserR uid = do
  user <- runDB $ get404 uid
  ((result, _), _) <- runFormPost $ userForm user
  formHandler result $ \res -> do
    runDB $ update uid [UserAdmin =. res]
    setMessage "Käyttäjän tiedot päivitettiin onnistuneesti"
  redirect $ UserR uid