{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Users where

import Import
import Database

getUsersR :: Handler Html
getUsersR = do
  users <- runDB $ selectList [] [Asc UserName]
  muser <- maybeAuthUser
  defaultLayout $ do
    setTitle "WeeklyApp"
    $(widgetFile "users")

-- helpers
superAdmin :: Entity User -> Bool
superAdmin (Entity _ user) =
  userSuperAdmin user

admin :: Entity User -> Bool
admin (Entity _ user) =
  userAdmin user && not (userSuperAdmin user)

user :: Entity User -> Bool
user (Entity _ user) =
  not (userAdmin user) && not (userSuperAdmin user)