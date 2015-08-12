{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Users where

import Import hiding (isSuperAdmin, isAdmin, isUser)

getUsersR :: Handler Html
getUsersR = do
  users <- runDB $ selectList [UserRealUser ==. True] [Asc UserName]
  defaultLayout $ do
    setTitleI MsgUsers
    $(widgetFile "users")

-- helpers
isSuperAdmin :: Entity User -> Bool
isSuperAdmin (Entity _ user) =
  userSuperAdmin user

isAdmin :: Entity User -> Bool
isAdmin (Entity _ user) =
  userAdmin user && not (userSuperAdmin user)

isUser :: Entity User -> Bool
isUser (Entity _ user) =
  not (userAdmin user) && not (userSuperAdmin user)