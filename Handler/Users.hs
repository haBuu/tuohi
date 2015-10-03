{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Users where

import Import hiding (isSuperAdmin, isAdmin, isUser)

getUsersR :: Handler Html
getUsersR = do
  users <- runDB $ selectList [UserRealUser ==. True] [Asc UserName]
  defaultLayout $ do
    setTitleI MsgUsers
    addScript $ StaticR js_jets_min_js
    $(widgetFile "users")
    search

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

search :: Widget
search = toWidget [julius|
  var jets = new Jets({
    searchTag: '#player-search',
    contentTag: '#player-content'
  });
|]