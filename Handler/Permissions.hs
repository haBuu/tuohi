module Handler.Permissions where

import Import

import Permission

getPermissionsR :: Handler Html
getPermissionsR = do
  admins <- runDB $ selectList [UserAdmin ==. True] [Asc UserName]
  let permissions = [minBound..] :: [Permission]
  defaultLayout $ do
    setTitleI MsgPermissions
    $(widgetFile "permissions")

postPermissionsR :: Handler Html
postPermissionsR = error "Not yet implemented: postPermissionsR"
