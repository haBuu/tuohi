module Handler.Permissions where

import Import

getPermissionsR :: Handler Html
getPermissionsR = do
  permissions <- runDB $ selectList [] [Asc PermissionUserId]
  defaultLayout $ do
    setTitleI MsgPermissions
    $(widgetFile "permissions")

postPermissionsR :: Handler Html
postPermissionsR = error "Not yet implemented: postPermissionsR"
