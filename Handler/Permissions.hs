module Handler.Permissions where

import Import
import Model.Permission

getPermissionsR :: Handler Html
getPermissionsR = do
  let permissionTypes = [minBound..] :: [PermissionType]
  users <- runDB $ selectList [] [Asc UserName]
  permissions <- runDB $ selectList [] []
  defaultLayout $ do
    setTitleI MsgPermissions
    $(widgetFile "permissions")

postPermissionsR :: Handler Html
postPermissionsR = error "Not yet implemented: postPermissionsR"

-- helpers
check :: [Entity Permission] -> UserId -> PermissionType -> Bool
check permissions uid permissionType =
  any (match uid permissionType . entityVal) permissions

match :: UserId -> PermissionType -> Permission -> Bool
match uid permissionType (Permission u t) =
  u == uid && t == permissionType