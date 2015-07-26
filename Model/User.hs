module Model.User where

import Import

insertUser :: Text -> Text -> Handler UserId
insertUser name email = do
  eitherUser <- runDB $ insertBy $ buildUser name (Just email) True
  return $ case eitherUser of
    Left (Entity uid _) -> uid
    Right uid -> uid

insertUserNoEmail :: Text -> ReaderT SqlBackend Handler UserId
insertUserNoEmail name = do
  eitherUser <- insertBy $ buildUser name Nothing False
  return $ case eitherUser of
    Left (Entity uid _) -> uid
    Right uid -> uid

buildUser :: Text -> Maybe Text -> Bool -> User
buildUser name mEmail real =
  User name mEmail Nothing Nothing False False False real

updateUser :: UserId -> Text -> Text -> Bool -> Handler ()
updateUser uid name email admin = runDB $ update uid
  [ UserName =. name
  , UserEmail =. Just email
  , UserAdmin =. admin
  ]

updateProfile :: UserId -> Text -> Text -> Handler ()
updateProfile uid name email = runDB $ update uid
  [ UserName =. name
  , UserEmail =. Just email
  ]

instance ToJSON User where
  toJSON user = object
    [ "name" .= (show $ userName user)
    , "email" .= (show $ userEmail user)
    ]

requireReal :: User -> Handler ()
requireReal user = unless (userRealUser user) $
  permissionDeniedI MsgUserIsNotRealUser