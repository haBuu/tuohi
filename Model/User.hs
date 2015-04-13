module Model.User where

import Import

insertUser :: Text -> Text -> Handler UserId
insertUser name email = do
  eitherUser <- runDB $ insertBy $ buildUser name email
  return $ case eitherUser of
    Left (Entity uid _) -> uid
    Right uid -> uid

buildUser :: Text -> Text -> User
buildUser name email =
  User name email Nothing Nothing False False False

updateUser :: UserId -> Text -> Text -> Bool -> Handler ()
updateUser uid name email admin = runDB $ update uid
  [ UserName =. name
  , UserEmail =. email
  , UserAdmin =. admin
  ]

updateProfile :: UserId -> Text -> Text -> Handler ()
updateProfile uid name email = runDB $ update uid
  [ UserName =. name
  , UserEmail =. email
  ]

instance ToJSON User where
  toJSON user = object
    [ "name" .= (show $ userName user)
    , "email" .= (show $ userEmail user)
    ]