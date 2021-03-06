{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Notifications where

import Import

import Yesod.Markdown
import Yesod.Form.Bootstrap3

import Handler.Forms

-- how many notifications gets displayed
-- this will also limit the ability to edit old notifications
-- but since they can't be seen anywhere it does not matter
notificationLimit :: Int
notificationLimit = 5

getNotificationsR :: Handler Html
getNotificationsR = do
  aid <- requireAuthId
  time <- liftIO getCurrentTime
  ((_, formWidget), formEnctype) <- notificationForm aid time
  notifications <- runDB $ selectList []
    [Desc NotificationDate, LimitTo notificationLimit]
  defaultLayout $ do
    setTitleI MsgNotifications
    $(widgetFile "notifications")

postNotificationsR :: Handler Html
postNotificationsR = do
  aid <- requireAuthId
  time <- liftIO getCurrentTime
  ((result, _), _) <- notificationForm aid time
  formHandler result $ \notification -> do
    runDB $ insert_ notification
    setMessageI MsgNotificationAdded
  redirect NotificationsR

deleteNotificationR :: NotificationId -> Handler Html
deleteNotificationR nid =
  runDB (delete nid) >> redirect NotificationsR

putNotificationR :: NotificationId -> Handler Html
putNotificationR nid = do
  aid <- requireAuthId
  content <- runInputPost $ ireq markdownField "content"
  time <- liftIO getCurrentTime
  runDB $ update nid
    [ NotificationContent =. content
    , NotificationDate =. time
    , NotificationUser =. aid
    ]
  redirect NotificationsR

notificationForm :: UserId
                 -> UTCTime
                 -> Handler ((FormResult Notification, Widget), Enctype)
notificationForm uid time = do
  mr <- getMessageRender
  let settings = withPlaceholder (mr MsgNotification) $ bfs MsgNotification
  runFormPost $ renderBootstrap3 BootstrapBasicForm $ Notification
    <$> areq markdownField settings Nothing
    <*> pure uid
    <*> pure time
    <* bootstrapSubmit (submitButton MsgAddNotification)
