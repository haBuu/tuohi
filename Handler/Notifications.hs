{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Notifications where

import Import

import Handler.Forms
import Database

getNotificationsR :: Handler Html
getNotificationsR = do
  aid <- requireAuthId
  time <- liftIO getCurrentTime
  ((_, formWidget), formEnctype) <- notificationForm aid time
  notifications <- getNotifications
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
  content <- runInputPost $ ireq textareaField "content"
  time <- liftIO getCurrentTime
  runDB $ update nid
    [ NotificationContent =. content
    , NotificationDate =. time
    , NotificationUser =. aid
    ]
  redirect NotificationsR