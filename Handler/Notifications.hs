{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Notifications where

import Import

import Handler.Forms
import Database

getNotificationsR :: Handler Html
getNotificationsR = do
  notifications <- getNotifications
  maid <- maybeAuthId
  case maid of
    Just aid -> do
      time <- liftIO getCurrentTime
      ((_, formWidget), formEnctype) <- notificationForm aid time
      defaultLayout $ do
        setTitleI MsgNotifications
        $(widgetFile "notifications")
    Nothing -> notAuthenticated -- this can't be reached

postNotificationsR :: Handler Html
postNotificationsR = do
  maid <- maybeAuthId
  case maid of
    Just aid -> do
      time <- liftIO getCurrentTime
      ((result, _), _) <- notificationForm aid time
      formHandler result $ \notification -> do
        runDB $ insert_ notification
        setMessageI MsgNotificationAdded
      redirect NotificationsR
    Nothing -> notAuthenticated -- this can't be reached

deleteNotificationR :: NotificationId -> Handler Html
deleteNotificationR nid =
  runDB $ delete nid >> redirect NotificationsR

putNotificationR :: NotificationId -> Handler Html
putNotificationR nid = do
  maid <- maybeAuthId
  case maid of
    Just aid -> do
      content <- runInputPost $ ireq textareaField "content"
      time <- liftIO getCurrentTime
      runDB $ update nid
        [ NotificationContent =. content
        , NotificationDate =. time
        , NotificationUser =. aid
        ]
      redirect NotificationsR
    Nothing -> notAuthenticated -- this can't be reached