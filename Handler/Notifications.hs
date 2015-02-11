{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Notifications where

import Import
import Data.Time

import Yesod.Auth

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
    Nothing -> return ()
  redirect NotificationsR

deleteNotificationR :: NotificationId -> Handler Html
deleteNotificationR nid = do
  runDB $ delete nid
  redirect NotificationsR

putNotificationR :: NotificationId -> Handler Html
putNotificationR nid = do
  content <- runInputPost $ ireq textareaField "content"
  runDB $ update nid [NotificationContent =. content]
  redirect NotificationsR