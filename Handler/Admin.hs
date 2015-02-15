{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin where

import Import

import Handler.CompetitionState
import Database

getAdminR :: Handler Html
getAdminR = do
  superAdmin <- Database.isSuperAdmin
  competitions <- runDB $ selectList
    [CompetitionState !=. Finished] [Asc CompetitionDate]
  defaultLayout $ do
    setTitleI MsgAdminPanel
    $(widgetFile "admin")