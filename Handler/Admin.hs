{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin where

import Import

import Model.CompetitionState
import Database

getAdminR :: Handler Html
getAdminR = do
  superAdmin <- Database.isSuperAdmin
  competitions <- runDB $ selectList
    [CompetitionState !=. Finished] [Asc CompetitionDate]
  defaultLayout $ do
    setTitleI MsgAdminPanel
    $(widgetFile "admin")