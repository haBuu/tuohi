{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin where

import Import

import Handler.CompetitionState
import Handler.TempAuth
import Database

getAdminR :: Handler Html
getAdminR = do
  -- current password
  (current, _) <- passwords
  superAdmin <- Database.isSuperAdmin
  competitions <- runDB $ selectList
    [CompetitionState !=. Finished] [Asc CompetitionDate]
  finished <- runDB $ selectList [CompetitionState ==. Finished]
    [Asc CompetitionDate, LimitTo 10]
  defaultLayout $ do
    setTitleI MsgAdminPanel
    $(widgetFile "admin")