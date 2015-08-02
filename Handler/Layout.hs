{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Layout where

import Import

import Handler.Forms
import Database

getLayoutR :: CourseId -> LayoutId -> Handler Html
getLayoutR cid lid = do
  layout <- runDB $ get404 lid
  holes <- runDB $ selectList [HoleLayoutId ==. lid] [Asc HoleNumber]
  ((_, formWidget), formEnctype) <- runFormPost $ holesForm holes
  defaultLayout $ do
    setTitleI MsgLayout
    $(widgetFile "layout")

postLayoutR :: CourseId -> LayoutId -> Handler Html
postLayoutR cid lid = do
  holes <- runDB $ selectList [HoleLayoutId ==. lid] [Asc HoleNumber]
  ((result, _), _) <- runFormPost $ holesForm holes
  formHandler result $ \res -> do
    forM_ res $ \(hid, par) -> do
      runDB $ update hid [HolePar =. par]
      logInfo "Layout updated"
    setMessageI MsgLayoutUpdated
  redirect $ LayoutR cid lid