{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Layout where

import Import
import Control.Monad

import Handler.Forms
import Database

getLayoutR :: CourseId -> LayoutId -> Handler Html
getLayoutR cid lid = do
  holes <- runDB $ selectList [HoleLayoutId ==. lid] [Asc HoleNumber]
  ((_, formWidget), formEnctype) <- runFormPost $ holesForm holes
  muser <- maybeAuthUser
  defaultLayout $ do
    setTitle "Layout"
    mmsg <- getMessage
    let headerWidget = $(widgetFile "header")
    $(widgetFile "layout")

postLayoutR :: CourseId -> LayoutId -> Handler Html
postLayoutR cid lid = do
  holes <- runDB $ selectList [HoleLayoutId ==. lid] [Asc HoleNumber]
  ((result, _), _) <- runFormPost $ holesForm holes
  formHandler result $ \res -> do
    forM_ res $ \(hid, par) -> do
      runDB $ update hid [HolePar =. par]
    setMessage "Layout päivitettiin onnistuneesti"
  redirect $ LayoutR cid lid