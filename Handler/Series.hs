{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Series where

import Import

import Handler.Forms
import Database

getSeriesR :: Handler Html
getSeriesR = do
  series <- runDB $ selectList [] [Asc SerieName]
  ((_, formWidget), formEnctype) <- runFormPost newSerieForm
  muser <- maybeAuthUser
  defaultLayout $ do
    mmsg <- getMessage
    setTitle "WeeklyApp"
    let headerWidget = $(widgetFile "header")
    $(widgetFile "series")

postSeriesR :: Handler Html
postSeriesR = do
  series <- runDB $ selectList [] [Asc SerieName]
  ((result, _), _) <- runFormPost newSerieForm
  formHandler result $ \res -> runDB $ insert_ res
  redirect SeriesR