{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Series where

import Import

import Handler.Forms

getSeriesR :: Handler Html
getSeriesR = do
  series <- runDB $ selectList [] [Asc SerieName]
  ((_, formWidget), formEnctype) <- newSerieForm
  defaultLayout $ do
    setTitleI MsgSeries
    $(widgetFile "series")

postSeriesR :: Handler Html
postSeriesR = do
  ((result, _), _) <- newSerieForm
  formHandler result $ \res -> do
    runDB $ insert_ res
    setMessageI MsgSerieAdded
  redirect SeriesR