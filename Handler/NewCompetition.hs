{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.NewCompetition where

import Import

import Handler.Forms
import Database

getNewCompetitionR :: Handler Html
getNewCompetitionR = do
  (formWidget, formEnctype) <- generateFormPost newCompetitionForm
  defaultLayout $ do
    -- TODO: bootstrapper datepicker or something?
    -- jquery datepicker
    -- addScript $ StaticR js_jquery_mobile_datepicker_js
    -- addStylesheet $ StaticR css_jquery_mobile_datepicker_css
    setTitleI MsgAddCompetition
    $(widgetFile "newcompetition")

postNewCompetitionR :: Handler Html
postNewCompetitionR = do
  ((result, formWidget), formEnctype) <- runFormPost newCompetitionForm
  formHandler result $ \res -> do
    runDB $ insert_ res
    setMessageI MsgCompetitionAdded
  redirect AdminR