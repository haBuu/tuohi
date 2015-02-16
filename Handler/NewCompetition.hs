{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.NewCompetition where

import Import

import Handler.Forms
import Helpers

getNewCompetitionR :: Handler Html
getNewCompetitionR = do
  -- language for datepicker
  lang <- liftM language languages
  ((_, formWidget), formEnctype) <- runFormPost newCompetitionForm
  defaultLayout $ do
    addScript $ StaticR js_bootstrap_datepicker_js
    -- add more languages here (currently finnish and english)
    addScript $ StaticR js_locales_bootstrap_datepicker_fi_js
    addStylesheet $ StaticR css_datepicker3_css
    setTitleI MsgAddCompetition
    -- datepicker widget enables datepicker for all inputs of type date
    $(widgetFile "datepicker")
    $(widgetFile "newcompetition")

postNewCompetitionR :: Handler Html
postNewCompetitionR = do
  ((result, _), _) <- runFormPost newCompetitionForm
  formHandler result $ \res -> do
    runDB $ insert_ res
    setMessageI MsgCompetitionAdded
  redirect AdminR