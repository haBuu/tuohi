{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.NewCompetition where

import Import

import Data.List(find)
import qualified Data.Text

import Handler.Forms
import Database

getNewCompetitionR :: Handler Html
getNewCompetitionR = do
  -- choose first 2 letter language for datepicker
  langs <- languages
  let lang = maybe "en" Data.Text.unpack $
        find (\l -> (Data.Text.length l) == 2) langs
  (formWidget, formEnctype) <- generateFormPost newCompetitionForm
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
  ((result, formWidget), formEnctype) <- runFormPost newCompetitionForm
  formHandler result $ \res -> do
    runDB $ insert_ res
    setMessageI MsgCompetitionAdded
  redirect AdminR