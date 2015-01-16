{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.NewCompetition where

import Import

import Handler.Forms
import Database

getNewCompetitionR :: Handler Html
getNewCompetitionR = do
  (formWidget, formEnctype) <- generateFormPost newCompetitionForm
  muser <- maybeAuthUser
  defaultLayout $ do
    -- jquery datepicker
    addScript $ StaticR js_jquery_mobile_datepicker_js
    addStylesheet $ StaticR css_jquery_mobile_datepicker_css
    setTitle "Add new competition"
    mmsg <- getMessage
    let headerWidget = $(widgetFile "header")
    $(widgetFile "newcompetition")

postNewCompetitionR :: Handler Html
postNewCompetitionR = do
  ((result, formWidget), formEnctype) <- runFormPost newCompetitionForm
  case result of
    FormSuccess res -> do
      runDB $ insert_ res
      setMessage "Kisa lisättiin onnistuneesti"
    FormFailure err -> setMessage $ toHtml $ head err
    FormMissing -> setMessage "No mitä vittua?"
  redirect AdminR