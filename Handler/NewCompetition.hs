{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.NewCompetition where

import Import

import Handler.Forms
import Helpers
import qualified Datepicker

getNewCompetitionR :: Handler Html
getNewCompetitionR = do
  -- language for datepicker
  lang <- liftM language languages
  uid <- requireAuthId
  ((_, formWidget), formEnctype) <- runFormPost $ newCompetitionForm uid
  defaultLayout $ do
    Datepicker.addDatepicker
    setTitleI MsgAddCompetition
    -- datepicker widget enables datepicker for all inputs of type date
    $(widgetFile "datepicker")
    $(widgetFile "newcompetition")

postNewCompetitionR :: Handler Html
postNewCompetitionR = do
  uid <- requireAuthId
  ((result, _), _) <- runFormPost $ newCompetitionForm uid
  formHandler result $ \res -> do
    runDB $ insert_ res
    setMessageI MsgCompetitionAdded
  redirect AdminR