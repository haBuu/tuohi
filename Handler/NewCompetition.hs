{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.NewCompetition where

import Import

import Handler.Forms
import Handler.Division
import Helpers
import qualified Datepicker

getNewCompetitionR :: Handler Html
getNewCompetitionR = do
  -- language for datepicker
  lang <- liftM language languages
  uid <- requireAuthId
  ((_, formWidget), formEnctype) <- runFormPost $
    competitionForm uid Nothing defaultDivisions
  defaultLayout $ do
    Datepicker.addDatepicker
    setTitleI MsgAddCompetition
    -- datepicker widget enables datepicker for all inputs of type date
    $(widgetFile "datepicker")
    $(widgetFile "newcompetition")

postNewCompetitionR :: Handler Html
postNewCompetitionR = do
  uid <- requireAuthId
  ((result, _), _) <- runFormPost $
    competitionForm uid Nothing defaultDivisions
  formHandler result $ \(comp, divisions) -> do
    runDB $ do
      cid <- insert comp
      insertMany_ $ map (\d -> CompetitionDivision cid d) divisions
    setMessageI MsgCompetitionAdded
  redirect AdminR