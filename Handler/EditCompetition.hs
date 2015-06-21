module Handler.EditCompetition where

import Import
import qualified Datepicker
import Helpers
import Handler.Forms
import Model.CompetitionState
import Handler.Division

getEditCompetitionR :: CompetitionId -> Handler Html
getEditCompetitionR cid = do
  -- language for datepicker
  lang <- liftM language languages
  competition <- runDB $ get404 cid
  requireInit competition
  divs <- compDivisions cid
  let uid = competitionUserId competition
  ((_, formWidget), formEnctype) <- runFormPost $
    competitionForm uid (Just competition) divs
  defaultLayout $ do
    Datepicker.addDatepicker
    setTitleI MsgEditCompetition
    -- datepicker widget enables datepicker for all inputs of type date
    $(widgetFile "datepicker")
    $(widgetFile "editcompetition")

postEditCompetitionR :: CompetitionId -> Handler Html
postEditCompetitionR cid = do
  competition <- runDB $ get404 cid
  requireInit competition
  divs <- compDivisions cid
  let uid = competitionUserId competition
  ((result, _), _) <- runFormPost $
    competitionForm uid (Just competition) divs
  formHandler result $ \(comp, divisions) -> do
    runDB $ do
      replace cid comp
      deleteWhere [CompetitionDivisionCompetitionId ==. cid]
      insertMany_ $ map (\d -> CompetitionDivision cid d) divisions
    setMessageI MsgCompetitionUpdated
  redirect $ CompetitionR cid

compDivisions :: CompetitionId -> Handler [Division]
compDivisions cid = do
  divs <- runDB $ selectList
    [CompetitionDivisionCompetitionId ==. cid] []
  return $ map (competitionDivisionDivision . entityVal) divs

requireInit :: Competition -> Handler ()
requireInit comp = unless (competitionState comp == Init) $
  permissionDeniedI MsgCompetitionStartedError