module Handler.EditCompetition where

import Import
import qualified Datepicker
import Helpers
import Handler.Forms
import Handler.CompetitionState

getEditCompetitionR :: CompetitionId -> Handler Html
getEditCompetitionR cid = do
  -- language for datepicker
  lang <- liftM language languages
  competition <- runDB $ get404 cid
  checkState competition
  let uid = competitionUserId competition
  ((_, formWidget), formEnctype) <- runFormPost $
    editCompetitionForm competition
  defaultLayout $ do
    Datepicker.addDatepicker
    setTitleI MsgEditCompetition
    -- datepicker widget enables datepicker for all inputs of type date
    $(widgetFile "datepicker")
    $(widgetFile "editcompetition")

postEditCompetitionR :: CompetitionId -> Handler Html
postEditCompetitionR cid = do
  competition <- runDB $ get404 cid
  checkState competition
  ((result, _), _) <- runFormPost $ editCompetitionForm competition
  formHandler result $ \res -> do
    runDB $ replace cid res
    setMessageI MsgCompetitionUpdated
  redirect $ CompetitionR cid

checkState :: Competition -> Handler ()
checkState competition =
  if competitionState competition /= Init
    then permissionDeniedI MsgCompetitionStartedError
    else return ()