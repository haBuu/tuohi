{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Competition where

import Import
import Data.List(nub)

import qualified Database.Esqueleto as E

import Handler.Forms
import Handler.CompetitionState
import Handler.RoundState(RoundState(DidNotFinish))
import Database

getCompetitionR :: CompetitionId -> Handler Html
getCompetitionR cid = do
  competition <- runDB $ get404 cid
  case competitionState competition of
    Init -> initPage cid
    Started -> startedPage cid
    Finished -> finishedPage cid

initPage :: CompetitionId -> Handler Html
initPage cid = do
  signups <- signUpsWithName cid
  (formWidget, formEnctype) <- generateFormPost $
    startCompetitionForm cid
  muser <- maybeAuthUser
  defaultLayout $ do
    setTitle "Competition"
    let headerWidget = $(widgetFile "header")
    $(widgetFile "init")
    $(widgetFile "competitioninit")

startedPage :: CompetitionId -> Handler Html
startedPage cid = do
  (nextRoundFormWidget, nextRoundFormEnctype) <- generateFormPost $
    identifyForm "nextround" $ nextRoundForm cid
  (finishCompetitionFormWidget, finishCompetitionFormEnctype) <- generateFormPost $
    identifyForm "finish" $ finishCompetitionForm cid
  rounds <- roundsWithNames cid
  -- for hamlet so it can put dividers between groups
  let groups = nub $ for rounds $ \(_,_,E.Value g,_) -> g
  muser <- maybeAuthUser
  defaultLayout $ do
    setTitle "Competition"
    let headerWidget = $(widgetFile "header")
    $(widgetFile "started")

finishedPage :: CompetitionId -> Handler Html
finishedPage cid = do
  competition <- runDB $ get404 cid
  muser <- maybeAuthUser
  defaultLayout $ do
    setTitle "Competition"
    let headerWidget = $(widgetFile "header")
    $(widgetFile "finished")

postCompetitionNextRoundR :: CompetitionId -> Handler Html
postCompetitionNextRoundR cid = do
  ((result, _), _) <- runFormPost $ nextRoundForm cid
  case result of
    FormSuccess res -> do
      nextRound res
      setMessage "Kierros vaihdettiin onnistuneesti"
    FormFailure err -> setMessage $ toHtml $ head err
    FormMissing -> setMessage "No mitä vittua?"
  redirect $ CompetitionR cid

postCompetitionFinishR :: CompetitionId -> Handler Html
postCompetitionFinishR cid = do
  ((result, _), _) <- runFormPost $ finishCompetitionForm cid
  case result of
    FormSuccess res -> do
      finishCompetition res
      setMessage "Kisa lopetettiin onnistuneesti"
    FormFailure err -> setMessage $ toHtml $ head err
    FormMissing -> setMessage "No mitä vittua?"
  redirect $ CompetitionR cid

postCompetitionR :: CompetitionId -> Handler Html
postCompetitionR cid = do
  ((result, _), _) <- runFormPost $ startCompetitionForm cid
  case result of
    FormSuccess res -> do
      startCompetition res
      setMessage "Kisa aloitettiin onnistuneesti"
    FormFailure err -> setMessage $ toHtml $ head err
    FormMissing -> setMessage "No mitä vittua?"
  redirect $ CompetitionR cid

postConfirmSignUpR :: SignUpId -> Handler Html
postConfirmSignUpR sid = do
  runDB $ update sid [SignUpConfirmed =. True]
  redirect AdminR

postRemoveSignUpR :: SignUpId -> Handler Html
postRemoveSignUpR sid = do
  runDB $ delete sid
  redirect AdminR

postDnfRoundR :: RoundId -> Handler Html
postDnfRoundR rid = do
  runDB $ update rid [RoundState =. DidNotFinish]
  redirect AdminR