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
  defaultLayout $ do
    $(widgetFile "init")
    $(widgetFile "competitioninit")

startedPage :: CompetitionId -> Handler Html
startedPage cid = do
  (nextRoundFormWidget, nextRoundFormEnctype) <- generateFormPost $
    identifyForm "nextround" $ nextRoundForm cid
  (finishCompetitionFormWidget, finishCompetitionFormEnctype) <- generateFormPost $
    identifyForm "finish" $ finishCompetitionForm cid
  rounds <- roundsWithNames cid
  dnf <- dnfRoundsWithNames cid
  -- for hamlet so it can put dividers between groups
  let groups = nub $ for rounds $ \(_,_,E.Value g,_) -> g
      mround = safeHead rounds
      currentRound = maybe 1 (\(_, E.Value r, _, _) -> r) mround
  defaultLayout $ do
    $(widgetFile "started")

finishedPage :: CompetitionId -> Handler Html
finishedPage cid = do
  competition <- runDB $ get404 cid
  defaultLayout $ do
    $(widgetFile "finished")

postCompetitionNextRoundR :: CompetitionId -> Handler Html
postCompetitionNextRoundR cid = do
  ((result, _), _) <- runFormPost $ nextRoundForm cid
  formHandler result $ \res -> do
    nextRound res
    setMessageI MsgNextRoundChanged
  redirect $ CompetitionR cid

postCompetitionFinishR :: CompetitionId -> Handler Html
postCompetitionFinishR cid = do
  ((result, _), _) <- runFormPost $ finishCompetitionForm cid
  formHandler result $ \res -> do
    finishCompetition res
    setMessageI MsgCompetitionFinished
  redirect $ CompetitionR cid

postCompetitionR :: CompetitionId -> Handler Html
postCompetitionR cid = do
  ((result, _), _) <- runFormPost $ startCompetitionForm cid
  formHandler result $ \res -> do
    startCompetition res
    setMessageI MsgCompetitionStarted
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