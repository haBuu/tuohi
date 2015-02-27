{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Competition where

import Import hiding(for)

import Data.List(nub)

import qualified Database.Esqueleto as E

import Handler.Forms
import Handler.CompetitionState
import Handler.RoundState(RoundState(DidNotFinish))
import Database
import Helpers

getCompetitionR :: CompetitionId -> Handler Html
getCompetitionR cid = do
  competition <- runDB $ get404 cid
  case competitionState competition of
    Init -> initPage cid
    Started -> startedPage cid
    Finished -> notFound

initPage :: CompetitionId -> Handler Html
initPage cid = do
  competition <- runDB $ get404 cid
  signups <- signUpsWithName cid
  ((_, formWidget), formEnctype) <- startCompetitionForm cid
  defaultLayout $ do
    setTitleI MsgAdminPanel
    $(widgetFile "init")
    $(widgetFile "competitioninit")

startedPage :: CompetitionId -> Handler Html
startedPage cid = do
  competition <- runDB $ get404 cid
  ((_, nextRoundFormWidget), nextRoundFormEnctype) <- nextRoundForm cid
  ((_, finishFormWidget), finishFormEnctype) <- finishCompetitionForm cid
  rounds <- roundsWithNames cid
  dnf <- dnfRoundsWithNames cid
  -- for hamlet so it can put dividers between groups
  let groups = nub $ for rounds $ \(_,_,E.Value g,_) -> g
      mround = safeHead rounds
      currentRound_ = maybe 1 (\(_, E.Value r, _, _) -> r) mround
  -- get score count for each player so we can display
  -- labels for how many holes they have played
  -- this could be done with the same query where we get the rounds
  scoreCounts <- forM rounds $
    \(E.Value rid, _, _, _) -> scoreCount rid
  -- how many holes does the layout have
  count_ <- holeCount $ competitionLayoutId competition
  -- compine rounds and score counts
  let roundsAndScores = zip rounds scoreCounts
  defaultLayout $ do
    setTitleI MsgAdminPanel
    $(widgetFile "started")

postCompetitionNextRoundR :: CompetitionId -> Handler Html
postCompetitionNextRoundR cid = do
  ((result, _), _) <- nextRoundForm cid
  formHandler result $ \res -> do
    nextRound res
    setMessageI MsgNextRoundChanged
  redirect $ CompetitionR cid

postCompetitionFinishR :: CompetitionId -> Handler Html
postCompetitionFinishR cid = do
  ((result, _), _) <- finishCompetitionForm cid
  formHandler result $ \res -> do
    finishCompetition res
    setMessageI MsgCompetitionFinished
  redirect $ CompetitionR cid

postCompetitionR :: CompetitionId -> Handler Html
postCompetitionR cid = do
  ((result, _), _) <- startCompetitionForm cid
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