{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Competition where

import Import hiding(for)

import Data.List(nub)
import Data.Time.LocalTime

import qualified Database.Esqueleto as E

import Handler.Forms
import Model.CompetitionState
import qualified Model.RoundState as R
import Database
import Helpers

getCompetitionR :: CompetitionId -> Handler Html
getCompetitionR cid = do
  competition <- runDB $ get404 cid
  case competitionState competition of
    Init -> initPage cid
    Started -> startedPage cid
    Finished -> redirect AdminR

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
  mScoreUpdate <- runDB $ selectFirst
    [ScoreUpdateLogCompetitionId ==. cid] []
  ((_, nextRoundFormWidget), nextRoundFormEnctype) <- nextRoundForm cid
  ((_, finishFormWidget), finishFormEnctype) <- finishCompetitionForm cid
  rounds <- roundsWithNames cid
  dnf <- dnfRoundsWithNames cid
  -- for hamlet so it can put dividers between groups
  let groups = nub $ for rounds $ \(_, _, E.Value g, _, _) -> g
      mround = safeHead rounds
      currentRound_ = maybe 1 (\(_, E.Value r, _, _, _) -> r) mround
  -- get score count for each player so we can display
  -- labels for how many holes they have played
  -- this could be done with the same query where we get the rounds
  scoreCounts <- forM rounds $
    \(E.Value rid, _, _, _, _) -> roundScoreCount rid
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
    allScores <- allScoresInserted res
    if allScores
      then do
        nextRound res
        logInfo "Round changed"
        setMessageI MsgNextRoundChanged
      else
        setMessageI MsgNextRoundScoresMissing
  redirect $ CompetitionR cid

postCompetitionFinishR :: CompetitionId -> Handler Html
postCompetitionFinishR cid = do
  ((result, _), _) <- finishCompetitionForm cid
  formHandler result $ \res -> do
    allScores <- allScoresInserted cid
    if allScores
      then do
        finishCompetition res
        logInfo "Competition finished"
        setMessageI MsgCompetitionFinished
      else
        setMessageI MsgFinishCompetitionScoresMissing
  redirect $ CompetitionR cid

postCompetitionR :: CompetitionId -> Handler Html
postCompetitionR cid = do
  ((result, _), _) <- startCompetitionForm cid
  formHandler result $ \res -> do
    startCompetition res
    logInfo "Competition started"
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
  runDB $ update rid [RoundState =. R.DidNotFinish]
  redirect AdminR

getScoreLogR :: CompetitionId -> Handler Html
getScoreLogR cid = do
  competition <- runDB $ get404 cid
  scoreUpdateLog <- scoreLogWithNames cid
  tz <- liftIO getCurrentTimeZone
  defaultLayout $ do
    setTitleI MsgAdminPanel
    $(widgetFile "score-log")

allScoresInserted :: CompetitionId -> Handler Bool
allScoresInserted cid = runDB $ do
  mround <- currentRound cid
  case mround of
    Nothing -> return False
    Just r -> do
      competition <- get404 cid
      let lid = competitionLayoutId competition
      holes <- count [HoleLayoutId ==. lid]
      rounds <- count $
        [ RoundCompetitionId ==. cid
        , RoundRoundnumber ==. r
        , RoundState ==. R.Started
        ]
      -- how many scores are expected to be inserted
      let expectedScores = holes * rounds
      -- count scores from started rounds in this round
      x <- scoreCount cid r
      return $ case x of
        [E.Value scores] -> scores == expectedScores
        _ -> False -- this should not be reached