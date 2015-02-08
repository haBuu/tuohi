{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Competition where

import Import
import Data.List(nub)
import Control.Monad(forM)

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
  ((_, formWidget), formEnctype) <- runFormPost $
    startCompetitionForm cid
  defaultLayout $ do
    $(widgetFile "init")
    $(widgetFile "competitioninit")

startedPage :: CompetitionId -> Handler Html
startedPage cid = do
  competition <- runDB $ get404 cid
  ((_, nextRoundFormWidget), nextRoundFormEnctype) <- runFormPost $
    identifyForm "nextround" $ nextRoundForm cid
  ((_, finishFormWidget), finishFormEnctype) <- runFormPost $
    identifyForm "finish" $ finishCompetitionForm cid
  rounds <- roundsWithNames cid
  dnf <- dnfRoundsWithNames cid
  -- for hamlet so it can put dividers between groups
  let groups = nub $ for rounds $ \(_,_,E.Value g,_) -> g
      mround = safeHead rounds
      currentRound = maybe 1 (\(_, E.Value r, _, _) -> r) mround
  -- get score count for each player so we can display
  -- labels for how many holes they have played
  -- this could be done with the same query where we get the rounds
  scoreCounts <- forM rounds $ \(E.Value rid, _, _, _) -> do
    scoreCount rid
  -- how many holes does the layout have
  count <- holeCount $ competitionLayoutId competition
  -- compine rounds and score counts
  let roundsAndScores = zip rounds scoreCounts
  defaultLayout $ do
    $(widgetFile "started")

finishedPage :: CompetitionId -> Handler Html
finishedPage cid = do
  competition <- runDB $ get404 cid
  layout <- runDB $ get404 $ competitionLayoutId competition
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