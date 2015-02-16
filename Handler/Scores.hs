{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Scores where

import Import

import Handler.CompetitionAuth
import Handler.Forms
import Handler.CompetitionState
import Handler.RoundState(RoundState(DidNotFinish))
import Competition.Competition
import Database
import Helpers

getScoresR :: CompetitionId -> Handler TypedContent
getScoresR cid = do
  competition <- runDB $ get404 cid
  let lid = competitionLayoutId competition
  holes <- runDB $ selectList
    [HoleLayoutId ==. lid] [Asc HoleNumber]
  let layoutPar = countPar holes
  curRound <- currentRound cid
  let roundCount = maybe 1 id curRound
  players <- playersAndScores cid
  let sortedPlayers = addPlacements holes $ playerSort holes players
  -- Accept: application/json will return JSON
  -- Accept: text/html will return HTML
  defaultLayoutJson
    (setTitleI MsgHoleScores >> $(widgetFile "scores")) -- html
    (returnJson sortedPlayers) -- json

postScoreR :: CompetitionId -> RoundId -> HoleId -> Handler Html
postScoreR cid rid hid = do
  competitionAuth <- isCompetitionAuth cid
  if competitionAuth
    then do
      round_ <- runDB $ get404 rid
      user <- runDB $ get404 $ roundUserId round_
      ((result, _), _) <- runFormPost $ scoreForm cid hid rid
        (userName user) Nothing
      formHandler result $ \res -> do
        void $ runDB $ upsert res [ScoreScore =. scoreScore res]
      redirect $ InputR (roundCompetitionId round_)
        (roundGroupnumber round_)
    else
      notAuthenticated