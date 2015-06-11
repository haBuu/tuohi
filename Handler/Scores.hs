{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Scores where

import Import

import Handler.CompetitionAuth
import Handler.Forms
import Model.RoundState(RoundState(..))
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
  auth <- isCompetitionAuth cid
  if auth
    then do
      round_ <- runDB $ get404 rid
      -- make sure that the round is in state started
      requireStarted round_
      user <- runDB $ get404 $ roundUserId round_
      ((result, _), _) <- runFormPost $ scoreForm cid hid rid
        (userName user) Nothing
      formHandler result $ \res -> insertScore cid res
      redirect $ InputR (roundCompetitionId round_)
        (roundGroupnumber round_)
    else
      notAuthenticated

requireStarted :: Round -> Handler ()
requireStarted round_ = unless (roundState round_ == Started) $
  permissionDeniedI MsgScoreInputNotAllowed

insertScore :: CompetitionId -> Score -> Handler ()
insertScore cid score = runDB $ do
  eScore <- insertBy score
  case eScore of
    -- new score was added
    Right _ -> return ()
    -- update existing score and log update
    Left (Entity key prev) -> do
      let new = scoreScore score
          old = scoreScore prev
      update key [ScoreScore =. scoreScore score]
      time <- liftIO getCurrentTime
      insert_ $ ScoreUpdateLog key cid time old new
