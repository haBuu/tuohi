{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Scores where

import Import

import qualified Database.Esqueleto as E
import Database.Esqueleto((^.))
import Text.Julius(rawJS)

import Handler.CompetitionAuth
import Handler.Forms
import Model.RoundState(RoundState(..))
import qualified Model.CompetitionState as CS
import Competition.Competition
import Database
import Helpers

getScoresR :: CompetitionId -> Handler TypedContent
getScoresR cid = do
  competition <- runDB $ get404 cid
  let live = competitionState competition /= CS.Finished
  let lid = competitionLayoutId competition
  holes <- runDB $ selectList
    [HoleLayoutId ==. lid] [Asc HoleNumber]
  let layoutPar = countPar holes
  curRound <- runDB $ currentRound cid
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
      value <- runInputPost $ ireq (checkScore intField) "score"
      let score = Score rid hid value
      insertScore cid score
      redirect $ InputR (roundCompetitionId round_)
        (roundGroupnumber round_)
    else
      notAuthenticated

requireStarted :: Round -> Handler ()
requireStarted round_ = unless (roundState round_ == Started) $
  permissionDeniedI MsgScoreInputNotAllowed

insertScore :: CompetitionId -> Score -> Handler ()
insertScore cid score = runDB $
  -- score 0 is special value for deleting scores
  if scoreScore score == 0
    then do
      mScore <- getBy $
        UniqueScore (scoreRoundId score) (scoreHoleId score)
      case mScore of
        -- maybe not the best idea to delete update log with the score
        -- but we will go with that for now
        Just (Entity sid _) -> deleteCascade sid
        Nothing -> return ()
    else do
      eScore <- insertBy score
      case eScore of
        -- new score was added
        Right _ -> return ()
        -- update existing score and log update if the score changed
        Left (Entity key prev) -> do
          let new = scoreScore score
              old = scoreScore prev
          when (old /= new) $ do
            update key [ScoreScore =. scoreScore score]
            time <- liftIO getCurrentTime
            insert_ $ ScoreUpdateLog key cid time old new

getScoreEditPlayersR :: CompetitionId -> Handler Html
getScoreEditPlayersR cid = do
  competition <- runDB $ get404 cid
  players <- runDB $ confirmedPlayers cid
  defaultLayout $ do
    setTitleI MsgEditScores
    $(widgetFile "score-edit-players")

getScoreEditPlayerR :: CompetitionId -> UserId -> Handler Html
getScoreEditPlayerR cid uid = do
  user <- runDB $ get404 uid
  competition <- runDB $ get404 cid
  let lid = competitionLayoutId competition
  holes <- runDB $ selectList
    [HoleLayoutId ==. lid] [Asc HoleNumber]
  rounds <- runDB $ selectList
    [ RoundCompetitionId ==. cid
    , RoundUserId ==. uid
    ]
    [Asc RoundRoundnumber]
  scores <- runDB $ selectList
    [ScoreRoundId <-. (map (\(Entity rid _) -> rid) rounds)]
    []
  roundsAndForms <- forM rounds $ \(Entity rid round_) -> do
    forms <- forM holes $ \(Entity hid hole) -> do
      -- try to find existing score for the form
      let mScore = flip find scores $ \(Entity _ score) ->
            scoreRoundId score == rid && scoreHoleId score == hid
      -- get actual value of the score from Maybe (Entity Score)
      let value = fmap (scoreScore . entityVal) mScore
      runFormPost $ scoreEditForm cid hid rid (holeNumber hole) value
    return (roundRoundnumber round_, forms)
  defaultLayout $ do
    setTitleI MsgEditScores
    $(widgetFile "score-edit")

postScoreEditR :: CompetitionId -> RoundId -> HoleId -> Handler Html
postScoreEditR cid rid hid = do
  round_ <- runDB $ get404 rid
  hole <- runDB $ get404 hid
  ((result, _), _) <- runFormPost $
    scoreEditForm cid hid rid (holeNumber hole) Nothing
  formHandler result $ \res -> insertScore cid res
  redirect $ ScoreEditPlayerR cid (roundUserId round_)

confirmedPlayers :: CompetitionId -> DB [Entity User]
confirmedPlayers cid = E.select $
  E.from $ \(signUp, user) -> do
    E.where_ $ signUp ^. SignUpUserId E.==. user ^. UserId
    E.where_ $ signUp ^. SignUpCompetitionId E.==. E.val cid
    E.where_ $ signUp ^. SignUpConfirmed E.==. E.val True
    E.orderBy [E.asc (user ^. UserName)]
    return user