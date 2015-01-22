{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Scores where

import Import

import Handler.TempAuth
import Handler.Forms
import Handler.CompetitionState
import Handler.RoundState(RoundState(DidNotFinish))
import Competition.Competition
import Database

-- just for convenience for the users
getScoresR :: Handler Html
getScoresR = do
  mcompetition <- runDB $ selectFirst
    [CompetitionState ==. Started] [Asc CompetitionDate]
  case mcompetition of
    Nothing -> notFound
    Just (Entity cid competition) -> redirect $ CompetitionScoresR cid

getCompetitionScoresR :: CompetitionId -> Handler TypedContent
getCompetitionScoresR cid = do
  competition <- runDB $ get404 cid
  let lid = competitionLayoutId competition
  holes <- runDB $ selectList
    [HoleLayoutId ==. lid] [Asc HoleNumber]
  let layoutPar = countPar holes
  curRound <- currentRound cid
  let roundCount = maybe 1 id curRound
  players <- playersAndScores cid
  let sortedPlayers = addPlacements holes $ playerSort holes players
  muser <- maybeAuthUser
  let headerWidget = $(widgetFile "header")
  -- Accept: application/json will return JSON
  -- Accept: text/html will return HTML
  defaultLayoutJson
    $(widgetFile "scores") -- html
    (returnJson sortedPlayers) -- json

postScoreR :: RoundId -> HoleId -> Handler Html
postScoreR rid hid = do
  tempAuth <- isTempAuth
  if tempAuth || True
    then do
      round_ <- runDB $ get404 rid
      user <- runDB $ get404 $ roundUserId round_
      ((result, _), _) <- runFormPost $ scoreForm hid rid
        (userName user) Nothing
      formHandler result $ \res -> do
        -- check if the score already exists
        mScore <- runDB $ getBy $
          UniqueScore (scoreRoundId res) (scoreHoleId res)
        case mScore of
          -- replace previous score
          Just (Entity sid _) -> runDB $ replace sid res
          -- insert new score
          Nothing -> runDB $ insert_ res
      redirect $ InputR (roundCompetitionId round_)
        (roundGroupnumber round_)
    else redirect TempAuthR