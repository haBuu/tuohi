{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Input where

import Import hiding(group)

import Handler.CompetitionAuth
import qualified Database.Esqueleto as E
import Database

getInputR :: CompetitionId -> Int -> Handler Html
getInputR cid groupNumber = do
  -- render input site only if user has competition auth
  auth <- isCompetitionAuth cid
  if auth
    then do
      competition <- runDB $ get404 cid
      let lid = competitionLayoutId competition
      holes <- runDB $ selectList
        [HoleLayoutId ==. lid] [Asc HoleNumber]
      group <- groupWithNames cid groupNumber
      -- all the scores for the group
      scores <- runDB $ selectList
        [ScoreRoundId <-. (map (\(E.Value rid, _, _,_) -> rid) group)]
        []
      r <- getUrlRender
      holesAndPlayers <- forM holes $ \(Entity hid hole) -> do
        players <- forM group $ \(E.Value rid, _, _, E.Value name) -> do
          -- try to find existing score
          let mScore = flip find scores $ \(Entity _ score) ->
                scoreRoundId score == rid && scoreHoleId score == hid
          -- get actual value of the score from Maybe (Entity Score)
          let mValue = fmap (scoreScore . entityVal) mScore
          return (rid, name, mValue)
        return ((hid, holeNumber hole), zip [1..] players)
      -- hole that should be active when the page is loaded
      let startingHole = groupNumber
      defaultLayout $ do
        setTitleI $ MsgGroupNumber groupNumber
        $(widgetFile "input-scores")
        $(widgetFile "input")
    else do
      -- set ultimate destination to current url
      -- so that the user is redirected back to this
      -- url after he has given the correct password
      setUltDestCurrent
      redirect $ CompetitionAuthR cid

-- helper
firstHoleFirstPlayer :: Int -> Int -> Bool
firstHoleFirstPlayer hole player = hole == 1 && player == 1