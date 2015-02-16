{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Input where

import Import hiding(group)

import Handler.CompetitionAuth
import Handler.Forms
import qualified Database.Esqueleto as E
import Database

getInputR :: CompetitionId -> Int -> Handler Html
getInputR cid groupNumber = do
  -- render input site only if user has temp auth
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
      holesAndForms <- forM holes $ \(Entity hid hole) -> do
        forms <- forM group $ \(E.Value rid, _, _, E.Value name) -> do
          -- try to find existing score for the form
          let mScore = flip find scores $ \(Entity _ score) ->
                scoreRoundId score == rid && scoreHoleId score == hid
          -- get actual value of the score from Maybe (Entity Score)
          let value = fmap (scoreScore . entityVal) mScore
          runFormPost $ scoreForm cid hid rid name value
        return (holeNumber hole, forms)
      muser <- maybeAuthUser
      defaultLayout $ do
        setTitleI $ MsgGroupNumber groupNumber
        $(widgetFile "input")
    else do
      -- set ultimate destination to current url
      -- so that the user is redirected back to this
      -- url after he has given the correct password
      setUltDestCurrent
      redirect $ CompetitionAuthR cid