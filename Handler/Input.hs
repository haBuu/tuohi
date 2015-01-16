{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Input where

import Import
import Control.Monad
import Data.List(find)

import Handler.TempAuth
import Handler.Forms
import qualified Database.Esqueleto as E
import Database

getInputR :: CompetitionId -> Int -> Handler Html
getInputR cid groupNumber = do
  -- render input site only if user has temp auth
  tempAuth <- isTempAuth
  if tempAuth
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
          let value = case mScore of
                Just (Entity _ score) -> Just $ scoreScore score
                Nothing -> Nothing
          generateFormPost $ scoreForm hid rid name value
        return (holeNumber hole, forms)
      muser <- maybeAuthUser
      defaultLayout $ do
        $(widgetFile "input")
    else do
      -- set ultimate destination to current url
      -- so that the user is redirected back to this
      -- url after he has given the correct password
      setUltDestCurrent
      redirect TempAuthR