{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Groups where

import Import
import Data.List(nub)

import qualified Database.Esqueleto as E
import Database

getGroupsR :: CompetitionId -> Handler Html
getGroupsR cid = do
  rounds <- roundsWithNames cid
  -- for hamlet so it can put dividers between groups
  let groups = nub $ for rounds $ \(_,_,E.Value g,_) -> g
  defaultLayout $ do
    $(widgetFile "groups")