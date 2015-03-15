module Handler.FinishedCompetitions where

import Import

import Model.CompetitionState

getFinishedCompetitionsR :: Handler Html
getFinishedCompetitionsR = do
  finished <- runDB $ selectList
    [CompetitionState ==. Finished]
    [Asc CompetitionDate]
  defaultLayout $ do
    setTitleI MsgCompetitions
    $(widgetFile "finished-competitions")