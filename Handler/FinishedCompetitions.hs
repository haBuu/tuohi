module Handler.FinishedCompetitions where

import Import

import Model.CompetitionState

getFinishedCompetitionsR :: Handler Html
getFinishedCompetitionsR = do
  finished <- runDB $ selectList
    [ CompetitionState ==. Finished
    , CompetitionPrivate !=. True
    ]
    [Desc CompetitionDate]
  defaultLayout $ do
    setTitleI MsgCompetitions
    $(widgetFile "finished-competitions")