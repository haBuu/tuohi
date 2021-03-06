{-# LANGUAGE FlexibleInstances #-}
module Model where

import ClassyPrelude.Yesod
import Yesod.Markdown (Markdown)
import Database.Persist.Quasi

import Handler.Division
import Helpers
import Model.Permission
import Model.EventLog
import Model.CompetitionState
import Model.RoundState hiding(Finished)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [ mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

isFinished :: Entity Competition -> Bool
isFinished = (== Finished) . competitionState . entityVal

displayCompetition :: Competition -> String
displayCompetition competition = name ++ ", " ++ date
  where
    name = unpack $ competitionName competition
    date = showDay $ competitionDate competition

instance ToJSON Score where
  toJSON score = object
    [ "rid" .= (String $ toPathPiece $ scoreRoundId score)
    , "hid" .= (String $ toPathPiece $ scoreHoleId score)
    , "score" .= (show $ scoreScore score)
    ]

instance ToJSON Round where
  toJSON round_ = object
    [ "uid" .= (String $ toPathPiece $ roundUserId round_)
    , "cid" .= (String $ toPathPiece $ roundCompetitionId round_)
    , "state" .= (show $ roundState round_)
    , "number" .= (show $ roundRoundnumber round_)
    , "group" .= (show $ roundGroupnumber round_)
    ]

instance ToJSON Division where
  toJSON division = object
    [ "division" .= (show $ division)
    ]
