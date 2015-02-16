module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import Handler.RoundState
import Handler.CompetitionState
import qualified Handler.CompetitionState as C
import Handler.Division
import Data.Time(Day, UTCTime)
import Yesod.Form.Fields(Textarea)

import Helpers
import Permission

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

isFinished :: Entity Competition -> Bool
isFinished = (== C.Finished) . competitionState . entityVal

displayCompetition :: Competition -> String
displayCompetition competition = name ++ ", " ++ date
  where
    name = unpack $ competitionName competition
    date = showDay $ competitionDate competition

instance ToJSON Score where
  toJSON score = object
    [ "rid"      .= (String $ toPathPiece $ scoreRoundId score)
    , "hid"   .= (String $ toPathPiece $ scoreHoleId score)
    , "score" .= (show $ scoreScore score)
    ]

instance ToJSON User where
  toJSON user = object
    [ "name"      .= (show $ userName user)
    , "email"   .= (show $ userEmail user)
    ]

instance ToJSON Round where
  toJSON round = object
    [ "uid"      .= (String $ toPathPiece $ roundUserId round)
    , "cid"   .= (String $ toPathPiece $ roundCompetitionId round)
    , "state" .= (show $ roundState round)
    , "number" .= (show $ roundRoundnumber round)
    , "group" .= (show $ roundGroupnumber round)
    ]

instance ToJSON Division where
  toJSON division = object
    [ "division" .= (show $ division)
    ]