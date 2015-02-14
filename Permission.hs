{-# LANGUAGE TemplateHaskell #-}
module Permission where

import Prelude

import Database.Persist.TH

data Permission =
    AddCompetition
  | EditCompetition
  | AddCourse
  | EditCourse
  | AddNotification
  | EditNotification
  | AddSerie
  | BypassFullCompetitionCheck
  deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "Permission"