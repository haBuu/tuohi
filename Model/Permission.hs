{-# LANGUAGE TemplateHaskell #-}
module Model.Permission where

import Prelude
import Database.Persist.TH

data PermissionType =
    AddCompetition
  | EditCompetition
  | AddCourse
  | EditCourse
  | AddNotification
  | EditNotification
  | AddSerie
  | BypassFullCompetitionCheck
  deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "PermissionType"