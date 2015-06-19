{-# LANGUAGE TemplateHaskell #-}
module Model.EventLog where

import Prelude
import Database.Persist.TH

data Level = Info | Warning | Error
  deriving (Show, Read)

derivePersistField "Level"