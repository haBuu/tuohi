{-# LANGUAGE TemplateHaskell #-}
module Handler.CompetitionState where

import Database.Persist.TH
import Prelude

data CompetitionState = Init | Started | Finished
    deriving (Show, Read, Eq)

derivePersistField "CompetitionState"