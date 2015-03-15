{-# LANGUAGE TemplateHaskell #-}
module Model.RoundState where

import Database.Persist.TH
import Prelude

data RoundState = Started | Finished | DidNotFinish
    deriving (Show, Read, Eq)

derivePersistField "RoundState"