{-# LANGUAGE TemplateHaskell #-}
module Handler.Division where

import Prelude

import Database.Persist.TH

data Division = MPO | FPO
  deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "Division"