{-# LANGUAGE TemplateHaskell #-}
module Handler.Division where

import Prelude

import Database.Persist.TH

data Division = MPO | FPO | MJ2 | MPM
  deriving (Show, Read, Eq, Enum, Bounded, Ord)

derivePersistField "Division"