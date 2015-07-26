{-# LANGUAGE TemplateHaskell #-}
module Handler.Division where

import Prelude

import Database.Persist.TH

data Division = MPO | FPO | MPM | MJ1 | MJ2 | MA2 | MA3
  deriving (Show, Read, Eq, Enum, Bounded, Ord)

derivePersistField "Division"

defaultDivisions :: [Division]
defaultDivisions = [MPO, FPO, MJ2, MPM]