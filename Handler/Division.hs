{-# LANGUAGE TemplateHaskell #-}
module Handler.Division where

import Prelude

import Database.Persist.TH
import Data.Text

data Division = MPO | FPO
  deriving (Show, Read, Eq)

derivePersistField "Division"