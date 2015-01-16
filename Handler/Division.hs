{-# LANGUAGE TemplateHaskell #-}
module Handler.Division where

import Database.Persist.TH
import Prelude
import Data.Text

data Division = MPO | FPO
    deriving (Show, Read, Eq)

derivePersistField "Division"