{-# LANGUAGE TemplateHaskell #-}
module DivisionMessages where

import Foundation

import Handler.Division

divisions = [(MPO, MsgMPO), (FPO, MsgFPO)]

divisionMsg division =
  case division of
    MPO -> MsgMPO
    FPO -> MsgFPO
