{-# LANGUAGE TemplateHaskell #-}
module DivisionMessages where

import Import

import Handler.Division

divisions :: [(Division, AppMessage)]
divisions = map (\d -> (d, divisionMsg d)) [minBound..]

divisionMsg :: Division -> AppMessage
divisionMsg division =
  case division of
    MPO -> MsgMPO
    FPO -> MsgFPO
