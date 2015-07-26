{-# LANGUAGE TemplateHaskell #-}
module DivisionMessages where

import Import

import Handler.Division

allDivisions :: [(Division, AppMessage)]
allDivisions = map (\d -> (d, divisionMsg d)) [minBound..]

withMessages :: [Division] -> [(Division, AppMessage)]
withMessages = map (\d -> (d, divisionMsg d))

divisionMsg :: Division -> AppMessage
divisionMsg division =
  case division of
    MPO -> MsgMPO
    FPO -> MsgFPO
    MPM -> MsgMPM
    MJ1 -> MsgMJ1
    MJ2 -> MsgMJ2
    MA2 -> MsgMA2
    MA3 -> MsgMA3
