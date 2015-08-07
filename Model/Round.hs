module Model.Round where

import Import
import Database.Persist.TH
import Model.RoundState (RoundState(Started))

buildRound :: UserId -> CompetitionId -> Int -> Int -> Round
buildRound uid cid roundNumber groupNumber =
  Round uid cid Started roundNumber groupNumber 0