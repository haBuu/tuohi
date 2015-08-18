module Handler.AddPenalty where

import Import

postAddPenaltyR :: CompetitionId -> RoundId -> Handler Html
postAddPenaltyR cid rid = do
  value <- runInputPost $ ireq (checkPenalty intField) "penalty"
  round_ <- runDB $ do
    -- update penalty for the round
    update rid [RoundPenalty =. value]
    -- get round for the redirect
    get404 rid
  redirect $ ScoresInputR cid $ roundRoundnumber round_

-- check that penalty is positive integer
checkPenalty :: Field Handler Int -> Field Handler Int
checkPenalty = checkBool ((>=) 0) MsgPenaltyRangeError
