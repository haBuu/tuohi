module Handler.ChangeGroup where

import Import
import Data.Text
import Data.Text.Read
import Data.Either

postChangeGroupR :: CompetitionId -> RoundId -> Handler Html
postChangeGroupR cid rid = do
  mgroup <- lookupPostParam "group"
  case mgroup of
    Just groupText -> do
      case decimal groupText of
        Right (group, _) -> runDB $ update rid [RoundGroupnumber =. group]
        Left _ -> invalidArgs [] -- bad parameter
    Nothing -> invalidArgs []-- missing parameter
  redirect $ CompetitionR cid