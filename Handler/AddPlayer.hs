module Handler.AddPlayer where

import Import
import Database
import Handler.Forms

getAddPlayerR :: CompetitionId -> Handler Html
getAddPlayerR cid = do
  -- if the competition does not exist return 404
  competition <- runDB $ get404 cid
  users <- runDB $ selectList [] [Asc UserName]
  ((_, formWidget), formEnctype) <- runFormPost addPlayerForm
  defaultLayout $ do
    setTitleI MsgAddPlayer
    $(widgetFile "addplayer")

postAddPlayerR :: CompetitionId -> Handler Html
postAddPlayerR cid = do
  ((result, _), _) <- runFormPost addPlayerForm
  formHandler result $ \(name, email, division) -> do
    let checkFull = False
    msid <- maybeInsertSignUp checkFull cid name email division
    case msid of
      Just _ -> setMessageI MsgPlayerAdded
      Nothing -> setMessageI MsgAddPlayerFail
  redirect $ CompetitionR cid