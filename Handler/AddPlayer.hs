module Handler.AddPlayer where

import Import
import Database
import Handler.Forms
import Handler.Division
import qualified Model.RoundState as R
import qualified Model.CompetitionState as C
import Model.User
import Model.Round

getAddPlayerR :: CompetitionId -> Handler Html
getAddPlayerR cid = do
  -- if the competition does not exist return 404
  competition <- runDB $ get404 cid
  users <- runDB $ selectList [UserRealUser ==. True] [Asc UserName]
  ((_, formWidget), formEnctype) <- runFormPost $ addPlayerForm cid
  defaultLayout $ do
    setTitleI MsgAddPlayer
    addScript $ StaticR js_jets_min_js
    $(widgetFile "addplayer")
    search

search :: Widget
search = toWidget [julius|
  var jets = new Jets({
    searchTag: '#player-search',
    contentTag: '#player-content'
  });
|]

getAddPDGAPlayerR :: CompetitionId -> Handler Html
getAddPDGAPlayerR cid = do
  -- if the competition does not exist return 404
  competition <- runDB $ get404 cid
  users <- runDB $ selectList [UserRealUser ==. True] [Asc UserName]
  ((_, formWidget), formEnctype) <- runFormPost $ addPDGAPlayerForm cid
  defaultLayout $ do
    setTitleI MsgAddPlayer
    $(widgetFile "addplayer")

-- TODO: check that the division is allowed in the competition
postAddPlayerR :: CompetitionId -> Handler Html
postAddPlayerR cid = do
  ((result, _), _) <- runFormPost $ addPlayerForm cid
  formHandler result $ \(name, email, division) -> do
    added <- maybeAddNewPlayer cid name email division
    case added of
      True -> setMessageI MsgPlayerAdded
      False -> setMessageI MsgAddPlayerFail
  redirect $ CompetitionR cid

-- TODO: check that the division is allowed in the competition
postAddPDGAPlayerR :: CompetitionId -> Handler Html
postAddPDGAPlayerR cid = do
  ((result, _), _) <- runFormPost $ addPDGAPlayerForm cid
  formHandler result $ \(name, mPdga, division) -> do
    added <- maybeAddPDGAPlayer cid name mPdga division
    case added of
      True -> setMessageI MsgPlayerAdded
      False -> setMessageI MsgAddPlayerFail
  redirect $ CompetitionR cid

-- adds new player to competition if conditions are met
-- competition can't be finished
-- if competition is started round must be one
-- returns true if the player was added, false otherwise
maybeAddNewPlayer :: CompetitionId -> Text -> Text -> Division
  -> Handler Bool
maybeAddNewPlayer cid name email division = do
  -- check that current round is one or return 400
  mround <- runDB $ currentRound cid
  case mround of
    -- no round started so everything is good
    Nothing -> return ()
    -- round one started so everything is good
    Just 1 -> return ()
    -- higher round started so can't add new player
    _ -> invalidArgsI [MsgNotRoundOne]
  competition <- runDB $ get404 cid
  case competitionState competition of
    -- can't add player to finished competition
    C.Finished -> return False
    -- if competition is not started only add sign up
    C.Init -> do
      msid <- insertSignUp cid name email division
      return $ isJust msid
    -- if competition is started we also need to add player to a group
    C.Started -> do
      uid <- insertUser name email
      msid <- runDB $ insertUnique $ SignUp uid cid True division
      case msid of
        Just _ -> do
          mgroup <- addToCompetition uid cid
          return $ isJust mgroup
        Nothing -> return False

maybeAddPDGAPlayer :: CompetitionId -> Text -> Maybe Int -> Division
  -> Handler Bool
maybeAddPDGAPlayer cid name mPdga division = do
  -- check that current round is one or return 400
  mround <- runDB $ currentRound cid
  case mround of
    -- no round started so everything is good
    Nothing -> return ()
    -- round one started so everything is good
    Just 1 -> return ()
    -- higher round started so can't add new player
    _ -> invalidArgsI [MsgNotRoundOne]
  competition <- runDB $ get404 cid
  case competitionState competition of
    -- can't add player to finished competition
    C.Finished -> return False
    -- if competition is not started only add sign up
    C.Init -> do
      uid <- runDB $ insertUserNoEmail name mPdga
      msid <- runDB $ insertUnique $ SignUp uid cid True division
      return $ isJust msid
    -- if competition is started we also need to add player to a group
    C.Started -> do
      uid <- runDB $ insertUserNoEmail name mPdga
      msid <- runDB $ insertUnique $ SignUp uid cid True division
      case msid of
        Just _ -> do
          mgroup <- addToCompetition uid cid
          return $ isJust mgroup
        Nothing -> return False

-- add player to group one and round one
addToCompetition :: UserId -> CompetitionId -> Handler (Maybe RoundId)
addToCompetition uid cid = runDB $ insertUnique $ buildRound uid cid 1 1