{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Database where

import Import
import Yesod.Auth

import Control.Monad

import qualified Database.Esqueleto as E
import Database.Esqueleto((^.))

import Handler.CompetitionState
import qualified Handler.RoundState as R
import Handler.Division
import Competition.Groups
import Competition.Competition
import Data.List(nub, nubBy, find, sortBy)

maybeAuthPlayer :: Handler (Maybe Player)
maybeAuthPlayer = do
  maid <- maybeAuthId
  case maid of
    Just aid -> do
      mplayer <- runDB $ get aid
      return mplayer
    Nothing -> return Nothing

updateUser :: PlayerId -> Text -> Text -> Handler ()
updateUser pid name email = runDB $ do
  update pid [PlayerName =. name]
  update pid [PlayerEmail =. email]

-- select competitions with state Init and corresponding sign ups with player names for each competition
competitionsAndSignUps :: Handler [(Entity Competition, [(E.Value (Key SignUp), E.Value Bool, E.Value Division, E.Value Text)])]
competitionsAndSignUps = do
  competitions <- runDB $ selectList [CompetitionState ==. Init] [Asc CompetitionDate]
  forM competitions $ \competition -> do
    signUps <- signUpsWithName $ entityKey competition
    return (competition, signUps)

-- select sign ups for given competition with player name
signUpsWithName :: CompetitionId
  -> Handler [(E.Value (Key SignUp), E.Value Bool, E.Value Division, E.Value Text)]
signUpsWithName cid = runDB $ E.select $
  E.from $ \(signUp `E.InnerJoin` player) -> do
    E.on $ signUp ^. SignUpPlayerId E.==. player ^. PlayerId
    E.where_ $ signUp ^. SignUpCompetitionId E.==. E.val cid
    return
      ( signUp ^. SignUpId
      , signUp ^. SignUpConfirmed
      , signUp ^. SignUpDivision
      , player ^. PlayerName
      )

maybeInsertSignUp :: CompetitionId -> Text -> Text -> Division
  -> Handler (Maybe SignUpId)
maybeInsertSignUp cid name email division = do
  -- if competition is full return Nothing
  full <- competitionFull cid
  if full then return Nothing
    else insertSignUp cid name email division

-- insert new player or get existing player's id and insert sign up
insertSignUp :: CompetitionId -> Text -> Text -> Division
  -> Handler (Maybe SignUpId)
insertSignUp cid name email division = do
  pid <- insertPlayer name email
  runDB $ insertUnique $ SignUp pid cid False division

-- either insert new player or get id from existing player
insertPlayer :: Text -> Text -> Handler PlayerId
insertPlayer name email = do
  eitherPlayer <- runDB $ insertBy $ Player name email Nothing Nothing False False
  return $ case eitherPlayer of
    Left (Entity pid _) -> pid
    Right pid -> pid

-- returns true if the competition is full
competitionFull :: CompetitionId -> Handler Bool
competitionFull cid = runDB $ do
  competition <- get404 cid
  -- how many players is allowed in the competition
  let playerLimit = competitionPlayerLimit competition
  -- how many players have signed up for the competition
  signups <- count [SignUpCompetitionId ==. cid]
  return $ signups >= playerLimit

startCompetition :: CompetitionId -> Handler ()
startCompetition cid = do
  -- set state to started
  runDB $ update cid [CompetitionState =. Started]
  -- get sign ups for the competition that are confirmed
  confirmedSignUps <- runDB $ selectList
    [SignUpConfirmed ==. True, SignUpCompetitionId ==. cid] []
  -- get competition
  competition <- runDB $ get404 cid
  -- competiton layout id
  let lid = competitionLayoutId competition
  -- count holes in the layout
  holes <- runDB $ count [HoleLayoutId ==. lid]
  -- make groups
  let groups_ = groups holes (length confirmedSignUps)
  -- make a round for each confirmed sign up
  forM_ (zip confirmedSignUps groups_ ) $ \((Entity _ signup), groupNumber) ->
    runDB $ insertBy $ Round (signUpPlayerId signup) cid R.Started 1 groupNumber

insertLayout :: Layout -> Int -> Handler ()
insertLayout layout holes = do
  -- insert layout
  mlid <- runDB $ insertUnique layout
  -- insert holes
  case mlid of
    Just lid -> forM_ [1..holes] $ \n -> runDB $ insertUnique $ Hole lid n 3
    Nothing -> return ()

-- select rounds for given competition with player names
roundsWithNames :: CompetitionId
  -> Handler [(E.Value (Key Round), E.Value Int, E.Value Int, E.Value Text)]
roundsWithNames cid = runDB $ E.select $
  E.from $ \(round_ `E.InnerJoin` player) -> do
    E.on $ round_ ^. RoundPlayerId E.==. player ^. PlayerId
    E.where_ $ round_ ^. RoundCompetitionId E.==. E.val cid
    E.where_ $ round_ ^. RoundState E.==. E.val R.Started
    E.orderBy [E.asc (round_ ^. RoundGroupnumber)]
    return
      ( round_ ^. RoundId
      , round_ ^. RoundRoundnumber
      , round_ ^. RoundGroupnumber
      , player ^. PlayerName
      )

-- select rounds for given competition and group with player names
groupWithNames :: CompetitionId -> Int
  -> Handler [(E.Value (Key Round), E.Value Int, E.Value Int, E.Value Text)]
groupWithNames cid groupNumber = runDB $ E.select $
  E.from $ \(round_ `E.InnerJoin` player) -> do
    E.on $ round_ ^. RoundPlayerId E.==. player ^. PlayerId
    E.where_ $ round_ ^. RoundCompetitionId E.==. E.val cid
    E.where_ $ round_ ^. RoundState E.==. E.val R.Started
    E.where_ $ round_ ^. RoundGroupnumber E.==. E.val groupNumber
    return
      ( round_ ^. RoundId
      , round_ ^. RoundRoundnumber
      , round_ ^. RoundGroupnumber
      , player ^. PlayerName
      )

nextRound :: CompetitionId -> Handler ()
nextRound cid = do
  mroundNumber <- currentRound cid
  case mroundNumber of
    Nothing -> return ()
    Just roundNumber -> do
      runDB $ updateWhere
        [ RoundCompetitionId ==. cid
        , RoundRoundnumber ==. roundNumber
        , RoundState ==. R.Started]
        [RoundState =. R.Finished]
      rounds <- runDB $ selectList
        [ RoundCompetitionId ==. cid
        , RoundRoundnumber ==. roundNumber
        , RoundState ==. R.Finished]
        []
      -- get competition
      competition <- runDB $ get404 cid
      -- competiton layout id
      let lid = competitionLayoutId competition
      holes <- runDB $ selectList
        [HoleLayoutId ==. lid] [Asc HoleNumber]
      -- make groups
      let groups_ = groups (length holes) (length rounds)
      -- get players and scores so we can put them in order
      players <- forM rounds $ \(Entity _ r) -> do
        rounds <- runDB $ playerRoundsAndScores (roundPlayerId r) cid
        return (roundPlayerId r, rounds)
      let sortedPlayers = playerSort holes players
      -- insert round for each player
      forM_ (zip sortedPlayers groups_) $ \((pid, _), groupNumber) -> do
        runDB $ insertBy $ Round pid cid R.Started
          (roundNumber + 1) groupNumber

-- TODO: handicaps, final results etc.
finishCompetition :: CompetitionId -> Handler ()
finishCompetition cid = do
  mroundNumber <- currentRound cid
  case mroundNumber of
    Just roundNumber -> do
      -- finish rounds
      runDB $ updateWhere
        [ RoundCompetitionId ==. cid
        , RoundRoundnumber ==. roundNumber
        , RoundState ==. R.Started]
        [RoundState =. R.Finished]
      -- finish competition
      runDB $ update cid [CompetitionState =. Finished]
    Nothing -> return ()

-- returns highest round that has state started from
-- given competition i.e. currently active round
currentRound :: CompetitionId -> Handler (Maybe Int)
currentRound cid = do
  mround <- runDB $ selectFirst
    [RoundCompetitionId ==. cid, RoundState ==. R.Started]
    [Desc RoundRoundnumber]
  return $ case mround of
    Just (Entity _ round_) -> Just $ roundRoundnumber round_
    Nothing -> Nothing

-- returns players, rounds and scores for given competition
-- return type may seem a bit complicated but it is very
-- convenient in the hamlet template
-- function flow: signups -> players -> rounds -> scores
playersAndScores :: CompetitionId
  -> Handler [(Player, [(Round, [Entity Score])])]
playersAndScores cid = runDB $ selectList
  [SignUpCompetitionId ==. cid, SignUpConfirmed ==. True] []
  >>= mapM (\(Entity _ signUp) -> do
    let pid = signUpPlayerId signUp
    player <- get404 pid
    rounds <- playerRoundsAndScores pid cid
    return (player, rounds))

playerRoundsAndScores pid cid = selectList
  [RoundPlayerId ==. pid, RoundCompetitionId ==. cid]
  [Asc RoundRoundnumber]
  >>= mapM (\entity@(Entity rid round_) -> do
    scores <- selectList [ScoreRoundId ==. rid] []
    return (round_, scores))

--productsAndCategories :: GHandler App App [(Product, Maybe Category)]
--productsAndCategories = runDB $ selectList [] [Asc ProductName] >>= mapM (\(Entity _ p) -> do
--    category <- case (productCategory p) of
--        Just c -> do
--            get c
--        Nothing -> return Nothing
--    return (p, category))