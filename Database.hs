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
import Data.Time(Day)

isAdmin :: Handler Bool
isAdmin = do
  muser <- maybeAuthUser
  return $ case muser of
    Just user -> userAdmin user
    Nothing -> False

isSuperAdmin :: Handler Bool
isSuperAdmin = do
  muser <- maybeAuthUser
  return $ case muser of
    Just user -> userSuperAdmin user
    Nothing -> False

maybeAuthUser :: Handler (Maybe User)
maybeAuthUser = do
  maid <- maybeAuthId
  case maid of
    Just aid -> runDB $ get aid
    Nothing -> return Nothing

updateUser :: UserId -> Text -> Text -> Handler ()
updateUser uid name email = runDB $
  update uid [UserName =. name, UserEmail =. email]

getNotifications :: Handler [Entity Notification]
getNotifications = runDB $ selectList []
  [Desc NotificationDate, LimitTo 5]

-- select competitions with state Init and corresponding sign ups
-- with user names for each competition
competitionsAndSignUps :: Handler [(Entity Competition, [(E.Value (Key SignUp), E.Value Bool, E.Value Division, E.Value Text)])]
competitionsAndSignUps = do
  competitions <- runDB $ selectList [CompetitionState ==. Init]
    [Asc CompetitionDate]
  forM competitions $ \competition -> do
    signUps <- signUpsWithName $ entityKey competition
    return (competition, signUps)

-- select sign ups for given competition with user name
signUpsWithName :: CompetitionId
  -> Handler [(E.Value (Key SignUp), E.Value Bool, E.Value Division, E.Value Text)]
signUpsWithName cid = runDB $ E.select $
  E.from $ \(signUp `E.InnerJoin` user) -> do
    E.on $ signUp ^. SignUpUserId E.==. user ^. UserId
    E.where_ $ signUp ^. SignUpCompetitionId E.==. E.val cid
    return
      ( signUp ^. SignUpId
      , signUp ^. SignUpConfirmed
      , signUp ^. SignUpDivision
      , user ^. UserName
      )

maybeInsertSignUp :: Bool -> CompetitionId -> Text -> Text -> Division
  -> Handler (Maybe SignUpId)
maybeInsertSignUp checkFull cid name email division = do
  -- if competition is full and checkFull is true return Nothing
  full <- competitionFull cid
  if checkFull && full
    then return Nothing
    else insertSignUp cid name email division

-- insert new user or get existing user's id and insert sign up
insertSignUp :: CompetitionId -> Text -> Text -> Division
  -> Handler (Maybe SignUpId)
insertSignUp cid name email division = do
  pid <- insertUser name email
  runDB $ insertUnique $ SignUp pid cid False division

-- either insert new user or get id from existing user
insertUser :: Text -> Text -> Handler UserId
insertUser name email = do
  eitherUser <- runDB $ insertBy $
    User name email Nothing Nothing False False False
  return $ case eitherUser of
    Left (Entity uid _) -> uid
    Right uid -> uid

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
    runDB $ insertBy $ Round (signUpUserId signup) cid R.Started 1 groupNumber

insertLayout :: Layout -> Int -> Handler ()
insertLayout layout holes = do
  -- insert layout
  mlid <- runDB $ insertUnique layout
  -- insert holes
  case mlid of
    Just lid -> forM_ [1..holes] $ \n -> runDB $ insertUnique $ Hole lid n 3
    Nothing -> return ()

-- select started rounds for given competition with user names
roundsWithNames :: CompetitionId
  -> Handler [(E.Value (Key Round), E.Value Int, E.Value Int, E.Value Text)]
roundsWithNames cid = runDB $ E.select $
  E.from $ \(round_ `E.InnerJoin` user) -> do
    E.on $ round_ ^. RoundUserId E.==. user ^. UserId
    E.where_ $ round_ ^. RoundCompetitionId E.==. E.val cid
    E.where_ $ round_ ^. RoundState E.==. E.val R.Started
    E.orderBy [E.asc (round_ ^. RoundGroupnumber)]
    return
      ( round_ ^. RoundId
      , round_ ^. RoundRoundnumber
      , round_ ^. RoundGroupnumber
      , user ^. UserName
      )

-- select dnf rounds with user names
dnfRoundsWithNames :: CompetitionId
  -> Handler [(E.Value (Key Round), E.Value Text)]
dnfRoundsWithNames cid = runDB $ E.select $
  E.from $ \(round_ `E.InnerJoin` user) -> do
    E.on $ round_ ^. RoundUserId E.==. user ^. UserId
    E.where_ $ round_ ^. RoundCompetitionId E.==. E.val cid
    E.where_ $ round_ ^. RoundState E.==. E.val R.DidNotFinish
    E.orderBy [E.asc (round_ ^. RoundGroupnumber)]
    return
      ( round_ ^. RoundId
      , user ^. UserName
      )

-- select rounds for given competition and group with user names
groupWithNames :: CompetitionId -> Int
  -> Handler [(E.Value (Key Round), E.Value Int, E.Value Int, E.Value Text)]
groupWithNames cid groupNumber = runDB $ E.select $
  E.from $ \(round_ `E.InnerJoin` user) -> do
    E.on $ round_ ^. RoundUserId E.==. user ^. UserId
    E.where_ $ round_ ^. RoundCompetitionId E.==. E.val cid
    E.where_ $ round_ ^. RoundState E.==. E.val R.Started
    E.where_ $ round_ ^. RoundGroupnumber E.==. E.val groupNumber
    return
      ( round_ ^. RoundId
      , round_ ^. RoundRoundnumber
      , round_ ^. RoundGroupnumber
      , user ^. UserName
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
        rounds <- runDB $ playerRoundsAndScores (roundUserId r) cid
        return (roundUserId r, True, rounds)
      let sortedPlayers = playerSort holes players
      -- insert round for each player
      forM_ (zip sortedPlayers groups_) $ \((pid, _, _), groupNumber) -> do
        runDB $ insertBy $ Round pid cid R.Started
          (roundNumber + 1) groupNumber

-- TODO: handicaps, final results etc.
finishCompetition :: CompetitionId -> Handler ()
finishCompetition cid = do
  mroundNumber <- currentRound cid
  -- finish competition
  runDB $ update cid [CompetitionState =. Finished]
  case mroundNumber of
    Just roundNumber -> do
      -- finish rounds
      runDB $ updateWhere
        [ RoundCompetitionId ==. cid
        , RoundRoundnumber ==. roundNumber
        , RoundState ==. R.Started]
        [RoundState =. R.Finished]
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

-- returns users, rounds and scores for given competition
-- return type may seem a bit complicated but it is very
-- convenient in the hamlet template
-- function flow: signups -> users -> rounds -> scores
playersAndScores :: CompetitionId
  -> Handler [(User, Division, [(Round, [Score])])]
playersAndScores cid = runDB $ selectList
  [SignUpCompetitionId ==. cid, SignUpConfirmed ==. True] []
  >>= mapM (\(Entity _ signUp) -> do
    let uid = signUpUserId signUp
    user <- get404 uid
    rounds <- playerRoundsAndScores uid cid
    return (user, signUpDivision signUp, rounds))

playerRoundsAndScores uid cid = selectList
  [RoundUserId ==. uid, RoundCompetitionId ==. cid]
  [Asc RoundRoundnumber]
  >>= mapM (\entity@(Entity rid round_) -> do
    scores <- selectList [ScoreRoundId ==. rid] []
    return (round_, map entityVal scores))

-- returns list where each item is one competition for the player
-- and that competition consist of par of the layout that was played
-- rounds and corresponding scores
handicapScores :: UserId -> CourseId -> Day -> Handler [(Int, [(Round, [Score])])]
handicapScores uid coid date = runDB $ do
  layouts <- selectList [LayoutCourseId ==. coid] []
  -- layout ids
  let lids = map entityKey layouts
  holes <- selectList [HoleLayoutId <-. lids] []
  -- competitions with given course and before date
  competitions <- selectList
    [ CompetitionLayoutId <-. lids
    , CompetitionDate <=. date]
    []
  forM competitions $ \(Entity cid competition) -> do
    -- layout id for this competion
    let lid = competitionLayoutId competition
        -- holes matching that layout
        holes_ = filter (\(Entity _ hole) -> holeLayoutId hole == lid) holes
        par = countPar holes_
    rounds <- finishedRounds uid cid
    return (par, rounds)

finishedRounds uid cid = selectList
  [ RoundUserId ==. uid
  , RoundCompetitionId ==. cid
  , RoundState ==. R.Finished]
  [Asc RoundRoundnumber]
  >>= mapM (\entity@(Entity rid round_) -> do
    scores <- selectList [ScoreRoundId ==. rid] []
    return (round_, map entityVal scores))

holeCount lid = runDB $ count [HoleLayoutId ==. lid]
scoreCount rid = runDB $ count [ScoreRoundId ==. rid]