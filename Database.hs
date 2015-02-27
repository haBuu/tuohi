{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Database where

import Import

import qualified Database.Esqueleto as E
import Database.Esqueleto((^.))

import Handler.CompetitionState
import qualified Handler.RoundState as R
import Handler.Division
import Competition.Groups
import Competition.Competition
import Permission
import Helpers(today)

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
maybeAuthUser = liftM (fmap entityVal) maybeAuth

check :: Permission -> Handler ()
check permission = do
  muser <- maybeAuthUser
  case muser of
    Just user -> do
      if elem permission $ userPermissions user
        then return ()
        else permissionDeniedI MsgMissingPermission
    Nothing -> notAuthenticated

setPermissions :: UserId -> [Permission] -> Handler ()
setPermissions uid permissions = runDB $
  update uid [UserPermissions =. permissions]

getActiveSignUps :: UserId
  -> Handler [(E.Value SignUpId, E.Value CompetitionId, E.Value Text, E.Value Day)]
getActiveSignUps uid = do
  today_ <- liftIO today
  -- let today_ = fromGregorian 2014 1 1
  runDB $ E.select $
    E.from $ \(competition `E.InnerJoin` signUp) -> do
      E.on $ competition ^. CompetitionId E.==. signUp ^. SignUpCompetitionId
      E.where_ $ signUp ^. SignUpUserId E.==. E.val uid
      E.where_ $ competition ^. CompetitionState E.==. E.val Init
      E.where_ $ competition ^. CompetitionDate E.>=. E.val today_
      E.orderBy [E.asc (competition ^. CompetitionDate)]
      return
        ( signUp ^. SignUpId
        , competition ^. CompetitionId
        , competition ^. CompetitionName
        , competition ^. CompetitionDate
        )

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
    User name email Nothing Nothing False False False []
  return $ case eitherUser of
    Left (Entity uid _) -> uid
    Right uid -> uid

-- returns true if the competition is full
competitionFull :: CompetitionId -> Handler Bool
competitionFull cid = runDB $ do
  competition <- get404 cid
  -- how many players are allowed in the competition
  let playerLimit = competitionPlayerLimit competition
  -- how many players have signed up for the competition
  signups <- count [SignUpCompetitionId ==. cid]
  return $ signups >= playerLimit

startCompetition :: CompetitionId -> Handler ()
startCompetition cid = runDB $ do
  competition <- get404 cid
  -- set state to started
  update cid [CompetitionState =. Started]
  -- get sign ups for the competition that are confirmed
  confirmed <- selectList
    [SignUpConfirmed ==. True, SignUpCompetitionId ==. cid] []
  -- count holes in the layout
  let lid = competitionLayoutId competition
  holes <- count [HoleLayoutId ==. lid]
  -- make groups
  let groups_ = groups holes $ length confirmed
  -- make a round for each confirmed sign up
  forM_ (zip confirmed groups_) $ \((Entity _ signup), groupNumber) ->
    void $ insertBy $
      Round (signUpUserId signup) cid R.Started 1 groupNumber

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
    Just roundNumber -> runDB $ do
      updateWhere
        [ RoundCompetitionId ==. cid
        , RoundRoundnumber ==. roundNumber
        , RoundState ==. R.Started]
        [RoundState =. R.Finished]
      rounds <- selectList
        [ RoundCompetitionId ==. cid
        , RoundRoundnumber ==. roundNumber
        , RoundState ==. R.Finished]
        []
      competition <- get404 cid
      -- competiton layout id
      let lid = competitionLayoutId competition
      holes <- selectList [HoleLayoutId ==. lid] [Asc HoleNumber]
      -- make groups
      let groups_ = groups (length holes) (length rounds)
      -- get players and scores so we can put them in order
      players <- forM rounds $ \(Entity _ r) -> do
        scores <- playerRoundsAndScores (roundUserId r) cid
        -- MPO is dummy
        return (roundUserId r, MPO, scores)
      let sortedPlayers = playerSort holes players
      -- insert round for each player
      forM_ (zip sortedPlayers groups_) $ \((pid, _, _), groupNumber) ->
        void $ insertBy $ Round pid cid R.Started
          (roundNumber + 1) groupNumber

finishCompetition :: CompetitionId -> Handler ()
finishCompetition cid = do
  mroundNumber <- currentRound cid
  -- finish competition
  runDB $ do
    update cid [CompetitionState =. Finished]
    case mroundNumber of
      Just roundNumber -> do
        -- finish rounds
        updateWhere
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
handicapScores :: UserId -> SerieId -> Day -> Handler [(Int, [(Round, [Score])])]
handicapScores uid sid date = runDB $ do
  -- competitions with given serie and before date
  competitions <- selectList
    [ CompetitionSerieId ==. Just sid
    , CompetitionDate <=. date]
    []
  unfiltered <- forM competitions $ \(Entity cid competition) -> do
    -- layout id for this competion
    let lid = competitionLayoutId competition
    holes <- selectList [HoleLayoutId ==. lid] []
    let par = countPar holes
    rounds <- finishedRounds uid cid
    return (par, rounds)
  -- filter out competitions which the player did not attented
  filterM (return . not . null . snd) unfiltered

finishedRounds uid cid = selectList
  [ RoundUserId ==. uid
  , RoundCompetitionId ==. cid
  , RoundState ==. R.Finished]
  [Asc RoundRoundnumber]
  >>= mapM (\(Entity rid round_) -> do
    scores <- selectList [ScoreRoundId ==. rid] []
    return (round_, map entityVal scores))

holeCount :: LayoutId -> Handler Int
holeCount lid = runDB $ count [HoleLayoutId ==. lid]
scoreCount :: RoundId -> Handler Int
scoreCount rid = runDB $ count [ScoreRoundId ==. rid]