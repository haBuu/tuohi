{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth
import Yesod.Auth.Email
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

import Handler.Forms

import Data.Time
import Data.List(nub, nubBy, find, sortBy)
import Data.Maybe(fromJust)
import Debug.Trace
import Control.Monad

import qualified Database.Esqueleto as E
import Database.Esqueleto((^.))

import Handler.CompetitionState
import Handler.RoundState(RoundState(DidNotFinish))
import Handler.Division
import Competition.Competition

import Database

import Text.Hamlet

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  competitions <- runDB $ selectList [CompetitionState !=. Finished]
    [Asc CompetitionDate]

  maid <- maybeAuthId
  liftIO $ print maid

  defaultLayout $ do
    setTitle "WeeklyApp"
    let loginWidget = $(widgetFile "signin-signup")
    let headerWidget = $(widgetFile "header")
    $(widgetFile "home")

getAdminR :: Handler Html
getAdminR = do
  competitions <- runDB $ selectList
    [CompetitionState !=. Finished] [Asc CompetitionDate]
  finished <- runDB $ selectList [CompetitionState ==. Finished]
    [Asc CompetitionDate, LimitTo 10]
  defaultLayout $ do
    setTitle "Admin"
    $(widgetFile "admin")

getNewCompetitionR :: Handler Html
getNewCompetitionR = do
  (formWidget, formEnctype) <- generateFormPost newCompetitionForm
  defaultLayout $ do
    -- jquery datepicker
    addScript $ StaticR js_jquery_mobile_datepicker_js
    addStylesheet $ StaticR css_jquery_mobile_datepicker_css
    setTitle "Add new competition"
    mmsg <- getMessage
    $(widgetFile "newcompetition")

postNewCompetitionR :: Handler Html
postNewCompetitionR = do
  ((result, formWidget), formEnctype) <- runFormPost newCompetitionForm
  liftIO $ print result
  case result of
    FormSuccess res -> do
      runDB $ insert_ res
      setMessage "Kisa lisättiin onnistuneesti"
    FormFailure err -> setMessage $ toHtml $ head err
    FormMissing -> setMessage "No mitä vittua?"
  redirect AdminR

getCoursesR :: Handler Html
getCoursesR = do
  (courseFormWidget, courseFormEnctype) <- generateFormPost $
    identifyForm "course" newCourseForm
  (layoutFormWidget, layoutFormEnctype) <- generateFormPost $
    identifyForm "layout" newLayoutForm2
  layouts <- runDB $ selectList [] [Asc LayoutName]
  courses <- runDB $ selectList [] [Asc CourseName]
  defaultLayout $ do
    -- jquery datepicker
    addScript $ StaticR js_jquery_mobile_datepicker_js
    addStylesheet $ StaticR css_jquery_mobile_datepicker_css
    setTitle "Add new course"
    mmsg <- getMessage
    $(widgetFile "courses")

-- FIXME
postCoursesR :: Handler Html
postCoursesR = do
  ((courseResult, _), _) <- runFormPost $
    identifyForm "course" newCourseForm
  ((layoutResult, _), _) <- runFormPost $
    identifyForm "layout" newLayoutForm2
  case courseResult of
    FormSuccess res -> do
      runDB $ insert_ res
      setMessage "Rata lisättiin onnistuneesti"
    FormFailure err -> setMessage $ toHtml $ head err
    FormMissing -> setMessage "No mitä vittua?"
  case layoutResult of
    FormSuccess res -> do
      --runDB $ insert_ res
      uncurry insertLayout res
      setMessage "Layout lisättiin onnistuneesti"
    FormFailure err -> setMessage $ toHtml $ head err
    FormMissing -> setMessage "No mitä vittua?"
  redirect CoursesR

getCourseR :: CourseId -> Handler Html
getCourseR cid = do
  (formWidget, formEnctype) <- generateFormPost $ newLayoutForm cid
  layouts <- runDB $ selectList
    [LayoutCourseId ==. cid]
    [Asc LayoutName]
  defaultLayout $ do
    -- jquery datepicker
    addScript $ StaticR js_jquery_mobile_datepicker_js
    addStylesheet $ StaticR css_jquery_mobile_datepicker_css
    setTitle "Add new layout"
    mmsg <- getMessage
    $(widgetFile "course")

postCourseR :: CourseId -> Handler Html
postCourseR cid = do
  ((result, _), _) <- runFormPost $ newLayoutForm cid
  liftIO $ print result
  case result of
    FormSuccess res -> do
      lid <- runDB $ insert $ fst res
      -- insert given number of holes with par set to 3
      let holes = for [1..(snd res)] $ \n -> Hole lid n 3
      _ <- runDB $ insertMany holes
      setMessage "Layout lisättiin onnistuneesti"
    FormFailure err -> setMessage $ toHtml $ head err
    FormMissing -> setMessage "No mitä vittua?"
  redirect $ CourseR cid

getCompetitionR :: CompetitionId -> Handler Html
getCompetitionR cid = do
  competition <- runDB $ get404 cid
  case competitionState competition of
    Init -> initPage cid
    Started -> startedPage cid
    Finished -> finishedPage cid

initPage :: CompetitionId -> Handler Html
initPage cid = do
  signups <- signUpsWithName cid
  (formWidget, formEnctype) <- generateFormPost $
    startCompetitionForm cid
  defaultLayout $ do
    setTitle "Competition"
    $(widgetFile "init")
    $(widgetFile "competitioninit")

startedPage :: CompetitionId -> Handler Html
startedPage cid = do
  (nextRoundFormWidget, nextRoundFormEnctype) <- generateFormPost $
    identifyForm "nextround" $ nextRoundForm cid
  (finishCompetitionFormWidget, finishCompetitionFormEnctype) <- generateFormPost $
    identifyForm "finish" $ finishCompetitionForm cid
  rounds <- roundsWithNames cid
  -- for hamlet so it can put dividers between groups
  let groups = nub $ for rounds $ \(_,_,E.Value g,_) -> g
  defaultLayout $ do
    setTitle "Competition"
    $(widgetFile "started")

-- TODO
finishedPage :: CompetitionId -> Handler Html
finishedPage cid = do
  competition <- runDB $ get404 cid
  defaultLayout $ do
    setTitle "Competition"
    $(widgetFile "finished")

postCompetitionNextRoundR :: CompetitionId -> Handler Html
postCompetitionNextRoundR cid = do
  ((result, _), _) <- runFormPost $ nextRoundForm cid
  case result of
    FormSuccess res -> do
      nextRound res
      setMessage "Kierros vaihdettiin onnistuneesti"
    FormFailure err -> setMessage $ toHtml $ head err
    FormMissing -> setMessage "No mitä vittua?"
  redirect $ CompetitionR cid

postCompetitionFinishR :: CompetitionId -> Handler Html
postCompetitionFinishR cid = do
  ((result, _), _) <- runFormPost $ finishCompetitionForm cid
  case result of
    FormSuccess res -> do
      finishCompetition res
      setMessage "Kisa lopetettiin onnistuneesti"
    FormFailure err -> setMessage $ toHtml $ head err
    FormMissing -> setMessage "No mitä vittua?"
  redirect $ CompetitionR cid

postCompetitionR :: CompetitionId -> Handler Html
postCompetitionR cid = do
  ((result, _), _) <- runFormPost $ startCompetitionForm cid
  case result of
    FormSuccess res -> do
      startCompetition res
      setMessage "Kisa aloitettiin onnistuneesti"
    FormFailure err -> setMessage $ toHtml $ head err
    FormMissing -> setMessage "No mitä vittua?"
  redirect $ CompetitionR cid

postConfirmSignUpR :: SignUpId -> Handler Html
postConfirmSignUpR sid = do
  runDB $ update sid [SignUpConfirmed =. True]
  redirect AdminR

postRemoveSignUpR :: SignUpId -> Handler Html
postRemoveSignUpR sid = do
  runDB $ delete sid
  redirect AdminR

postDnfRoundR :: RoundId -> Handler Html
postDnfRoundR rid = do
  runDB $ update rid [RoundState =. DidNotFinish]
  redirect AdminR

getEditCourseR :: CourseId -> Handler Html
getEditCourseR cid = do
  redirect AdminR

postEditCourseR :: CourseId -> Handler Html
postEditCourseR cid = do
  redirect AdminR

getLayoutR :: CourseId -> LayoutId -> Handler Html
getLayoutR cid lid = do
  (formWidget, formEnctype) <- generateFormPost $ holesForm lid
  holes <- runDB $ selectList [HoleLayoutId ==. lid] [Asc HoleNumber]
  defaultLayout $ do
    setTitle "Layout"
    mmsg <- getMessage
    $(widgetFile "layout")

postLayoutR :: CourseId -> LayoutId -> Handler Html
postLayoutR cid lid = do
  redirect $ LayoutR cid lid

getSignUpR :: CompetitionId -> Handler Html
getSignUpR cid = do
  -- if the competition does not exist return 404
  competition <- runDB $ get404 cid
  full <- competitionFull cid
  mplayer <- maybeAuthPlayer
  ((_, formWidget), formEnctype) <- case mplayer of
    -- user is logged in
    Just player -> runFormPost $ signUpFormLoggedIn cid player
    -- user is not logged in
    Nothing -> runFormPost $ signUpForm cid
  signups <- signUpsWithName cid
  defaultLayout $ do
    setTitle "Sign ups"
    mmsg <- getMessage
    --addScriptRemote "https://www.google.com/recaptcha/api.js"
    $(widgetFile "signup")

postSignUpR :: CompetitionId -> Handler Html
postSignUpR cid = do
  mplayer <- maybeAuthPlayer
  ((result, _), _) <- case mplayer of
    -- user is logged in
    Just player -> runFormPost $ signUpFormLoggedIn cid player
    -- user is not logged in
    Nothing -> runFormPost $ signUpForm cid
  case result of
    FormSuccess (name, email, division) -> do
      msid <- maybeInsertSignUp cid name email division
      case msid of
        Just sid -> setMessage "Ilmoittautuminen onnistui"
        Nothing -> setMessage "Ilmoittautuminen epäonnistui"
    FormFailure err -> setMessage $ toHtml $ head err
    FormMissing -> setMessage "No mitä vittua?"
  redirect $ SignUpR cid

postScoreR :: RoundId -> HoleId -> Handler Html
postScoreR rid hid = do
  round_ <- runDB $ get404 rid
  player <- runDB $ get404 $ roundPlayerId round_
  ((result, _), _) <- runFormPost $ scoreForm hid rid (playerName player)
  -- TODO: test that the refactored code works
  formHandler result $ \res -> do
    mScore <- runDB $ getBy $
      UniqueScore (scoreRoundId res) (scoreHoleId res)
    case mScore of
      Just (Entity sid _) -> runDB $ replace sid res
      Nothing -> runDB $ insert_ res
  -- case result of
  --   FormSuccess res -> do
  --     mScore <- runDB $ getBy $
  --       UniqueScore (scoreRoundId res) (scoreHoleId res)
  --     case mScore of
  --       Just (Entity sid _) -> runDB $ replace sid res
  --       Nothing -> runDB $ insert_ res
  --   FormFailure err -> setMessage $ toHtml $ head err
  --   FormMissing -> setMessage "No mitä vittua?"
  redirect $ InputR (roundCompetitionId round_) (roundGroupnumber round_)

getInputR :: CompetitionId -> Int -> Handler Html
getInputR cid groupNumber = do
  competition <- runDB $ get404 cid
  let lid = competitionLayoutId competition
  holes <- runDB $ selectList
    [HoleLayoutId ==. lid] [Asc HoleNumber]
  group <- groupWithNames cid groupNumber
  holesAndForms <- forM holes $ \(Entity hid hole) -> do
    forms <- forM group $ \(E.Value rid, _, _, E.Value name) ->
      generateFormPost $ scoreForm hid rid name
    return (holeNumber hole, forms)
  defaultLayout $ do
    $(widgetFile "style")
    $(widgetFile "input")

getGroupsR :: CompetitionId -> Handler Html
getGroupsR cid = do
  rounds <- roundsWithNames cid
  -- for hamlet so it can put dividers between groups
  let groups = nub $ for rounds $ \(_,_,E.Value g,_) -> g
  defaultLayout $ do
    $(widgetFile "groups")

getLiveR :: Handler Html
getLiveR = do
  mcompetition <- runDB $ selectFirst
    [CompetitionState ==. Started] [Asc CompetitionDate]
  case mcompetition of
    Nothing -> notFound
    Just (Entity cid competition) -> do
      let lid = competitionLayoutId competition
      holes <- runDB $ selectList
        [HoleLayoutId ==. lid] [Asc HoleNumber]
      let layoutPar = countPar holes
      curRound <- currentRound cid
      let roundCount = maybe 1 id curRound
      players <- playersAndScores cid
      let sortedPlayers = playerSort holes players
      defaultLayout $ do
        $(widgetFile "style")
        $(widgetFile "live")

getProfileR :: Handler Html
getProfileR = do
  maid <- maybeAuthId
  mplayer <- maybeAuthPlayer
  case mplayer of
    Just player -> do
      ((_, formWidget), formEnctype) <- runFormPost $ profileForm player
      defaultLayout $ do
        mmsg <- getMessage
        setTitle "WeeklyApp"
        let loginWidget = $(widgetFile "signin-signup")
        let headerWidget = $(widgetFile "header")
        $(widgetFile "profile")
    -- this can't never happen because this handler
    -- is never reached if the user is not authenticated
    Nothing -> redirect HomeR

postProfileR :: Handler Html
postProfileR = do
  maid <- maybeAuthId
  mplayer <- maybeAuthPlayer
  case mplayer of
    Just player -> do
      ((result, _), _) <- runFormPost $ profileForm player
      formHandler result $ \(name, email) ->
        case maid of
          Just aid -> do
            updateUser aid name email
            setMessage "Profiilin päivitys onnistui"
          Nothing -> setMessage "Profiilin päivitys epäonnistui"
    -- see getProfileR
    Nothing -> return ()
  redirect ProfileR

-- form handler with default action for FormFailure and FormMissing
formHandler :: FormResult a -> (a -> Handler ()) -> Handler ()
formHandler result f =
  case result of
    FormSuccess res -> f res
    FormFailure err -> setMessage $ toHtml $ head err
    FormMissing -> setMessage "No mitä vittua?"