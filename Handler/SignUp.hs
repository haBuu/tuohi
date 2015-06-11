module Handler.SignUp where

import Import

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Network.HTTP.Conduit as HTTP

import Handler.Forms
import qualified Database.Esqueleto as E
import Database
import DivisionMessages
import Model.CompetitionState

deleteSignUpR :: SignUpId -> Handler Html
deleteSignUpR sid = do
  runDB $ do
    signup <- get404 sid
    competition <- get404 $ signUpCompetitionId signup
    -- allow deleting of signup only from competitions
    -- that are in state Init
    when (competitionState competition == Init) $ delete sid
  redirect HomeR

getSignUpsR :: CompetitionId -> Handler Html
getSignUpsR cid = do
  -- if the competition does not exist return 404
  competition <- runDB $ get404 cid
  full <- competitionFull cid
  muser <- maybeAuthUser
  ((_, formWidget), formEnctype) <- case muser of
    -- user is logged in
    Just user -> runFormPost $ signUpFormLoggedIn user
    -- user is not logged in
    Nothing -> runFormPost signUpForm
  signups <- signUpsWithName cid
  defaultLayout $ do
    addScriptRemote "https://www.google.com/recaptcha/api.js"
    $(widgetFile "signup")

postSignUpsR :: CompetitionId -> Handler Html
postSignUpsR cid = do
  muser <- maybeAuthUser
  ((result, _), _) <- case muser of
    -- user is logged in
    Just user -> runFormPost $ signUpFormLoggedIn user
    -- user is not logged in
    Nothing -> do
      -- short-circuit recaptcha
      checkRecaptcha >>= flip unless (recaptchaError cid)
      -- if recaptcha fails we won't reach here
      runFormPost signUpForm
  formHandler result $ \(name, email, division) -> do
    let checkFull = True
    msid <- maybeInsertSignUp checkFull cid name email division
    case msid of
      Just _ -> setMessageI MsgSignUpSuccess
      Nothing -> setMessageI MsgSignUpFail
  redirect $ SignUpsR cid

-- recaptcha
checkRecaptcha :: Handler Bool
checkRecaptcha = do
  mrecaptcha <- lookupPostParam "g-recaptcha-response"
  -- get recaptcha secret key
  master <- getYesod
  let mkey = appRecaptcha $ appSettings master
  case (mrecaptcha, mkey) of
    (Just response, Just key) -> do
      req <- HTTP.parseUrl $ verifyUrl key $ unpack response
      res <- HTTP.withManager $ HTTP.httpLbs req
      return $ verifyResponse $ HTTP.responseBody res
    _ -> return False

recaptchaError :: CompetitionId -> Handler ()
recaptchaError cid = do
  setMessageI MsgRecaptchaError
  redirect $ SignUpsR cid

verifyResponse :: L.ByteString -> Bool
verifyResponse = isInfixOf "\"success\": true" . C.unpack

verifyUrl :: String -> String -> String
verifyUrl secret res =
  "https://www.google.com/recaptcha/api/siteverify?secret="
    ++ secret ++ "&response=" ++ res