{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.SignUp where

import Import
import Yesod.Default.Config(appExtra)

import Data.List(isInfixOf)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad(unless)
import qualified Network.HTTP.Conduit as HTTP

import Handler.Forms
import Handler.Division
import qualified Database.Esqueleto as E
import Database
import DivisionMessages
import Data.Text(unpack)

getSignUpR :: CompetitionId -> Handler Html
getSignUpR cid = do
  -- if the competition does not exist return 404
  competition <- runDB $ get404 cid
  full <- competitionFull cid
  muser <- maybeAuthUser
  ((_, formWidget), formEnctype) <- case muser of
    -- user is logged in
    Just user -> runFormPost $ signUpFormLoggedIn cid user
    -- user is not logged in
    Nothing -> runFormPost $ signUpForm cid
  signups <- signUpsWithName cid
  defaultLayout $ do
    addScriptRemote "https://www.google.com/recaptcha/api.js"
    $(widgetFile "signup")

postSignUpR :: CompetitionId -> Handler Html
postSignUpR cid = do
  muser <- maybeAuthUser
  ((result, _), _) <- case muser of
    -- user is logged in
    Just user -> runFormPost $ signUpFormLoggedIn cid user
    -- user is not logged in
    Nothing -> do
      -- short-circuit recaptcha
      checkRecaptcha >>= flip unless (recaptchaError cid)
      -- if recaptcha fails we won't reach here
      runFormPost $ signUpForm cid
  formHandler result $ \(name, email, division) -> do
    let checkFull = True
    msid <- maybeInsertSignUp checkFull cid name email division
    case msid of
      Just _ -> setMessageI MsgSignUpSuccess
      Nothing -> setMessageI MsgSignUpFail
  redirect $ SignUpR cid

-- recaptcha
checkRecaptcha :: Handler Bool
checkRecaptcha = do
  mrecaptcha <- lookupPostParam "g-recaptcha-response"
  -- get recaptcha secret key
  master <- getYesod
  let mkey = extraRecaptchakey $ appExtra $ settings master
  case (mrecaptcha, mkey) of
    (Just response, Just key) -> do
      req <- HTTP.parseUrl $ verifyUrl key $ unpack response
      res <- HTTP.withManager $ HTTP.httpLbs req
      return $ verifyResponse $ HTTP.responseBody res
    _ -> return False

recaptchaError :: CompetitionId -> Handler ()
recaptchaError cid = do
  setMessageI MsgRecaptchaError
  redirect $ SignUpR cid

verifyResponse :: L.ByteString -> Bool
verifyResponse = isInfixOf "\"success\": true" . C.unpack

verifyUrl :: String -> String -> String
verifyUrl secret res =
  "https://www.google.com/recaptcha/api/siteverify?secret="
    ++ secret ++ "&response=" ++ res