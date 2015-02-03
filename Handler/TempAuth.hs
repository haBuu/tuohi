{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.TempAuth where

import Import

import Data.Time
import Data.Text
import Control.Monad

import Handler.Forms
import Database

getTempAuthR :: CompetitionId -> Handler Html
getTempAuthR cid = do
  ((_, formWidget), formEnctype) <- runFormPost tempAuthForm
  defaultLayout $ do
    setTitle "WeeklyApp"
    $(widgetFile "tempauth")

postTempAuthR :: CompetitionId -> Handler Html
postTempAuthR cid = do
  ((result, _), _) <- runFormPost tempAuthForm
  formHandler result $ tempAuth cid
  -- redirect to ultimate destination what should be
  -- the input page where the user wanted to go
  -- default to HomeR if destination is not set
  redirectUltDest HomeR

-- name of the temp session
-- this can be anything
sessionName :: Text
sessionName = "_TMP"

tempAuth :: CompetitionId -> Text -> Handler ()
tempAuth cid pw = do
  -- session can be created only with the current password
  competition <- runDB $ get404 cid
  let password = competitionPassword competition
  when (pw == password) $ do
    setSession sessionName pw

isTempAuth :: CompetitionId -> Handler Bool
isTempAuth cid = do
  competition <- runDB $ get404 cid
  let password = competitionPassword competition
  mSession <- lookupSession sessionName
  return $ case mSession of
    Just pw -> if pw == password then True else False
    _ -> False

-- TODO: change from 4-digit passwords to some standard words that
-- are easier for humans
-- first in the tuple is the current password and second is previous
passwords :: Handler (Text, Text)
passwords = do
  (year1, month1, day1) <- liftIO today
  (year2, month2, day2) <- liftIO yesterday
  let pw1 = mkPassword day1 month1 year1
      pw2 = mkPassword day2 month2 year2
  return (pack $ show pw1, pack $ show pw2)
  where
    mkPassword day month year =
      -- just some random numbers
      mod ((day * 583) + (month * 45) + fromIntegral year) (9999 + 1)

-- returns (year, month, day)
today :: IO (Integer, Int, Int)
today = getCurrentTime
  >>= return . toGregorian . utctDay

yesterday :: IO (Integer, Int, Int)
yesterday = getCurrentTime
  >>= return . toGregorian . addDays (-1) . utctDay