module Email
( sendEmails
) where

import Import
import Network.Mail.Mime
import Data.Time.Clock
import Text.Shakespeare.Text (stext)
import qualified Data.Text.Lazy.Encoding
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Database.Persist.Sqlite(ConnectionPool, runSqlPool)

import qualified Database.Esqueleto as E
import Database.Esqueleto((^.))
import Database

sendEmails :: ConnectionPool -> IO ()
sendEmails pool = do
  sendReminderEmails pool

-- select emails that should receive reminder
reminderEmails :: SqlPersistT IO [E.Value Text]
reminderEmails = E.select $
  E.from $ \(user `E.InnerJoin` signUpEmailReminder) -> do
    E.on $ user ^. UserId E.==. signUpEmailReminder ^. UserId
    return $ user ^. UserEmail

sendReminderEmails :: ConnectionPool -> IO ()
sendReminderEmails pool = runSqlPool reminderEmails pool
  >>= mapM_ (\(E.Value email) -> sendReminderEmail email)

sendReminderEmail :: Text -> IO ()
sendReminderEmail email = do
  print $ "Sending email to: " ++ show email
  renderSendMail $ reminderMail email

reminderMail :: Text -> Mail
reminderMail email = (emptyMail $ Address Nothing "noreply")
  { mailTo = [Address Nothing email]
  , mailHeaders =
      [ ("Subject", "Test")
      ]
  , mailParts = [[textPart, htmlPart]]
  }
  where
    textPart = Part
      { partType = "text/plain; charset=utf-8"
      , partEncoding = None
      , partFilename = Nothing
      , partContent = Data.Text.Lazy.Encoding.encodeUtf8
          [stext|
            Test
          |]
      , partHeaders = []
      }
    htmlPart = Part
      { partType = "text/html; charset=utf-8"
      , partEncoding = None
      , partFilename = Nothing
      , partContent = renderHtml
          [shamlet|
            <p>Test
          |]
      , partHeaders = []
      }