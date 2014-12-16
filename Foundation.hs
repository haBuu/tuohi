module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.Email
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlBackend)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Yesod.Core.Types (Logger)
import Yesod.Form.Jquery

-- email auth needs these
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Network.Mail.Mime
import Text.Shakespeare.Text (stext)
import qualified Data.Text.Lazy.Encoding
import Data.Maybe (isJust)
import Control.Monad (join)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { settings :: AppConfig DefaultEnv Extra
  , getStatic :: Static -- ^ Settings for static file serving.
  , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
  , httpManager :: Manager
  , persistConfig :: Settings.PersistConf
  , appLogger :: Logger
  }

instance HasHttpManager App where
  getHttpManager = httpManager

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  approot = ApprootMaster $ appRoot . settings

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
    120    -- timeout in minutes
    "config/client_session_key.aes"

  defaultLayout widget = do
    master <- getYesod
    --mmsg <- getMessage

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    pc <- widgetToPageContent $ do
      addStylesheet $ StaticR css_bootstrap_css
      $(widgetFile "style")
      -- jquery
      addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
      addScriptRemote "//ajax.googleapis.com/ajax/libs/jqueryui/1.11.2/jquery-ui.min.js"
      addScriptRemote "//ajax.googleapis.com/ajax/libs/jquerymobile/1.4.3/jquery.mobile.min.js"
      addStylesheetRemote "//ajax.googleapis.com/ajax/libs/jquerymobile/1.4.3/jquery.mobile.min.css"
      $(widgetFile "mobileinit")
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  -- This is done to provide an optimization for serving static files from
  -- a separate domain. Please see the staticRoot setting in Settings.hs
  urlRenderOverride y (StaticR s) =
    Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
  urlRenderOverride _ _ = Nothing

  -- The page to be redirected to when authentication is required.
  authRoute _ = Just $ AuthR LoginR

  -- Routes not requiring authentication.
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized

  -- user profile
  isAuthorized ProfileR _ = isUser

  -- admin ui
  isAuthorized AdminR _ = isAdmin
  isAuthorized NewCompetitionR _ = isAdmin
  isAuthorized CoursesR _ = isAdmin
  isAuthorized (CourseR _) _ = isAdmin
  isAuthorized (EditCourseR _) _ = isAdmin
  isAuthorized (LayoutR _ _) _ = isAdmin
  isAuthorized (CompetitionR _) _ = isAdmin
  isAuthorized (ConfirmSignUpR _) _ = isAdmin
  isAuthorized (RemoveSignUpR _) _ = isAdmin
  isAuthorized (DnfRoundR _) _ = isAdmin
  isAuthorized (CompetitionNextRoundR _) _ = isAdmin
  isAuthorized (CompetitionFinishR _) _ = isAdmin

  -- Default to Authorized for now.
  isAuthorized _ _ = return Authorized

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent =
    addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
    where
      -- Generate a unique filename based on the content itself
      genFileName lbs
          | development = "autogen-" ++ base64md5 lbs
          | otherwise   = base64md5 lbs

  -- Place Javascript at bottom of the body tag so the rest of the page loads first
  jsLoader _ = BottomOfBody

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLog _ _source level =
    development || level == LevelWarn || level == LevelError

  makeLogger = return . appLogger

isUser = do
  maid <- maybeAuthId
  return $ case maid of
    Just _ -> Authorized
    Nothing -> AuthenticationRequired

isAdmin = do
  maid <- maybeAuthId
  case maid of
    Just aid -> do
      mplayer <- runDB $ get aid
      return $ case mplayer of
        Just player ->
          if playerAdmin player
            -- user is admin,
            then Authorized
            -- user is not admin
            else Unauthorized "You must be an admin"
        -- logged in user was not found in db
        -- this should and can't never happen
        Nothing -> Unauthorized "You must be an admin"
    -- not logged in
    Nothing -> return AuthenticationRequired

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
  --type AuthId App = UserId
  type AuthId App = PlayerId

  -- Where to send a user after successful login
  loginDest _ = HomeR
  -- Where to send a user after logout
  logoutDest _ = HomeR

  getAuthId creds = runDB $ do
    x <- getBy $ UniquePlayer (credsIdent creds)
    return $ case x of
      Just (Entity pid _) -> Just pid
      Nothing -> Nothing

  -- You can add other plugins like BrowserID, email or OAuth here
  authPlugins _ = [authEmail]

  authHttpManager = httpManager

instance YesodAuthPersist App

instance YesodAuthEmail App where
  type AuthEmailId App = PlayerId

  afterPasswordRoute _ = HomeR

  addUnverified email verkey =
    runDB $ insert $ Player "" email Nothing (Just verkey) False False

  sendVerifyEmail email _ verurl = do
    liftIO $ print verurl
    liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
      { mailTo = [Address Nothing email]
      , mailHeaders =
          [ ("Subject", "Verify your email address")
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
              Please confirm your email address by clicking on the link below.

              #{verurl}

              Thank you
            |]
        , partHeaders = []
        }
      htmlPart = Part
        { partType = "text/html; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partContent = renderHtml
            [shamlet|
              <p>Please confirm your email address by clicking on the link below.
              <p>
                  <a href=#{verurl}>#{verurl}
              <p>Thank you
            |]
        , partHeaders = []
        }

  getVerifyKey = runDB . fmap (join . fmap playerVerkey) . get
  setVerifyKey uid key = runDB $ update uid [PlayerVerkey =. Just key]

  verifyAccount uid = runDB $ do
    muser <- get uid
    case muser of
      Nothing -> return Nothing
      Just u -> do
        update uid [PlayerVerified =. True]
        return $ Just uid

  getPassword = runDB . fmap (join . fmap playerPassword) . get
  setPassword uid pass = runDB $ update uid [PlayerPassword =. Just pass]

  getEmailCreds email = runDB $ do
    muser <- getBy $ UniquePlayer email
    case muser of
      Nothing -> return Nothing
      Just (Entity pid player) -> return $ Just EmailCreds
          { emailCredsId = pid
          , emailCredsAuthId = Just pid
          , emailCredsStatus = isJust $ playerPassword player
          , emailCredsVerkey = playerVerkey player
          , emailCredsEmail = email
          }

  getEmail = runDB . fmap (fmap playerEmail) . get

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

instance YesodJquery App