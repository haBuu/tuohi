module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.Email
import Yesod.Auth.Message
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
import Data.List(find)
import Data.Text(Text, isInfixOf)
import Control.Monad (join)
import Helpers

import qualified Yesod.Auth.Message as Msg
import qualified Yesod.Form.I18n.Finnish as FF

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
-- yesod will overwrite this if it detects finnish
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
  makeSessionBackend _
    | development = session
    | otherwise = sslOnlySessions session
    where
      session = fmap Just $ defaultClientSessionBackend
        120 -- timeout in minutes
        "config/client_session_key.aes"

  yesodMiddleware
    | development = defaultYesodMiddleware
    | otherwise = (sslOnlyMiddleware 120) . defaultYesodMiddleware

  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    maid <- maybeAuthId
    muser <- case maid of
      Just aid -> runDB $ get aid
      Nothing -> return Nothing

    pc <- widgetToPageContent $ do
      addStylesheet $ StaticR css_bootstrap_css
      $(widgetFile "style")
      -- jquery
      addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
      -- addScriptRemote "//ajax.googleapis.com/ajax/libs/jqueryui/1.11.2/jquery-ui.min.js"
      addScript $ StaticR js_bootstrap_js
      $(widgetFile "header")
      $(widgetFile "message")
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  -- This is done to provide an optimization for serving static files from
  -- a separate domain. Please see the staticRoot setting in Settings.hs
  urlRenderOverride y (StaticR s) =
    Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
  urlRenderOverride _ _ = Nothing

  -- The page to be redirected to when authentication is required.
  authRoute _ = Just $ HomeR

  -- Routes not requiring authentication.
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized

  -- public
  isAuthorized HomeR _ = return Authorized
  isAuthorized (GroupsR _) _ = return Authorized
  isAuthorized (SignUpR _) _ = return Authorized
  isAuthorized ScoresR _ = return Authorized
  isAuthorized (CompetitionScoresR _) _ = return Authorized
  isAuthorized (TempAuthR _) _ = return Authorized

  -- user
  isAuthorized ProfileR _ = isUser

  -- admin
  isAuthorized AdminR _ = isAdmin
  isAuthorized NewCompetitionR _ = isAdmin
  isAuthorized CoursesR _ = isAdmin
  isAuthorized SeriesR _ = isAdmin
  isAuthorized (CourseR _) _ = isAdmin
  isAuthorized (LayoutR _ _) _ = isAdmin
  isAuthorized (CompetitionR _) _ = isAdmin
  isAuthorized (ConfirmSignUpR _) _ = isAdmin
  isAuthorized (RemoveSignUpR _) _ = isAdmin
  isAuthorized (DnfRoundR _) _ = isAdmin
  isAuthorized (CompetitionNextRoundR _) _ = isAdmin
  isAuthorized (CompetitionFinishR _) _ = isAdmin
  isAuthorized NotificationsR _ = isAdmin
  isAuthorized (NotificationR _) _ = isAdmin
  isAuthorized (AddPlayerR _) _ = isAdmin

  -- super admin
  isAuthorized UsersR _ = isSuperAdmin
  isAuthorized (UserR _) _ = isSuperAdmin

  -- Default to Authorized for now.
  -- TODO: REMOVE THIS
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
  mr <- getMessageRender
  maid <- maybeAuthId
  case maid of
    Just aid -> do
      muser <- runDB $ get aid
      return $ case muser of
        Just user ->
          if userAdmin user
            -- user is admin,
            then Authorized
            -- user is not admin
            else Unauthorized $ mr MsgNotAdmin
        -- logged in user was not found in db
        -- this should and can't never happen
        Nothing -> Unauthorized $ mr MsgNotAdmin
    -- not logged in
    Nothing -> return AuthenticationRequired

isSuperAdmin = do
  mr <- getMessageRender
  maid <- maybeAuthId
  case maid of
    Just aid -> do
      muser <- runDB $ get aid
      return $ case muser of
        Just user ->
          if userSuperAdmin user
            -- user is super admin,
            then Authorized
            -- user is not super admin
            else Unauthorized $ mr MsgNotSuperAdmin
        -- logged in user was not found in db
        -- this should and can't never happen
        Nothing -> Unauthorized $ mr MsgNotSuperAdmin
    -- not logged in
    Nothing -> return AuthenticationRequired

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
  type AuthId App = UserId

  renderAuthMessage _ langs =
    case fmap (isInfixOf "fi") (safeHead langs) of
      Just True -> finnishMessage
      _ -> defaultMessage

  -- Where to send a user after successful login
  loginDest _ = HomeR
  -- Where to send a user after logout
  logoutDest _ = HomeR

  getAuthId creds = runDB $ do
    x <- getBy $ UniqueUser (credsIdent creds)
    return $ case x of
      Just (Entity uid _) -> Just uid
      Nothing -> Nothing

  -- You can add other plugins like BrowserID, email or OAuth here
  authPlugins _ = [authEmail]

  authHttpManager = httpManager

  loginHandler = do
    tp <- getRouteToParent
    lift $ authLayout $ do
      setTitleI Msg.LoginTitle
      myLoginHandler tp

instance YesodAuthPersist App

instance YesodAuthEmail App where
  type AuthEmailId App = UserId

  afterPasswordRoute _ = HomeR

  addUnverified email verkey =
    runDB $ insert $
      User "" email Nothing (Just verkey) False False False

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

  getVerifyKey = runDB . fmap (join . fmap userVerkey) . get
  setVerifyKey uid key = runDB $ update uid [UserVerkey =. Just key]

  verifyAccount uid = runDB $ do
    muser <- get uid
    case muser of
      Nothing -> return Nothing
      Just u -> do
        update uid [UserVerified =. True]
        return $ Just uid

  getPassword = runDB . fmap (join . fmap userPassword) . get
  setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]

  getEmailCreds email = runDB $ do
    muser <- getBy $ UniqueUser email
    case muser of
      Nothing -> return Nothing
      Just (Entity uid user) -> return $ Just EmailCreds
          { emailCredsId = uid
          , emailCredsAuthId = Just uid
          , emailCredsStatus = isJust $ userPassword user
          , emailCredsVerkey = userVerkey user
          , emailCredsEmail = email
          }

  getEmail = runDB . fmap (fmap userEmail) . get

  registerHandler = myRegisterHandler
  confirmationEmailSentResponse = myConfirmationEmailSentResponse
  setPasswordHandler = mySetPasswordHandler
  forgotPasswordHandler = myForgotPasswordHandler

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage _ langs =
    case fmap (isInfixOf "fi") (safeHead langs) of
      Just True -> FF.finnishMessage
      _ -> defaultFormMessage

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

-- own implementations for nicer ui
myRegisterHandler :: YesodAuthEmail master => AuthHandler master Html
myRegisterHandler = do
  tp <- getRouteToParent
  lift $ authLayout $ do
    setTitleI Msg.RegisterLong
    [whamlet|
      <div .container>
        <h3>_{Msg.EnterEmail}
        <form method="post" action="@{tp registerR}">
          <div .form-group>
            <label .control-label>_{Msg.Email}:
            <input .form-control type="email" name="email" autofocus>
          <div .form-group>
            <input type=submit .btn .btn-default .btn-block value=_{Msg.Register}>
    |]

myConfirmationEmailSentResponse :: Text -> HandlerT App IO TypedContent
myConfirmationEmailSentResponse identifier = do
  mr <- getMessageRender
  selectRep $ do
    provideJsonMessage (mr msg)
    provideRep $ authLayout $ do
      setTitleI Msg.ConfirmationEmailSentTitle
      [whamlet|
        <div .container>
          <h3>_{msg}
      |]
  where
    msg = Msg.ConfirmationEmailSent identifier

-- TODO: type?
-- mySetPasswordHandler :: YesodAuthEmail master => Bool -> AuthHandler master TypedContent
mySetPasswordHandler needOld = do
  tp <- getRouteToParent
  mr <- lift getMessageRender
  selectRep $ do
    provideJsonMessage $ mr Msg.SetPass
    provideRep $ lift $ authLayout $ do
      setTitleI Msg.SetPassTitle
      [whamlet|
        <div .container>
          <h3>_{Msg.SetPass}
          <form method="post" action="@{tp setpassR}">
            $if needOld
              <div .form-group>
                <label .control-label>_{MsgCurrentPassword}
                <input .form-control type="password" name="current" autofocus>
            <div .form-group>
              <label .control-label>_{Msg.NewPass}
              <input .form-control type="password" name="new" :not needOld:autofocus>
            <div .form-group>
              <label .control-label>_{Msg.ConfirmPass}
              <input .form-control type="password" name="confirm">
            <div .form-group>
              <input type="submit" .btn .btn-default .btn-block value=_{Msg.SetPassTitle}>
      |]

myForgotPasswordHandler :: YesodAuthEmail master => AuthHandler master Html
myForgotPasswordHandler = do
  tp <- getRouteToParent
  lift $ authLayout $ do
    setTitleI Msg.PasswordResetTitle
    [whamlet|
      <div .container>
        <h3>_{Msg.PasswordResetPrompt}
        <form method="post" action="@{tp forgotPasswordR}">
          <div .form-group>
            <label .control-label>_{Msg.ProvideIdentifier}
            <input .form-control type="text" name="email" autofocus>
          <div .form-group>
            <input type=submit .btn .btn-default .btn-block value=_{Msg.SendPasswordResetEmail}>
    |]

myLoginHandler tm =
  [whamlet|
    <div .container>
      <form method="post" action="@{tm loginR}">
        <div .form-group>
          <label .control-label>_{Msg.Email}
          <input .form-control type="email" name="email" required>
        <div .form-group>
          <label .control-label>_{Msg.Password}
          <input .form-control type="password" name="password" required>
        <div .form-group>
          <input type=submit .btn .btn-success .btn-block value=_{Msg.LoginViaEmail}>
      <a href="@{tm registerR}" .btn .btn-default .btn-block>_{Msg.RegisterLong}
  |]