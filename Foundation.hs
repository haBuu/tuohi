module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.Email
import Yesod.Auth.Message
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

-- email auth needs these
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Network.Mail.Mime
import Text.Shakespeare.Text (stext)
import qualified Data.Text.Lazy.Encoding

import qualified Yesod.Auth.Message as Msg
import qualified Yesod.Form.I18n.Finnish as FF

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { appSettings    :: AppSettings
  , appStatic      :: Static -- ^ Settings for static file serving.
  , appConnPool    :: ConnectionPool -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger      :: Logger
  }

instance HasHttpManager App where
  getHttpManager = appHttpManager

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

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

development :: Bool
development =
#if DEVELOPMENT
  True
#else
  False
#endif

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot = ApprootMaster $ appRoot . appSettings

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is one month
  makeSessionBackend _
    | development = session
    | otherwise = session
    -- | otherwise = sslOnlySessions session
    where
      session = fmap Just $ defaultClientSessionBackend
        (60 * 24 * 30) -- timeout in minutes, one month
        "config/client_session_key.aes"

  yesodMiddleware
    | development = defaultYesodMiddleware
    | otherwise = defaultYesodMiddleware
    -- | otherwise = (sslOnlyMiddleware 120) . defaultYesodMiddleware

  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    muser <- maybeAuth

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
      addScript $ StaticR js_bootstrap_js
      $(widgetFile "header")
      $(widgetFile "message")
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  -- The page to be redirected to when authentication is required.
  authRoute _ = Just $ HomeR

  -- Routes not requiring authentication.
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized
  isAuthorized (StaticR _) _ = return Authorized

  -- public
  isAuthorized HomeR _ = return Authorized
  isAuthorized (GroupsR _) _ = return Authorized
  isAuthorized (SignUpsR _) _ = return Authorized
  isAuthorized (ScoresR _) _ = return Authorized
  isAuthorized (LatestScoreTimeR _) _ = return Authorized
  isAuthorized (ResultsR _) _ = return Authorized
  isAuthorized (HandicapsR _) _ = return Authorized
  isAuthorized (ScoreR _ _ _) _ = return Authorized
  isAuthorized (CompetitionAuthR _) _ = return Authorized
  isAuthorized FinishedCompetitionsR _ = return Authorized
  isAuthorized LanguageR _ = return Authorized
  isAuthorized InfoR _ = return Authorized
  isAuthorized (InputR _ _) _ = return Authorized

  -- user
  isAuthorized ProfileR _ = isUser
  isAuthorized (SignUpR _) _ = isUser

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
  isAuthorized (RevertDnfRoundR _) _ = isAdmin
  isAuthorized (CompetitionNextRoundR _) _ = isAdmin
  isAuthorized (CompetitionFinishR _) _ = isAdmin
  isAuthorized NotificationsR _ = isAdmin
  isAuthorized (NotificationR _) _ = isAdmin
  isAuthorized (AddPlayerR _) _ = isAdmin
  isAuthorized (ScoreLogR _) _ = isAdmin
  isAuthorized (ScoreEditPlayersR _) _ = isAdmin
  isAuthorized (ScoreEditPlayerR _ _) _ = isAdmin
  isAuthorized (ScoreEditR _ _ _) _ = isAdmin
  isAuthorized (ScoresInputR _ _) _ = isAdmin
  isAuthorized (ScoreInputR _ _ _) _ = isAdmin
  isAuthorized (EditCompetitionR _) _ = isAdmin
  isAuthorized (ChangeGroupR _ _) _ = isAdmin
  isAuthorized (ImportPlayersR _) _ = isAdmin
  isAuthorized (ExportScoresR _) _ = isAdmin
  isAuthorized (AddPDGAPlayerR _) _ = isAdmin
  isAuthorized (LockCompetitionR _) _ = isAdmin
  isAuthorized (AddPenaltyR _ _) _ = isAdmin

  -- super admin
  isAuthorized UsersR _ = isSuperAdmin
  isAuthorized (UserR _) _ = isSuperAdmin
  isAuthorized PermissionsR _ = isSuperAdmin
  isAuthorized EventLogR _ = isSuperAdmin

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      -- Generate a unique filename based on the content itself
      genFileName lbs = "autogen-" ++ base64md5 lbs

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLog app _source level =
    appShouldLogAll (appSettings app)
      || level == LevelWarn
      || level == LevelError

  makeLogger = return . appLogger

isUser :: Handler AuthResult
isUser = do
  maid <- maybeAuthId
  return $ case maid of
    Just _ -> Authorized
    Nothing -> AuthenticationRequired

isAdmin :: Handler AuthResult
isAdmin = do
  mr <- getMessageRender
  muser <- maybeAuth
  return $ case muser of
    Just (Entity _ user) ->
      if userAdmin user
        -- user is admin,
        then Authorized
        -- user is not admin
        else Unauthorized $ mr MsgNotAdmin
    -- not logged in
    Nothing -> AuthenticationRequired

isSuperAdmin :: Handler AuthResult
isSuperAdmin = do
  mr <- getMessageRender
  muser <- maybeAuth
  return $ case muser of
    Just (Entity _ user) ->
      if userSuperAdmin user
        -- user is super admin,
        then Authorized
        -- user is not super admin
        else Unauthorized $ mr MsgNotSuperAdmin
    -- not logged in
    Nothing -> AuthenticationRequired

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = UserId

  renderAuthMessage _ [] = defaultMessage
  renderAuthMessage _ ("en":_) = defaultMessage
  renderAuthMessage _ ("fi":_) = finnishMessage
  renderAuthMessage m (_:ls) = renderAuthMessage m ls

  -- Where to send a user after successful login
  loginDest _ = HomeR
  -- Where to send a user after logout
  logoutDest _ = HomeR
  -- Override the above two destinations when a Referer: header is present
  redirectToReferer _ = True

  authenticate creds = runDB $ do
    x <- getBy $ UniqueUser $ Just $ credsIdent creds
    return $ case x of
      Just (Entity uid _) -> Authenticated uid
      Nothing -> UserError InvalidLogin

  -- You can add other plugins like BrowserID, email or OAuth here
  authPlugins _ = [authEmail]

  authHttpManager = getHttpManager

  loginHandler = do
    tp <- getRouteToParent
    lift $ authLayout $ do
      setTitleI Msg.LoginTitle
      myLoginHandler tp

instance YesodAuthPersist App

instance YesodAuthEmail App where
  type AuthEmailId App = UserId

  afterPasswordRoute _ = HomeR

  addUnverified email verkey = runDB $ insert $
    User "N/A" (Just email)
      Nothing -- password
      (Just verkey) -- verkey
      False -- verified
      False -- admin
      False -- super admin
      True -- real user
      Nothing -- pdga number

  sendVerifyEmail email _ verurl = do
    liftIO $ print verurl
    liftIO $ renderSendMail (emptyMail $ Address (Just "Tampereen Frisbeeseura") "noreply@kisa.tfs.fi")
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
      Just _ -> do
        update uid [UserVerified =. True]
        return $ Just uid

  getPassword = runDB . fmap (join . fmap userPassword) . get
  setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]

  getEmailCreds email = runDB $ do
    muser <- getBy $ UniqueUser $ Just email
    case muser of
      Nothing -> return Nothing
      Just (Entity uid user) -> return $ Just EmailCreds
        { emailCredsId = uid
        , emailCredsAuthId = Just uid
        , emailCredsStatus = isJust $ userPassword user
        , emailCredsVerkey = userVerkey user
        , emailCredsEmail = email
        }

  getEmail = runDB . fmap (join . fmap userEmail) . get

  registerHandler = myRegisterHandler
  confirmationEmailSentResponse = myConfirmationEmailSentResponse
  setPasswordHandler = mySetPasswordHandler
  forgotPasswordHandler = myForgotPasswordHandler

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage _ [] = defaultFormMessage
  renderMessage _ ("en":_) = defaultFormMessage
  renderMessage _ ("fi":_) = FF.finnishMessage
  renderMessage m (_:ls) = renderMessage m ls

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

-- own implementations for nicer ui
myRegisterHandler :: YesodAuthEmail master => AuthHandler master Html
myRegisterHandler = do
  tp <- getRouteToParent
  lift $ authLayout $ do
    setTitleI Msg.RegisterLong
    [whamlet|
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
        <h3>_{msg}
      |]
  where
    msg = Msg.ConfirmationEmailSent identifier

mySetPasswordHandler needOld = do
  tp <- getRouteToParent
  mr <- lift getMessageRender
  selectRep $ do
    provideJsonMessage $ mr Msg.SetPass
    provideRep $ lift $ authLayout $ do
      setTitleI Msg.SetPassTitle
      [whamlet|
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
