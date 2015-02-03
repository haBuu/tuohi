{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
( makeApplication
, getApplicationDev
, makeFoundation
, startProductionApp
)
where

import Import
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Database.Persist
import Database.Persist.Sql (runMigrationUnsafe, runMigration, runSqlPool, printMigration)
import Database.Persist.Sqlite (createSqlitePool, sqlDatabase, sqlPoolSize)
import Network.HTTP.Client.Conduit (newManager)
import Control.Monad.Logger (runLoggingT, LogLevel(LevelError), liftLoc)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize, toLogStr)
import Network.Wai.Logger (clockDateCacher)
import Data.Default (def)
import Yesod.Core.Types (loggerSet, Logger (Logger))

-- production
import Control.Monad
import Language.Haskell.TH.Syntax (qLocation)
import Network.Wai.Handler.WarpTLS
import Network.Wai.Handler.Warp

import Database

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.Profile
import Handler.Admin
import Handler.Notifications
import Handler.NewCompetition
import Handler.Competition
import Handler.Layout
import Handler.SignUp
import Handler.Series
import Handler.TempAuth
import Handler.Scores
import Handler.Groups
import Handler.Input
import Handler.Courses
import Handler.Course
import Handler.User
import Handler.Users
import Handler.Language
import Handler.Results
import Handler.Info
import Handler.AddPlayer

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication conf = do
  foundation <- makeFoundation conf

  -- Initialize the logging middleware
  logWare <- mkRequestLogger def
    { outputFormat =
        if development
            then Detailed True
            else Apache FromSocket
    , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
    }

  -- Create the WAI application and apply middlewares
  app <- toWaiAppPlain foundation
  let logFunc = messageLoggerSource foundation (appLogger foundation)
  return (logWare $ defaultMiddlewaresNoLogging app, logFunc)

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
  manager <- newManager
  s <- staticSite
  dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
            Database.Persist.loadConfig >>=
            Database.Persist.applyEnv

  loggerSet' <- newStdoutLoggerSet defaultBufSize
  (getter, _) <- clockDateCacher

  let logger = Yesod.Core.Types.Logger loggerSet' getter
      mkFoundation p = App
        { settings = conf
        , getStatic = s
        , connPool = p
        , httpManager = manager
        , persistConfig = dbconf
        , appLogger = logger
        }
      tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
      logFunc = messageLoggerSource tempFoundation logger

  p <- flip runLoggingT logFunc
     $ createSqlitePool (sqlDatabase dbconf) (sqlPoolSize dbconf)
  let foundation = mkFoundation p

  -- Perform database migration using our application's logging settings.
  flip runLoggingT logFunc
    (Database.Persist.runPool dbconf (runMigration migrateAll) p)

  flip runSqlPool p $ do
    let pw = Just "sha256|14|vRb1oHsjxtKhQ/ftDnRc0w==|wJA1uiJK/o8q+zvcBeYlRoHP9iVSPcRfg4efqiuxrSs="
        verKey = Just "yAqw2iRbi7"
    _ <- insertUnique $ User "super admin" "super@super" pw verKey True True True
    return ()
  return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
  defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
      { csParseExtra = parseExtra
      }

startProductionApp :: IO ()
startProductionApp = do
  let tls = tlsSettings "config/certificate.pem" "config/key.pem"
  config <- fromArgs parseExtra
  (app, logFunc) <- makeApplication config
  runTLS tls defaultSettings
    { settingsPort = appPort config
    , settingsHost = appHost config
    , settingsOnException = const $ \e -> when (shouldLog' e) $ logFunc
      $(qLocation >>= liftLoc)
      "yesod"
      LevelError
      (toLogStr $ "Exception from Warp: " ++ show e)
    } app
  where
    shouldLog' = defaultShouldDisplayException
