-- |
-- Description: Definition of the app monad 'AppM'.
-- Maintainer: Marco Schneider <marco.schneider@posteo.de>
--
-- This module defines the main app monad 'AppM', as well as the default instances
-- of all relevant typeclasses.
module Housekeeper.App.Monad (AppM, runAppM) where

import Control.Concurrent.MonadIO (MonadIO (..), liftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.UUID.V4 qualified as V4
import Database.PostgreSQL.Simple qualified as PSQL
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Housekeeper.App.Env (Env (..), askLogLevel, withDbConn)
import Housekeeper.Capability.EventStore (EventEnvelope (..), MonadEventStore (..))
import Housekeeper.Capability.Id (MonadId (..))
import Housekeeper.Capability.Logger (LogLevel (..), LogMessage (..), MonadLog (..))
import Housekeeper.Capability.Time (MonadTime (..))
import Housekeeper.Context.ClientManager (Client (..), MonadClientRepo (..))
import Housekeeper.Types (Topic)
import System.Log.Logger qualified as Log

-- | Main application monad.
newtype AppM a = AppM {unAppM :: ReaderT Env IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

-- | Run the monad in 'IO', given some 'Env', producing a value of type 'a'.
runAppM :: AppM a -> Env -> IO a
runAppM app = runReaderT (unAppM app')
  where
    -- Do some initialization first.
    app' = do
      logLevel <- askLogLevel
      initLogger logLevel
      app

instance MonadClientRepo AppM where
  save = saveClientImpl
  find = findClientImpl
  findAll = findAllClientsImpl

saveClientImpl :: Client -> AppM ()
saveClientImpl (Client topic name isArchived) =
  withDbConn
    ( \conn -> do
        -- Discard the result
        _ <-
          liftIO $
            PSQL.execute
              conn
              -- hamfisted 'UPSERT'
              "INSERT INTO clients (id, name, archived) VALUES (?, ?, ?) ON CONFLICT (id) DO UPDATE set name = ?, archived = ?"
              (topic, name, isArchived, name, isArchived)
        return ()
    )

findAllClientsImpl :: AppM [Client]
findAllClientsImpl =
  withDbConn
    ( \conn -> do
        liftIO $ PSQL.query_ conn "SELECT id, name, archived FROM clients"
    )

findClientImpl :: Topic -> AppM (Maybe Client)
findClientImpl topic =
  withDbConn
    ( \conn -> do
        clients <- liftIO $ PSQL.query conn "SELECT id, name, archived FROM clients WHERE id = ?" $ PSQL.Only topic
        case clients of
          [] -> return Nothing
          _ -> return $ Just (head clients)
    )

-- Utility capabilities
instance MonadId AppM where
  genId = liftIO V4.nextRandom

instance MonadTime AppM where
  currentTimestamp = currentTimestampImpl

currentTimestampImpl :: AppM UTCTime
currentTimestampImpl = liftIO getCurrentTime

-- EventStore
instance MonadEventStore AppM where
  appendEvent = appendEventImpl
  stream = streamImpl

appendEventImpl :: (ToJSON a) => EventEnvelope a -> AppM ()
appendEventImpl (EventEnvelope topic timestamp payload) =
  withDbConn
    ( \conn -> do
        _ <- liftIO $ PSQL.execute conn "INSERT INTO events (topic, timestamp, payload) VALUES (?, ?, ?)" (topic, timestamp, encode payload)
        return ()
    )

-- TODO: Ask Johannes if this makes sense or if there is a more
-- canonical way to do this.

-- Make the compiler not complain about orphan instances.
newtype DbEventEnvelope a = DbEventEnvelope {unDbEventEnvelope :: EventEnvelope a}

instance (FromJSON a) => FromRow (DbEventEnvelope a) where
  fromRow = do
    topic <- field
    timestamp <- field
    payload <- field
    case decode payload of
      Nothing -> undefined
      Just pl -> pure $ DbEventEnvelope $ EventEnvelope topic timestamp pl

streamImpl :: (MonadReader Env m, MonadIO m, FromJSON a) => m [EventEnvelope a]
streamImpl = do
  res <- withDbConn (\conn -> liftIO $ PSQL.query_ conn "SELECT topic, timestamp, payload FROM events")
  pure $ map unDbEventEnvelope res

instance MonadLog AppM where
  initLogger level = liftIO $ Log.updateGlobalLogger "Housekeeper" (Log.setLevel (priority level))
  logMessage = logMessageImpl

priority :: LogLevel -> Log.Priority
priority logLevel = case logLevel of
  Debug -> Log.DEBUG
  Info -> Log.INFO
  Warning -> Log.WARNING
  Error -> Log.ERROR

logMessageImpl :: LogLevel -> LogMessage -> AppM ()
logMessageImpl logLevel logMsg = liftIO $ Log.logM "Housekeeper" (priority logLevel) (show (encode logMsg))
