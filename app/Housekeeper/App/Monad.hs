{- |
Description: Definition of the app monad 'AppM'.
Maintainer: Marco Schneider <marco.schneider@posteo.de>

This module defines the main app monad 'AppM', as well as the default instances
of all relevant typeclasses.
-}
module Housekeeper.App.Monad (AppM, runAppM) where

import Control.Concurrent.MonadIO (MonadIO (..), liftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Aeson (encode)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.UUID.V4 qualified as V4
import Database.PostgreSQL.Simple (Only (..), execute, query)
import Database.PostgreSQL.Simple qualified as PSQL
import Housekeeper.App.Env (Env (..), withDbConn)
import Housekeeper.Data.Client (Client (..))
import Housekeeper.Data.Event (Event (..))
import Housekeeper.Effects.ClientRepo (MonadClientRepoFind (..), MonadClientRepoFindAll (..), MonadClientRepoSave (..))
import Housekeeper.Effects.EventStore (MonadEventStore (..))
import Housekeeper.Effects.Id (MonadId (..))
import Housekeeper.Effects.Time (MonadTime (..))
import Housekeeper.Types (Topic)

-- | Main application monad.
newtype AppM a = AppM {unAppM :: ReaderT Env IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

-- | Run the monad in 'IO', given some 'Env', producing a value of type 'a'.
runAppM :: AppM a -> Env -> IO a
runAppM app = runReaderT (unAppM app)

instance MonadClientRepoSave AppM where
    saveClient = saveClientImpl

saveClientImpl :: Client -> AppM ()
saveClientImpl (Client topic name) =
    withDbConn
        ( \conn -> do
            -- Discard the result
            _ <- liftIO $ PSQL.execute conn "INSERT INTO client (id, name) VALUES (?, ?)" (topic, name)
            return ()
        )

-- Client Repo
instance MonadClientRepoFindAll AppM where
    findAllClients = findAllClientsImpl

findAllClientsImpl :: AppM [Client]
findAllClientsImpl =
    withDbConn
        ( \conn -> do
            liftIO $ PSQL.query_ conn "SELECT id, name FROM client"
        )

instance MonadClientRepoFind AppM where
    findClient = findClientImpl

findClientImpl :: Topic -> AppM (Maybe Client)
findClientImpl topic =
    withDbConn
        ( \conn -> do
            clients <- liftIO $ PSQL.query conn "SELECT id, name FROM client WHERE id = ?" $ PSQL.Only topic
            case clients of
                [] -> return Nothing
                _ -> return $ Just (head clients)
        )

-- Utility effects
instance MonadId AppM where
    genId = liftIO V4.nextRandom

instance MonadTime AppM where
    currentTimestamp = currentTimestampImpl

currentTimestampImpl :: AppM UTCTime
currentTimestampImpl = liftIO getCurrentTime

instance MonadEventStore AppM where
    appendEvent = appendEventImpl
    streamTopic = streamTopicImpl

appendEventImpl :: (MonadReader Env m, MonadIO m) => Event -> m ()
appendEventImpl (Event topic timestamp payload) =
    withDbConn
        ( \conn -> do
            _ <- liftIO $ execute conn "INSERT INTO events (topic, timestamp, payload) VALUES (?, ?, ?)" (topic, timestamp, encode payload)
            return ()
        )

streamTopicImpl :: (MonadReader Env m, MonadIO m) => Topic -> m [Event]
streamTopicImpl topic =
    withDbConn
        ( \conn -> do
            liftIO $ query conn "SELECT topic, timestamp, payload FROM events WHERE topic = ?" (Only topic)
        )
