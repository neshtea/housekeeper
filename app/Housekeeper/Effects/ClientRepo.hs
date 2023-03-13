module Housekeeper.Effects.ClientRepo where

import Control.Concurrent.MonadIO (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Database.PostgreSQL.Simple qualified as PSQL
import Housekeeper.App.Env (Env (..), withDbConn)
import Housekeeper.App.Monad (AppM)
import Housekeeper.Data.Client (Client (..))
import Housekeeper.Types (Topic)

-- Finding clients
class Monad m => MonadClientRepoFind m where
  findClient :: Topic -> m (Maybe Client)

instance MonadClientRepoFind AppM where
  findClient = findClientImpl

findClientImpl :: (MonadIO m, MonadReader Env m) => Topic -> m (Maybe Client)
findClientImpl topic =
  withDbConn
    ( \conn -> do
        clients <- liftIO $ PSQL.query conn "SELECT id, name FROM client WHERE id = ?" $ PSQL.Only topic
        case clients of
          [] -> return Nothing
          _ -> return $ Just (head clients)
    )

-- Saving clients
class Monad m => MonadClientRepoSave m where
  saveClient :: Client -> m ()

instance MonadClientRepoSave AppM where
  saveClient = saveClientImpl

saveClientImpl :: (MonadIO m, MonadReader Env m) => Client -> m ()
saveClientImpl (Client topic name) =
  withDbConn
    ( \conn -> do
        -- Discard the result
        _ <- liftIO $ PSQL.execute conn "INSERT INTO client (id, name) VALUES (?, ?)" (topic, name)
        return ()
    )

class Monad m => MonadClientRepoFindAll m where
  findAllClients :: m [Client]

instance MonadClientRepoFindAll AppM where
  findAllClients = findAllClientsImpl

findAllClientsImpl :: (MonadIO m, MonadReader Env m) => m [Client]
findAllClientsImpl =
  withDbConn
    ( \conn -> do
        liftIO $ PSQL.query_ conn "SELECT id, name FROM client"
    )
