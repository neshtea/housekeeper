module Housekeeper.App.Env where

import Control.Monad.Reader (MonadReader, asks)
import Database.PostgreSQL.Simple qualified as PSQL

data Env = Env
  { dbConn :: PSQL.Connection,
    logLevel :: String
  }

mkDbConn :: IO PSQL.Connection
mkDbConn = do
  PSQL.connectPostgreSQL "host='localhost' port=5432 user=schneider"

mkEnv :: IO Env
mkEnv = do
  conn <- mkDbConn
  return $ Env conn "info"

-- | Returns the 'dbConn' from the environment.
askDbConn :: (MonadReader Env m) => m PSQL.Connection
askDbConn = do asks dbConn

-- | Executes 'f' by providing the 'dbConn' from the environment at
-- the argument.
withDbConn :: (MonadReader Env m) => (PSQL.Connection -> m b) -> m b
withDbConn f = do
  dbConn <- askDbConn
  f dbConn
