module Housekeeper.App.Env (Env (..), mkEnv, withDbConn, askLogLevel) where

import Control.Monad.Reader (MonadReader, asks)
import Database.PostgreSQL.Simple qualified as PSQL
import Housekeeper.Capability.Logger (LogLevel (Debug))

-- | Environment for the application.
data Env = Env
  { -- | A connection to the postgres database.  FIXME: This is not a good idea now, is it?
    dbConn :: PSQL.Connection,
    -- | Just a dummy until we use a real loggin library.
    logLevel :: LogLevel
  }

mkDbConn :: IO PSQL.Connection
mkDbConn = do
  PSQL.connectPostgreSQL "host='localhost' port=5432 user=schneider"

-- | A fresh environment, using defaults.
mkEnv :: IO Env
mkEnv = do
  conn <- mkDbConn
  return $ Env conn Debug

-- Returns the 'dbConn' from the environment.
askDbConn :: (MonadReader Env m) => m PSQL.Connection
askDbConn = asks dbConn

askLogLevel :: (MonadReader Env m) => m LogLevel
askLogLevel = asks logLevel

-- | Executes 'f' by providing the 'dbConn' from the environment at
-- the argument.
withDbConn :: (MonadReader Env m) => (PSQL.Connection -> m b) -> m b
withDbConn f = do
  dbConn <- askDbConn
  f dbConn
