module Housekeeper.Effects.Log where

import Control.Concurrent.MonadIO (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Housekeeper.App.Env (Env (..))
import Housekeeper.App.Monad (AppM)

class Monad m => MonadLog m where
  logMessage :: String -> m ()

instance MonadLog AppM where
  logMessage = logMessageImpl

logMessageImpl :: (MonadIO m, MonadReader Env m) => String -> m ()
logMessageImpl msg = do
  cfg <- ask
  let logMsg = logLevel cfg ++ ":" ++ msg
  liftIO $ putStrLn logMsg
