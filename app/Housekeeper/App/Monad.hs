module Housekeeper.App.Monad where

import Control.Concurrent.MonadIO (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Housekeeper.App.Env

newtype AppM a = AppM {unAppM :: ReaderT Env IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runAppM :: AppM a -> Env -> IO a
runAppM app = runReaderT (unAppM app)
