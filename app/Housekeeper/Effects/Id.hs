module Housekeeper.Effects.Id where

import Control.Concurrent.MonadIO (MonadIO, liftIO)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as V4
import Housekeeper.App.Monad (AppM)

class Monad m => MonadId m where
  genId :: m UUID

instance MonadId AppM where
  genId = genIdImpl -- V4.nextRandom  -- this is 'just' io

genIdImpl :: (MonadIO m) => m UUID
genIdImpl = liftIO V4.nextRandom
