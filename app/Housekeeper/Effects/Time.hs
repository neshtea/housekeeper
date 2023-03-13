module Housekeeper.Effects.Time where

import Control.Concurrent.MonadIO (liftIO)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Housekeeper.App.Monad (AppM)

class Monad m => MonadTime m where
  currentTimestamp :: m UTCTime

instance MonadTime AppM where
  currentTimestamp = currentTimestampImpl

currentTimestampImpl :: AppM UTCTime
currentTimestampImpl = liftIO getCurrentTime
