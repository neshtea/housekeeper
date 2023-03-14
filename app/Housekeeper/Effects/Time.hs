module Housekeeper.Effects.Time where

import Data.Time.Clock (UTCTime)

class Monad m => MonadTime m where
  currentTimestamp :: m UTCTime
