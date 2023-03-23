module Housekeeper.Capability.Time where

import Data.Time.Clock (UTCTime)

-- | A capability that allows the user to generate timestamps of the
-- current time in UTC.
class Monad m => MonadTime m where
  currentTimestamp :: m UTCTime
