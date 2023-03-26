module Housekeeper.Capability.Time where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.DateTime (Date, DateTime, Time)
import Data.DateTime.Instant (Instant)
import Halogen (HalogenM)

class Monad m <= MonadTime m where
  now :: m Instant
  nowDate :: m Date
  nowTime :: m Time
  nowDateTime :: m DateTime

instance monadTimeHalogenM :: MonadTime m => MonadTime (HalogenM st act slots msg m) where
  now = lift now
  nowDate = lift nowDate
  nowTime = lift nowTime
  nowDateTime = lift nowDateTime
