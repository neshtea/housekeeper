module Housekeeper.Capability.Logger where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (HalogenM)
import Housekeeper.Capability.Time (class MonadTime)
import Housekeeper.Data.Log (Log, LogReason(..), mkLog)

class Monad m <= MonadLogger m where
  logMessage :: Log -> m Unit

instance monadLoggerHalogenM :: MonadLogger m => MonadLogger (HalogenM st act slots msg m) where
  logMessage = lift <<< logMessage

-- | Log a message to given a particular `LogType`
log :: forall m. MonadLogger m => MonadTime m => LogReason -> String -> m Unit
log reason = logMessage <=< mkLog reason

-- | Log a message for debugging purposes
logDebug :: forall m. MonadLogger m => MonadTime m => String -> m Unit
logDebug = log Debug

-- | Log a message to convey non-error information
logInfo :: forall m. MonadLogger m => MonadTime m => String -> m Unit
logInfo = log Info

-- | Log a message as a warning
logWarn :: forall m. MonadLogger m => MonadTime m => String -> m Unit
logWarn = log Warn

-- | Log a message as an error
logError :: forall m. MonadLogger m => MonadTime m => String -> m Unit
logError = log Error

-- | Hush a monadic action by logging the error, leaving it open why the error is being logged
logHush :: forall m a. MonadLogger m => MonadTime m => LogReason -> m (Either String a) -> m (Maybe a)
logHush reason action =
  action >>= case _ of
    Left e -> case reason of
      Debug -> logDebug e *> pure Nothing
      Info -> logInfo e *> pure Nothing
      Warn -> logWarn e *> pure Nothing
      Error -> logError e *> pure Nothing
    Right v -> pure $ Just v

-- | Hush a monadic action by logging the error in debug mode
debugHush :: forall m a. MonadLogger m => MonadTime m => m (Either String a) -> m (Maybe a)
debugHush = logHush Debug
