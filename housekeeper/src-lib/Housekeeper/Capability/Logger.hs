-- Description: Logging capability.
-- Maintainer: Marco Schneider <marco.schneider@posteo.de>
--
-- We support four log levels (see 'LogLevel').  The log level for the application
-- can be set (once) via the 'Env'.  The levels are:
--
-- - 'Debug'
-- - 'Info'
-- - 'Warning'
-- - 'Error'
module Housekeeper.Capability.Logger
  ( MonadLog (..),
    LogLevel (..),
    LogMessage (..),
    logDebug,
    logInfo,
    logWarn,
    logError,
  )
where

import Data.Aeson (ToJSON)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Housekeeper.Capability.Time (MonadTime (..))

data LogLevel
  = -- | Debug messages
    Debug
  | -- | Information
    Info
  | -- |  General Warnings
    Warning
  | -- | General Errors
    Error
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data LogMessage = LogMessage
  { logMessageLevel :: LogLevel,
    logMessageContext :: Text,
    logMessageMessage :: Text,
    logMessageTimestamp :: UTCTime
  }
  deriving (Show, Generic, ToJSON)

class (MonadTime m, Monad m) => MonadLog m where
  -- | Initialize the logger.  When an monad that implements this class is
  -- evaluated, the global logger should be initialized to whatever log-level
  -- is should be.  Ideally, it uses the 'Env' and takes whatever log-level
  -- is configued there.
  initLogger :: LogLevel -> m ()

  -- | Log a message at a certain 'LogLevel'.  Please consider using one of
  -- the specialized log functions instead of this.
  logMessage :: LogLevel -> LogMessage -> m ()

mkLogMessage :: (Show a, MonadTime m) => LogLevel -> a -> m LogMessage
mkLogMessage logLvl msg =
  LogMessage logLvl "Housekeeper" (pack $ show msg) <$> currentTimestamp

logAtLevel ::
  ( MonadTime m,
    MonadLog m,
    Show a
  ) =>
  LogLevel ->
  a ->
  m ()
logAtLevel logLvl msg = do
  logMsg <- mkLogMessage logLvl msg
  logMessage Debug logMsg
  pure ()

-- | Log a message at 'Debug' level.
logDebug :: (MonadLog m, Show a) => a -> m ()
logDebug = logAtLevel Debug

-- | Log a message at 'Info' level.
logInfo :: (MonadLog m, Show a) => a -> m ()
logInfo = logAtLevel Info

-- | Log a message at 'Warning' level.
logWarn :: (MonadLog m, Show a) => a -> m ()
logWarn = logAtLevel Warning

-- | Log a message at 'Error' level.
logError :: (MonadLog m, Show a) => a -> m ()
logError = logAtLevel Error
