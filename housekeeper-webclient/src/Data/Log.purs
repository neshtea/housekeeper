module Housekeeper.Data.Log where

import Prelude

import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Foldable (fold)
import Data.Formatter.DateTime (formatDateTime)
import Housekeeper.Capability.Time (class MonadTime, nowDateTime)

data LogReason = Debug | Info | Warn | Error

derive instance eqLogReason :: Eq LogReason
derive instance ordLogReason :: Ord LogReason

newtype Log = Log
  { reason :: LogReason
  , timestamp :: DateTime
  , message :: String
  }

derive instance eqLog :: Eq Log

message :: Log -> String
message (Log { message: m }) = m

reason :: Log -> LogReason
reason (Log { reason: r }) = r

mkLog :: forall m. MonadTime m => LogReason -> String -> m Log
mkLog logReason msg = do
  now <- nowDateTime
  let
    headerWith start =
      fold [ "[", start, ": ", formatTimestamp now, "]\n", msg ]
    formattedLog = headerWith case logReason of
      Debug -> "DEBUG"
      Info -> "INFO"
      Warn -> "WARNING"
      Error -> "ERROR"
  pure $ Log { reason: logReason, timestamp: now, message: formattedLog }
  where
  -- Will format "2018-10-25 11:25:29 AM"
  formatTimestamp =
    either (const "(Failed to assign time)") identity
      <<< formatDateTime "YYYY-DD-MM hh:mm:ss a"
