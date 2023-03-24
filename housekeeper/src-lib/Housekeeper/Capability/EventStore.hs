-- |
-- Description: The EventStore monad.
-- Maintainer: Marco Schneider <marco.schneider@posteo.de>
--
-- This module provides the 'MonadEventStore' with facilities to read the event
-- stream as well as append new events to it.
module Housekeeper.Capability.EventStore
  ( MonadEventStore (..),
    EventEnvelope (..),
    appendEvents,
    wrapEvent,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Housekeeper.Capability.Time (MonadTime (..))

data EventEnvelope a = EventEnvelope
  { eventEnvelopeTopic :: UUID,
    eventEnvelopeTimestamp :: UTCTime,
    eventEnvelopeEvent :: a
  }
  deriving stock (Show, Eq)

wrapEvent :: (MonadTime m) => UUID -> a -> m (EventEnvelope a)
wrapEvent top evt = do
  now <- currentTimestamp
  pure $ EventEnvelope top now evt

-- | A class that provides read and write access to an event store.
-- Users can append a new event to the store (via 'appendEvent') or
-- retrieve the event stream (via 'stream').
class Monad m => MonadEventStore m where
  -- | 'appendEvent' adds a new event to the event stream.
  appendEvent :: (ToJSON a) => EventEnvelope a -> m ()

  -- | 'stream' returns all events from the store.
  stream :: (FromJSON a) => m [EventEnvelope a]

-- | Append a list of events to the event store.
appendEvents ::
  (MonadEventStore m, ToJSON a) =>
  [EventEnvelope a] ->
  m ()
appendEvents = mapM_ appendEvent
