{- |
Description: The EventStore monad.
Maintainer: Marco Schneider <marco.schneider@posteo.de>

This module provides the 'MonadEventStore' with facilities to read the event
stream as well as append new events to it.
-}
module Housekeeper.Effects.EventStore (
    MonadEventStore (..),
    StreamFilter,
    appendEvents,
    streamTopicFiltered,
) where

import Housekeeper.Data.Event (Event (..))
import Housekeeper.Types (Topic)

class Monad m => MonadEventStore m where
    -- | 'appendEvent' adds a new event to the event stream.
    appendEvent :: Event -> m ()

    -- | 'streamTopic' takes a topic and returns all events for that topic.
    streamTopic :: Topic -> m [Event]

-- | Append a list of events to the event store.
appendEvents :: (MonadEventStore m) => [Event] -> m ()
appendEvents = mapM_ appendEvent

type StreamFilter = Event -> Bool

-- | View a topic from the event stream, filtered by 'p'.
streamTopicFiltered :: (MonadEventStore m) => Topic -> StreamFilter -> m [Event]
streamTopicFiltered topic p = do
    events <- streamTopic topic
    pure $ filter p events
