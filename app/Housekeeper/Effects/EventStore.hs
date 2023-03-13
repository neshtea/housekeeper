module Housekeeper.Effects.EventStore where

import Control.Concurrent.MonadIO (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (encode)
import Database.PostgreSQL.Simple (Only (..), execute, query)
import Housekeeper.App.Env (Env, withDbConn)
import Housekeeper.App.Monad (AppM)
import Housekeeper.Data.Event (Event (..))
import Housekeeper.Types (Topic)

class Monad m => EventStore m where
  -- | 'appendEvent' adds a new event to the event stream.
  appendEvent :: Event -> m ()

  -- | 'streamTopic' takes a topic and returns all events for that topic.
  streamTopic :: Topic -> m [Event]

instance EventStore AppM where
  appendEvent = appendEventImpl
  streamTopic = streamTopicImpl

appendEventImpl :: (MonadReader Env m, MonadIO m) => Event -> m ()
appendEventImpl (Event topic timestamp payload) =
  withDbConn
    ( \conn -> do
        _ <- liftIO $ execute conn "INSERT INTO events (topic, timestamp, payload) VALUES (?, ?, ?)" (topic, timestamp, encode payload)
        return ()
    )

streamTopicImpl :: (MonadReader Env m, MonadIO m) => Topic -> m [Event]
streamTopicImpl topic =
  withDbConn
    ( \conn -> do
        liftIO $ query conn "SELECT topic, timestamp, payload FROM events WHERE topic = ?" (Only topic)
    )
