{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Housekeeper.Data.Event where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics
import Housekeeper.Types (Topic)

data Payload
  = ClientCreated Text
  | ClientNameUpdated Text
  | ClientDeleted
  | OtherEvent
  deriving (Show, Generic)

instance ToJSON Payload where
  toJSON (ClientCreated name) =
    object
      [ "event" .= ("client-created" :: Text),
        "name" .= name
      ]
  toJSON (ClientNameUpdated name) =
    object
      [ "event" .= ("client-name-updated" :: Text),
        "name" .= name
      ]
  toJSON ClientDeleted =
    object ["event" .= ("client-deleted" :: Text)]
  toJSON _ = undefined

instance FromJSON Payload where
  parseJSON = withObject "Payload" $ \obj -> do
    (event :: Text) <- obj .: "event"
    case event of
      "client-created" -> ClientCreated <$> obj .: "name"
      "client-name-updated" -> ClientNameUpdated <$> obj .: "name"
      "client-deleted" -> pure ClientDeleted
      _ -> undefined

data Event = Event
  { eventTopic :: Topic,
    eventTimestamp :: UTCTime,
    eventPayload :: Payload
  }
  deriving (Show, Generic)

instance FromRow Event where
  fromRow = do
    topic <- field
    timestamp <- field
    payload <- field
    case decode payload of
      Nothing -> undefined
      Just pl -> pure $ Event topic timestamp pl
