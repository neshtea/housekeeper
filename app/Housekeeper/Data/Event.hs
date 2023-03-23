{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Description: Definitions of events and payloads.
Maintainer: Marco Schneider <marco.schneider@posteo.de>

This module provides the 'Event' type and associated 'Payloads'.
Each event carries with it a 'Payload' which identifies the type of event and
carries it's domain data.

Each event is JSON-serializable (via 'FromJSON' and 'ToJSON').
-}
module Housekeeper.Data.Event where

import Data.Aeson (FromJSON (..), ToJSON (..), decode, object, withObject, (.:), (.=))
import Data.Text (Text)
import Data.Time.Clock (DiffTime, UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics

{- | Each entity has an identity, and the 'Topic' usually is this identity.
 There is other data that also uses this type, most notably
 'Housekeeper.Data.Command's.
-}
type Topic = UUID

-- | Payloads of events.  See 'eventPayload'.
data Payload
    = ClientCreated Text
    | ClientNameUpdated Text
    | ClientDeleted
    | -- | A scheduled appointment with a client.
      AppointmentScheduled
        Topic
        -- ^ the client's id
        UTCTime
        -- ^ timestamp of the scheduled appointment
        DiffTime
        -- ^ the duration of the appointment
    | OtherEvent
    deriving (Show, Generic)

instance ToJSON Payload where
    toJSON (ClientCreated name) =
        object
            [ "event" .= ("client-created" :: Text)
            , "name" .= name
            ]
    toJSON (ClientNameUpdated name) =
        object
            [ "event" .= ("client-name-updated" :: Text)
            , "name" .= name
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

-- | Domain events in 'Housekeeper'.
data Event = Event
    { eventTopic :: Topic
    -- ^ The thing that event is about.
    , eventTimestamp :: UTCTime
    -- ^ Timestamp of the event.  This is it's identity.
    , eventPayload :: Payload
    -- ^ The domain 'Payload' of the event.
    }
    deriving (Show, Generic)

instance ToJSON Event

instance FromRow Event where
    fromRow = do
        topic <- field
        timestamp <- field
        payload <- field
        case decode payload of
            Nothing -> undefined
            Just pl -> pure $ Event topic timestamp pl
