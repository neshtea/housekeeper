{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Description: Capability for a 'Client' store.
-- Maintainer: Marco Schneider <marco.schneider@posteo.de>
--
-- This module provides all commands known to Housekeeper.
-- Every command has a 'Topic' (the thing it is about).
-- Commands that 'create' something also carry the 'Topic' (which will be the created thing's topic when it is successfully created).
-- This means that every topic is generated from outside the application.
module Housekeeper.Data.Command (Command (..), topic) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics
import Housekeeper.Data.Event (Topic)

-- | All commands known in Housekeeper.
data Command
  = -- | Create a new client with a topic and a name.
    CreateClient Topic Text
  | -- | Update the name of an existing client identified by the topic.
    UpdateClientName Topic Text
  | -- | Delete an existing client identified by the topic.
    DeleteClient Topic
  | -- | Schedule a new appointment for a client at a certain date with a certain duration
    ScheduleAppointment Topic UUID UTCTime Integer
  | -- | FIXME: Just a dummy.
    OtherCommand Topic
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Returns the 'Topic' for a command.
topic :: Command -> Topic
topic (CreateClient t _) = t
topic (UpdateClientName t _) = t
topic (DeleteClient t) = t
topic (ScheduleAppointment t _ _ _) = t
topic (OtherCommand t) = t
