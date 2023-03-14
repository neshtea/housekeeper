{- |
Description: Effects for a 'Client' store.
Maintainer: Marco Schneider <marco.schneider@posteo.de>

This module provides all commands known to Housekeeper.
Every command has a 'Topic' (the thing it is about).
Commands that 'create' something also carry the 'Topic' (which will be the created thing's topic when it is successfully created).
This means that every topic is generated from outside the application.
-}
module Housekeeper.Data.Command (Command (..), topic) where

import Data.Text
import Housekeeper.Data.Event (Topic)

-- | All commands known in Housekeeper.
data Command
    = -- | Create a new client with a topic and a name.
      CreateClient Topic Text
    | -- | Update the name of an existing client identified by the topic.
      UpdateClientName Topic Text
    | -- | Delete an existing client identified by the topic.
      DeleteClient Topic
    | -- | FIXME: Just a dummy.
      OtherCommand Topic

-- | Returns the 'Topic' for a command.
topic :: Command -> Topic
topic (CreateClient t _) = t
topic (UpdateClientName t _) = t
topic (DeleteClient t) = t
topic (OtherCommand t) = t
