{-# LANGUAGE DeriveAnyClass #-}

module Housekeeper.Data.Client where

import Data.Aeson
import Data.Text
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics (Generic)
import Housekeeper.Types (Topic)

data Client = Client
  { -- | The identity of the client.
    clientId :: Topic,
    -- | The current name of the client.
    clientName :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Typeable)

instance FromRow Client where
  fromRow = Client <$> field <*> field
