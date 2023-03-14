{-# LANGUAGE DeriveAnyClass #-}

module Housekeeper.Data.Client where

import Data.Aeson
import Data.Text
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics
import Housekeeper.Types (Topic)

data Client = Client
    { clientId :: Topic
    , clientName :: Text
    }
    deriving (Show, Generic, ToJSON, FromJSON)

instance FromRow Client where
    fromRow = Client <$> field <*> field
