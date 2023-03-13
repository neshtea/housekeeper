{-# LANGUAGE DeriveAnyClass #-}

module Housekeeper.Data.Client where

import Data.Aeson
import Data.Text
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics
import Housekeeper.Effects.Id (MonadId (..))
import Housekeeper.Types (Topic)

data Client = Client
  { clientId :: Topic,
    clientName :: Text
  }
  deriving (Show, Generic, ToJSON)

-- | Make a new 'Client', using a randomly generated UUID as the id.
mkClient :: (MonadId m) => Text -> m Client
mkClient name = do
  uid <- genId
  return $ Client uid name

instance FromRow Client where
  fromRow = Client <$> field <*> field
