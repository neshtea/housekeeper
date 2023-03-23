-- |
-- Module: ClientRepo
-- Description: Capability for a 'Client' store.
-- Maintainer: Marco Schneider <marco.schneider@posteo.de>
--
-- This module provides capabilities (as Typeclasses) for interacting with a 'Client' store.
--
-- - Reading a 'Client' from the store: 'MonadClientRepoFind'
-- - Reading all 'Client's from the store: 'MonadClientRepoFindAll'
-- - Writing a 'Client's to the store: 'MonadClientRepoSave'
module Housekeeper.Capability.ClientRepo
  ( MonadClientRepoFind (..),
    MonadClientRepoFindAll (..),
    MonadClientRepoSave (..),
    saveThenFind,
  )
where

import Housekeeper.Data.Client (Client (..))
import Housekeeper.Types (Topic)

class Monad m => MonadClientRepoFind m where
  -- | Find a single client in the repo by it's id.
  findClient :: Topic -> m (Maybe Client)

class Monad m => MonadClientRepoFindAll m where
  -- | Find all clients in the repo.
  findAllClients :: m [Client]

class Monad m => MonadClientRepoSave m where
  -- | Save a client to the repo.
  saveClient :: Client -> m ()

-- | Save a 'Client' to the store, then retrieve it.
saveThenFind :: (MonadClientRepoFind m, MonadClientRepoSave m) => Client -> m (Maybe Client)
saveThenFind client@(Client topic _) = do
  saveClient client
  findClient topic
