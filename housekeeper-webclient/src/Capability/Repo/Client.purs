module Housekeeper.Capability.Repo.Client where

import Prelude

import Housekeeper.Data.Client (Client)
import Data.Maybe (Maybe)
import Data.UUID.Random as UUID
import Halogen (HalogenM, lift)

data NoArgs = NoArgs

class Monad m <= MonadClientRepo m where
  findClient :: UUID.UUIDv4 -> m (Maybe Client)
  -- FIXME I don't why, but 'NoArgs' protects the HalogenM
  -- implementation of findClients to go into an infinite loop.
  findClients :: NoArgs -> m (Maybe (Array Client))

instance monadClientRepoHalogenM
         :: MonadClientRepo m => MonadClientRepo (HalogenM st act slots msg m) where
  findClient = lift <<< findClient
  findClients = lift <<< findClients
