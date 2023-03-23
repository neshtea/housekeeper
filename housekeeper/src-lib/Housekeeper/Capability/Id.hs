module Housekeeper.Capability.Id where

import Data.UUID (UUID)

-- | A capability that allows the user to generate 'UUID's.
class Monad m => MonadId m where
  genId :: m UUID
