module Housekeeper.Effects.Id where

import Data.UUID (UUID)

class Monad m => MonadId m where
  genId :: m UUID
