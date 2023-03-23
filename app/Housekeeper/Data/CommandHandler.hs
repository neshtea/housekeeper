{-# LANGUAGE DeriveAnyClass #-}

module Housekeeper.Data.CommandHandler where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Housekeeper.Data.Command (Command)
import Housekeeper.Data.Event (Event)

data ExecResult a
    = -- | The okay result, returning a list of 'Event's.
      Ok [Event]
    | -- | A generic failure, described by some String.
      GenericFailure String
    | -- | A failure in the domain.
      DomainFailure a
    | -- | Result if the handler is not interested in the Command.
      Unhandled
    deriving (Show, Generic, ToJSON)

data Handler = Handler
    { handlerHandles :: Command -> Bool
    , handlerHandle :: Command
    }
