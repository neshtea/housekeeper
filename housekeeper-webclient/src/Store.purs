module Housekeeper.Store where

import Prelude

import Housekeeper.Api.Request (BaseURL)

data LogLevel = Dev | Prod

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

type Store =
  { logLevel :: LogLevel
  , baseUrl :: BaseURL
  }

data Action = NoAction

reduce :: Store -> Action -> Store
reduce store _ = store
