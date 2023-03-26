module Housekeeper.Data.Client where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.UUID.Random as UUID

type Client =
  { id :: UUID.UUIDv4
  , name :: String
  , isArchived :: Boolean
  }

type Clients = Array Client

uuidCodec :: CA.JsonCodec UUID.UUIDv4
uuidCodec = CA.prismaticCodec "UUID" UUID.fromString UUID.toString CA.string

clientCodec :: JsonCodec Client
clientCodec = CAR.object "Client"
  { id: uuidCodec
  , name: CA.string
  , isArchived: CA.boolean
  }

clientsCodec :: JsonCodec Clients
clientsCodec =
  CA.array clientCodec
