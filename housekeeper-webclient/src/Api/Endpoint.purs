module Housekeeper.Api.Endpoint where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.UUID.Random (UUIDv4)
import Routing.Duplex (RouteDuplex', root, segment, prefix)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Housekeeper.Route (uuid)

data Endpoint =
  Client UUIDv4
  | Clients

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ prefix "api" $ sum
                { "Client": "client" / uuid segment
                , "Clients": "clients" / noArgs }
