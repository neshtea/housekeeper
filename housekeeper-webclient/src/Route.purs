module Housekeeper.Route where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.UUID.Random as UUID
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Home
  | Client UUID.UUIDv4
  | Clients

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance orgRoute :: Ord Route

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "Client": "client" / uuid segment
  , "Clients": "clients" / noArgs
  }

uuid :: RouteDuplex' String -> RouteDuplex' UUID.UUIDv4
uuid = as UUID.toString (UUID.fromString >>> note "not a uuid")
