module Housekeeper.Api.Request where

import Prelude

import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.Web (Request)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Housekeeper.Api.Endpoint (Endpoint, endpointCodec)
import Routing.Duplex (print)

data RequestMethod =
  Get

newtype BaseURL = BaseURL String

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

defaultRequest :: BaseURL -> RequestOptions -> Request Json
defaultRequest (BaseURL baseUrl) { endpoint, method } =
  { method: Left requestMethod
  , url: baseUrl <> print endpointCodec endpoint
  , headers: []
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , timeout: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple requestMethod body = case method of
    Get -> Tuple GET Nothing
