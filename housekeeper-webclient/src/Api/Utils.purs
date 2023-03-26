module Housekeeper.Api.Utils where

import Prelude

import Affjax.Web (request)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (rmap)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen.Store.Monad (class MonadStore, getStore)
import Housekeeper.Api.Request (RequestOptions, defaultRequest)
import Housekeeper.Capability.Logger (class MonadLogger, logError)
import Housekeeper.Capability.Time (class MonadTime)
import Housekeeper.Store (Action, Store)

mkRequest :: forall m. MonadAff m => MonadStore Action Store m => RequestOptions -> m (Maybe Json)
mkRequest opts = do
  { baseUrl } <- getStore
  response <- liftAff $ request $ defaultRequest baseUrl opts
  pure $ hush $ rmap _.body response

decode :: forall m a. MonadLogger m => MonadTime m => JsonCodec a -> Maybe Json -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing
decode codec (Just json) = case CA.decode codec json of
  Left err -> logError (printJsonDecodeError err) *> pure Nothing
  Right response -> pure (Just response)
