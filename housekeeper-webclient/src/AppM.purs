module Housekeeper.AppM where

import Prelude

import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT)
import Housekeeper.Api.Endpoint (Endpoint(..))
import Housekeeper.Api.Request (RequestMethod(..))
import Housekeeper.Api.Utils (decode, mkRequest)
import Housekeeper.Capability.Logger (class MonadLogger)
import Housekeeper.Capability.Repo.Client (class MonadClientRepo)
import Housekeeper.Capability.Time (class MonadTime)
import Housekeeper.Data.Client (clientCodec, clientsCodec)
import Housekeeper.Data.Log as Log
import Housekeeper.Store (LogLevel(..))
import Housekeeper.Store as Store
import Safe.Coerce (coerce)

-- | Our main app monad.
newtype AppM a = AppM (StoreT Store.Action Store.Store Aff.Aff a)

runAppM :: forall q i o. Store.Store -> H.Component q i o AppM -> Aff.Aff (H.Component q i o Aff.Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore Store.Action Store.Store AppM

instance monadTimeAppM :: MonadTime AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance monadLoggerAppM :: MonadLogger AppM where
  logMessage log = do
    { logLevel } <- getStore
    liftEffect case logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

instance monadClientRepo :: MonadClientRepo AppM where
  findClient uid = do
    mbJson <- mkRequest { endpoint: Client uid, method: Get }
    decode clientCodec mbJson
  findClients _ = do
    mbJson <- mkRequest { endpoint: Clients, method: Get }
    decode clientsCodec mbJson
