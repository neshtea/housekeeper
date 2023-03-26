module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.Store.Monad (class MonadStore)
import Halogen.VDom.Driver (runUI)
import Housekeeper.Api.Request (BaseURL(..))
import Housekeeper.AppM as AppM
import Housekeeper.Capability.Logger (class MonadLogger, logInfo)
import Housekeeper.Capability.Repo.Client (class MonadClientRepo, NoArgs(..), findClients)
import Housekeeper.Capability.Time (class MonadTime)
import Housekeeper.Data.Client as Client
import Housekeeper.Store as Store
import Network.RemoteData (RemoteData(..), fromMaybe)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  let
    baseUrl = BaseURL "http://localhost:8080"
    logLevel = Store.Dev
    initialStore = { baseUrl, logLevel }
  rootComponent <- AppM.runAppM initialStore component
  runUI rootComponent unit body

type State =
  { loading :: Boolean
  , clientID :: String
  , result :: Maybe String
  , error :: Maybe String
  , clients :: RemoteData String (Array Client.Client)
  }

data Action
  = Initialize
  | LoadClients

component
  :: forall query input output m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => MonadClientRepo m
  => MonadLogger m
  => MonadTime m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

initialState :: forall input. input -> State
initialState _ =
  { loading: false
  , clientID: ""
  , result: Nothing
  , error: Nothing
  , clients: NotAsked
  }

render :: forall slots m. State -> H.ComponentHTML Action slots m
render { clients } =
  case clients of
    NotAsked ->
      HH.div_
        [ HH.text "Clients not loaded" ]
    Loading ->
      HH.div_
        [ HH.text "Loading Clients" ]
    Failure err ->
      HH.div_
        [ HH.text $ "Failed loading clients: " <> err ]
    Success loadedClients ->
      HH.div_
        [ HH.ul_
            (map renderClient loadedClients)
        ]
  where
  renderClient { name } =
    HH.li_ [ HH.text name ]

-- HH.p_ [ HH.text "hello world" ]

handleAction
  :: forall output m
   . MonadClientRepo m
  => MonadLogger m
  => MonadTime m
  => Action
  -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize ->
    handleAction LoadClients

  LoadClients -> do
    H.modify_ _ { clients = Loading }
    clients <- findClients NoArgs
    H.modify_ _ { clients = fromMaybe clients }
