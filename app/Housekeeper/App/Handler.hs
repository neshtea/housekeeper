{-# LANGUAGE DataKinds #-}

{- |
Description: Running the monad in the context of a servant handler.
Maintainer: Marco Schneider <marco.schneider@posteo.de>

This module provides facilities (namely, the 'app' function) to run
the application as a servant handler.  The 'app' can be run as a
server via WAI.
-}
module Housekeeper.App.Handler (app) where

import Control.Concurrent.MonadIO (liftIO)
import Data.UUID (UUID)
import Housekeeper.App.Env (Env (..))
import Housekeeper.App.Monad (AppM, runAppM)
import Housekeeper.Data.Client (Client (..))

import Housekeeper.Effects.ClientRepo (MonadClientRepoFind (..), MonadClientRepoFindAll (..), MonadClientRepoSave (..))
import Servant

type PostClient = ReqBody '[JSON] Client :> PostCreated '[JSON] Client

type GetClient = Capture "id" UUID :> Get '[JSON] (Maybe Client)

type GetClients = Get '[JSON] [Client]

type ClientAPI = "clients" :> GetClients :<|> "client" :> (GetClient :<|> PostClient)

api :: Proxy ClientAPI
api = Proxy

handleGetClients :: (MonadClientRepoFindAll m) => m [Client]
handleGetClients = findAllClients

handleGetClient :: (MonadClientRepoFind m) => UUID -> m (Maybe Client)
handleGetClient = findClient

handlePostClient :: (MonadClientRepoSave m) => Client -> m Client
handlePostClient client = saveClient client >> return client

server :: ServerT ClientAPI AppM
server = handleGetClients :<|> handleGetClient :<|> handlePostClient

nt :: Env -> AppM a -> Handler a
nt env x = liftIO $ runAppM x env

{- | Given an 'Env', provide the application as an 'Application' that via
 'Network.Wai.Handler.Warp.run'.
-}
app :: Env -> Application
app env = serve api $ hoistServer api (nt env) server
