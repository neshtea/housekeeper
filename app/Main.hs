{-# LANGUAGE DataKinds #-}

module Main where

import Control.Concurrent.MonadIO (liftIO)
import Housekeeper.App.Env (Env (..), mkEnv)
import Housekeeper.App.Monad (AppM, runAppM)
import Housekeeper.Data.Client (Client (..))
import Housekeeper.Effects.ClientRepo (MonadClientRepoFindAll (..))
import Network.Wai.Handler.Warp (run)
import Servant

type GetClients = Get '[JSON] [Client]

type ClientAPI = "clients" :> GetClients

api :: Proxy ClientAPI
api = Proxy

server :: ServerT ClientAPI AppM
server = findAllClients

nt :: Env -> AppM a -> Handler a
nt env x = liftIO $ runAppM x env

app :: Env -> Application
app env = serve api $ hoistServer api (nt env) server

main :: IO ()
main = do
  env <- mkEnv
  let runApp = run 8080 $ app env
  runApp
