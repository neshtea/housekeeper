{-# LANGUAGE DataKinds #-}

-- |
-- Description: Running the monad in the context of a servant handler.
-- Maintainer: Marco Schneider <marco.schneider@posteo.de>
--
-- This module provides facilities (namely, the 'app' function) to run
-- the application as a servant handler.  The 'app' can be run as a
-- server via WAI.
module Housekeeper.App.Handler (app) where

import Control.Concurrent.MonadIO (liftIO)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Text (Text)
import Data.UUID (UUID)
import Housekeeper.App.Env (Env (..))
import Housekeeper.App.Monad (AppM, runAppM)
import Housekeeper.Context.ClientManager
  ( Client,
    DomainCommand (..),
    MonadClientRepo (..),
    processCommand,
  )
import Servant

type GetClients = Get '[JSON] [Client]

type GetClient = Get '[JSON] (Maybe Client)

newtype HandlerCommand = HandlerCommand {unHandlerCommand :: DomainCommand}

instance FromJSON HandlerCommand where
  parseJSON = withObject "HandlerCommand" $ \obj -> do
    (command :: Text) <- obj .: "command"
    (domainCommand :: DomainCommand) <-
      case command of
        "create-client" -> CreateClient <$> obj .: "id" <*> obj .: "name"
        "update-client-name" -> UpdateClientName <$> obj .: "id" <*> obj .: "name"
        "archive-client" -> ArchiveClient <$> obj .: "id"
        "restore-client" -> RestoreClient <$> obj .: "id"
        _ -> undefined
    pure $ HandlerCommand domainCommand

type PostCommand = ReqBody '[JSON] HandlerCommand :> Post '[JSON] (Either String (Maybe Client))

type ClientAPI =
  ("clients" :> GetClients)
    :<|> ( "client"
             :> ( Capture "id" UUID :> GetClient
                    :<|> ("exec" :> PostCommand)
                )
         )

server :: ServerT ClientAPI AppM
server =
  handleGetClients
    :<|> ( handleGetClient
             :<|> handlePostCommand
         )

handlePostCommand :: HandlerCommand -> AppM (Either String (Maybe Client))
handlePostCommand cmd = do
  res <- processCommand (unHandlerCommand cmd)
  case res of
    Left domainError -> pure $ Left $ show domainError
    Right maybeClient -> pure $ Right maybeClient

api :: Proxy ClientAPI
api = Proxy

handleGetClients :: AppM [Client]
handleGetClients = findAll

handleGetClient :: UUID -> AppM (Maybe Client)
handleGetClient = find

-- | Natural transformation that makes a 'Handler' out of our 'AppM' monad.
nt :: Env -> AppM a -> Handler a
nt env x = liftIO $ runAppM x env

-- | Given an 'Env', provide the application as an 'Application' that via
-- 'Network.Wai.Handler.Warp.run'.
app :: Env -> Application
app env = serve api $ hoistServer api (nt env) server
