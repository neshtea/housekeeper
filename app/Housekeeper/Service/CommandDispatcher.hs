module Housekeeper.Service.CommandDispatcher where

import Housekeeper.Data.Client (Client (..))
import Housekeeper.Data.Command (Command (..), topic)
import Housekeeper.Data.Event (Event (..), Payload (..))
import Housekeeper.Effects.ClientRepo (MonadClientRepoFind (..), MonadClientRepoSave (..))
import Housekeeper.Effects.EventStore (MonadEventStore (..))
import Housekeeper.Effects.Time (MonadTime (..))
import Housekeeper.Types (Topic)

mkEvent :: (MonadTime m) => Topic -> Payload -> m Event
mkEvent t payload = do
  now <- currentTimestamp
  pure $ Event t now payload

execClientCommand' :: MonadTime m => Maybe Client -> Command -> m [Event]
execClientCommand' Nothing (CreateClient t name) = pure <$> mkEvent t (ClientCreated name)
execClientCommand' (Just _) (CreateClient _ _) = pure []
execClientCommand' Nothing _ = pure []
execClientCommand' (Just _) (UpdateClientName t name) = pure <$> mkEvent t (ClientNameUpdated name)
execClientCommand' (Just _) (DeleteClient t) = pure <$> mkEvent t ClientDeleted
execClientCommand' _ _ = pure []

execClientCommand :: (MonadClientRepoFind m, MonadTime m) => Command -> m [Event]
execClientCommand command = do
  client <- findClient (topic command)
  execClientCommand' client command

applyClientPayload :: Maybe Client -> Event -> Maybe Client
applyClientPayload Nothing (Event t _ (ClientCreated name)) = Just (Client t name)
applyClientPayload Nothing _ = undefined
applyClientPayload (Just client) (Event _ _ (ClientNameUpdated name)) = Just (client {clientName = name})
applyClientPayload (Just _) (Event _ _ ClientDeleted) = Nothing
applyClientPayload (Just _) _ = undefined

applyClientEvent :: (MonadClientRepoFind m, MonadClientRepoSave m) => Event -> m (Maybe Client)
applyClientEvent e@(Event t _ _) = do
  client <- findClient t
  pure $ applyClientPayload client e

dispatchCommand :: (MonadClientRepoFind m, MonadTime m) => Command -> m [Event]
dispatchCommand e@(CreateClient _ _) = execClientCommand e
dispatchCommand e@(UpdateClientName _ _) = execClientCommand e
dispatchCommand e@(DeleteClient _) = execClientCommand e
dispatchCommand _ = pure []

processCommand :: (MonadClientRepoFind m, MonadClientRepoSave m, MonadTime m, MonadEventStore m) => Command -> m ()
processCommand command = do
  events <- dispatchCommand command
  mapM_ appendEvent events
  clients <- mapM applyClientEvent events
  case clients of
    [] -> return ()
    _ -> case last clients of
      Nothing -> pure ()
      Just c -> do
        saveClient c
        pure ()
