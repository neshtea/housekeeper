module Housekeeper.Context.ClientManager
  ( Client (..),
    DomainEvent (..),
    DomainCommand (..),
    DomainError (..),
    MonadClientRepo (..),
    ClientResult,
    -- create,
    processCommand,
    exec,
    -- updateName,
    -- archive,
    -- restore,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Text (Text)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics (Generic)
import Housekeeper.Capability.EventStore (EventEnvelope (..), MonadEventStore (..), appendEvents, wrapEvent)
import Housekeeper.Capability.Time (MonadTime (..))

-- | Represents the state of a 'Client'.
data Client = Client
  { -- | The (immuatable) identity of a 'Client'.
    clientId :: UUID,
    -- | The current name of a 'Client'.
    clientName :: Text,
    -- | 'Client's may be archived, this tracks that state.
    clientArchived :: Bool
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance FromRow Client where
  fromRow = Client <$> field <*> field <*> field

-- | 'Command's that can be executed on a 'Client'.  Each 'DomainCommand'
-- represents a user's intent to do something, not the result of the
-- user interaction.
data DomainCommand
  = -- | The intention to create a new 'Client'.
    CreateClient
      UUID
      -- ^ The intended id of the 'Client' that should be created.
      Text
      -- ^ The name of the 'Client' that should be created.
  | -- | The intention to change the name of a 'Client'.
    UpdateClientName
      UUID
      -- ^ The id of the 'Client' whose name should be changed.
      Text
      -- ^ The new name of the 'Client'.
  | -- | The intention to archive a 'Client'.
    ArchiveClient UUID
                  -- ^ The 'Client's id.
  | -- | The intention to restore an already archived 'Client'.
    RestoreClient UUID
                  -- ^ The 'Client's id.

-- | A class for things that have a topic.  Things that have a topic
-- are, for example, domain objects, events or commands.
class HasTopic a where
  -- | Extract the topic of an 'a'.
  topic :: a -> UUID

instance HasTopic DomainCommand where
  topic = topicImpl

topicImpl :: DomainCommand -> UUID
topicImpl (CreateClient uid _) = uid
topicImpl (UpdateClientName uid _) = uid
topicImpl (ArchiveClient uid) = uid
topicImpl (RestoreClient uid) = uid

data DomainEvent
  = ClientCreated Text
  | ClientNameUpdated Text
  | ClientArchived
  | ClientRestored
  deriving stock (Show, Eq)

instance ToJSON DomainEvent where
  toJSON (ClientCreated name) =
    object
      [ "event" .= ("client-created" :: Text),
        "name" .= name
      ]
  toJSON (ClientNameUpdated name) =
    object
      [ "event" .= ("client-name-updated" :: Text),
        "name" .= name
      ]
  toJSON ClientArchived = object ["event" .= ("client-archived" :: Text)]
  toJSON ClientRestored = object ["event" .= ("client-restored" :: Text)]

instance FromJSON DomainEvent where
  parseJSON = withObject "DomainEvent" $ \obj -> do
    (event :: Text) <- obj .: "event"
    case event of
      "client-created" -> ClientCreated <$> obj .: "name"
      "client-name-updated" -> ClientNameUpdated <$> obj .: "name"
      "client-archived" -> pure ClientArchived
      "client-restored" -> pure ClientRestored
      _ -> undefined

-- | Errors that can occur in the ClientManager domain.
data DomainError
  = -- | Signals that a user attempted to create a 'Client' with an id that is already taken.
    ClientAlreadyCreated UUID
                         -- ^ The duplicate id.
  | -- | Signals that a user attempted to update a 'Client' that doesn't exist.
    ClientDoesNotExist
      UUID
      -- ^ The id of the client the user tried to change.
  | -- | Signals that a user atttempted to archive a 'Client' that is already archived.
    ClientAlreadyArchived UUID
                          -- ^ The id of the 'Client' that could not be archived.
  | -- | Signals that a user attempted to restore a 'Client' that is not archived.
    ClientNotArchived UUID
                      -- ^ The id of the 'Client' that was attempted to be restored.
  deriving stock (Show, Eq)

exec ::
  MonadTime m =>
  Maybe Client ->
  DomainCommand ->
  m (Either DomainError [EventEnvelope DomainEvent])
-- TODO: Maybe it's nicer to pattern match in the definition?
exec maybeClient command =
  case maybeClient of
    Nothing ->
      case command of
        CreateClient uid name -> wrapOne uid (ClientCreated name)
        -- TODO: cont'd from above: This wildcard will fall on my feet
        -- at some point.
        c -> pure $ Left $ ClientDoesNotExist (topic c)
    Just client ->
      case command of
        CreateClient uid _ -> pure $ Left $ ClientAlreadyCreated uid
        UpdateClientName uid name -> wrapOne uid (ClientNameUpdated name)
        ArchiveClient uid ->
          if clientArchived client
            then pure $ Left (ClientAlreadyArchived uid)
            else wrapOne uid ClientArchived
        RestoreClient uid ->
          if clientArchived client
            then wrapOne uid ClientRestored
            else pure $ Left $ ClientNotArchived uid
  where
    -- Lazy shortcut to return only one Right event envelope.
    wrapOne uid evt = do
      envelope <- wrapEvent uid evt
      pure $ Right [envelope]

-- | Apply an 'Event'
apply :: Maybe Client -> EventEnvelope DomainEvent -> Maybe Client
-- TODO: Same as above (maybe pattern-match in the definition).
apply maybeClient eventEnvelope =
  case maybeClient of
    Nothing ->
      case eventEnvelopeEvent eventEnvelope of
        ClientCreated name -> Just $ Client (eventEnvelopeTopic eventEnvelope) name False
        _ -> maybeClient
    Just client ->
      case eventEnvelopeEvent eventEnvelope of
        ClientNameUpdated name -> Just $ client {clientName = name}
        ClientArchived -> Just $ client {clientArchived = True}
        ClientRestored -> Just $ client {clientArchived = False}
        _ -> maybeClient

applyMulti :: Maybe Client -> [EventEnvelope DomainEvent] -> Maybe Client
applyMulti = foldl apply

-- | Interactions with a repository that stores and provides
-- 'Client's.
class Monad m => MonadClientRepo m where
  -- | Find a single client in the repo by it's id.
  find :: UUID -> m (Maybe Client)

  -- | Find all clients in the repo.
  findAll :: m [Client]

  -- | Save a client to the repo.
  save :: Client -> m ()

type ClientResult m = m (Either DomainError (Maybe Client))

-- | Process a single 'DomainCommand'.
processCommand ::
  ( MonadClientRepo m,
    MonadTime m,
    MonadEventStore m
  ) =>
  DomainCommand ->
  ClientResult m
processCommand cmd = do
  client <- find (topic cmd)
  execRes <- exec client cmd
  case execRes of
    Left domainError -> pure $ Left domainError
    Right eventEnvelopes -> do
      appendEvents eventEnvelopes
      let maybeNewClient = applyMulti client eventEnvelopes
      case maybeNewClient of
        Nothing -> pure $ Right Nothing
        Just newClient -> do
          save newClient
          pure $ Right (Just newClient)

-- Domain functions

-- NOTE I would like this to be the interface to this module but I
-- can't get it right in Handler.hs, so im using 'DomainCommand's and
-- 'processCommand' as the interface.  Let's see what I do when the
-- corona haze is over and I can think straight.

-- create ::
--   ( MonadTime m,
--     MonadClientRepo m,
--     MonadEventStore m
--   ) =>
--   UUID ->
--   Text ->
--   ClientResult m
-- -- FIXME: We don't want to return a Maybe Client.
-- create uid = processCommand . CreateClient uid

-- updateName ::
--   ( MonadTime m,
--     MonadClientRepo m,
--     MonadEventStore m
--   ) =>
--   UUID ->
--   Text ->
--   ClientResult m
-- updateName uid = processCommand . UpdateClientName uid

-- -- | Archive the 'Client' with the given id.
-- archive ::
--   ( MonadTime m,
--     MonadClientRepo m,
--     MonadEventStore m
--   ) =>
--   UUID ->
--   ClientResult m
-- archive = processCommand . ArchiveClient

-- -- | Restore the 'Client' with the given id.
-- restore ::
--   ( MonadTime m,
--     MonadClientRepo m,
--     MonadEventStore m
--   ) =>
--   UUID ->
--   ClientResult m
-- restore = processCommand . RestoreClient
