module Housekeeper.Data.Todo where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)

data RecurrenceScheme
    = Days Int
    | Weeks Int
    deriving (Show, Eq)

data Timing
    = Scheduled UTCTime
    | DeadlineAt UTCTime
    | ScheduledAndDeadline UTCTime UTCTime
    deriving (Show, Eq)

-- | Each 'Todo' has a specific type.
data TodoType
    = -- | A 'Todo' that has no schedule or deadline and is not recurring.
      OneOff
    | -- | A 'Todo' that occurs once
      OneOffWithTiming Timing
    | -- | A 'Todo' that recurrs in some way.
      Recurring
        UTCTime
        -- ^ The start-date of the recurring 'Todo'
        RecurrenceScheme
    deriving (Show, Eq)

data TodoEnvelope = TodoEnvelope
    { todoEnvelopeId :: UUID
    , todoEnvelopeHeader :: Text
    , todoEnvelopeBody :: Text
    }
    deriving (Show, Eq)

-- | The container of a Todo entry.
data Todo = Todo
    { todoEnvelope :: TodoEnvelope
    -- ^ Generic information about the 'Todo' that is shared among 'TodoType's.
    , todoType :: TodoType
    -- ^ Holds the specific information about that 'Todo', such as recurrence, deadlines, and scheduled information.
    }
    deriving (Show, Eq)
