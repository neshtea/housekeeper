module Housekeeper.Data.Appointment where

import Data.Text (Text)
import Data.Time.Clock (DiffTime, UTCTime, secondsToDiffTime)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Housekeeper.Types (Topic)

data Appointment = Appointment
    { appointmentId :: Topic
    , appointmentClient :: Topic
    , appointmentStartAt :: UTCTime
    , appointmentDuration :: DiffTime
    , appointmentReason :: Text
    -- ^ The time between the start of the appointment 'appointmentStart' and it's end.
    }
    deriving (Show, Eq)

instance FromRow Appointment where
    fromRow = do
        topic <- field
        client <- field
        startAt <- field
        duration <- field
        Appointment topic client startAt (secondsToDiffTime duration) <$> field
