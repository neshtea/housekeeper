module Housekeeper.Data.Command where

import Data.Text
import Housekeeper.Types (Topic)

data Command
  = CreateClient Topic Text
  | UpdateClientName Topic Text
  | DeleteClient Topic
  | OtherCommand Topic

topic :: Command -> Topic
topic (CreateClient t _) = t
topic (UpdateClientName t _) = t
topic (DeleteClient t) = t
topic (OtherCommand t) = t
