module Type where

import           Data.Text (Text)

type Subgroup = Text
type Zone = Text
type Role = Text
type Stack = Text
type Node = Text
type Cmd = Text
type Key = Text
type Jobid = Text
type User = Text

data ServiceAction = ServiceStatus | ServiceReload deriving (Show)

instance Read ServiceAction where
  readsPrec _ "status" = [(ServiceStatus, "")]
  readsPrec _ "reload" = [(ServiceReload, "")]
  readsPrec _ _ = []
