{-# LANGUAGE TemplateHaskell #-}
module Type where

import           Control.Lens
import           Data.Text    (Text)

type Node = Text
type Cmd = Text
type Key = Text
type Jobid = Text
type User = Text

data ServiceAction = ServiceStatus | ServiceReload deriving (Show)

instance Read ServiceAction where
  readsPrec _ "status" = [(ServiceStatus, "")]
  readsPrec _ "reload" = [(ServiceReload, "")]
  readsPrec _ _        = []

data Target = Target
  { _node     :: Maybe Text
  , _subgroup :: Maybe Text
  , _role     :: Maybe Text
  , _stack    :: Text
  , _zone     :: Text
  } deriving Show

makeLenses ''Target
