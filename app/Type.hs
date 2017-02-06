{-# LANGUAGE TemplateHaskell #-}
module Type where

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
  { node     :: Maybe Text
  , subgroup :: Maybe Text
  , role     :: Maybe Text
  , stack    :: Text
  , zone     :: Text
  } deriving Show
