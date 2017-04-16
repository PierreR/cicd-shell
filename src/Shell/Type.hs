{-# LANGUAGE TemplateHaskell #-}
module Shell.Type where

import           Shell.Prelude

data ServiceAction = ServiceStatus | ServiceReload deriving (Show)

data Target = Target
  { _node     :: Maybe Text
  , _subgroup :: Maybe Text
  , _role     :: Maybe Text
  , _stack    :: Text
  , _zone     :: Text
  } deriving Show

makeLenses ''Target

newtype Zone = Zone Text
