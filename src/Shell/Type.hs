{-# LANGUAGE TemplateHaskell #-}
module Shell.Type where

import           Control.Lens
import           Data.Text    (Text)

data ServiceAction = ServiceStatus | ServiceReload deriving (Show)

data Target = Target
  { _node     :: Maybe Text
  , _subgroup :: Maybe Text
  , _role     :: Maybe Text
  , _stack    :: Text
  , _zone     :: Text
  } deriving Show

makeClassy ''Target

newtype Zone = Zone Text
