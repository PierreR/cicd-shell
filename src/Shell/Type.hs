{-# LANGUAGE TemplateHaskell #-}
module Shell.Type where

import           Control.Lens
import           Data.Text    (Text)

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

makeClassy ''Target
