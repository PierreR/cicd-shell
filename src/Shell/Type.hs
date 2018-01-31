{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Shell.Type where

import           Shell.Prelude

newtype Zone = Zone Text
newtype ServiceName = ServiceName Text deriving Show
newtype Down = Down Bool deriving Show

data ServiceAction = ServiceStatus | ServiceRestart deriving (Show)

data Target = Target
  { _node     :: Maybe Text
  , _subgroup :: Maybe Text
  , _role     :: Maybe Text
  , _stack    :: Text
  , _zone     :: Text
  } deriving Show

makeFieldsNoPrefix ''Target

data ExtraFlag
  = ExtraFlag
  { _raw     :: Bool -- ^ Display the result with no `jq` pretty printer
  , _verbose :: Bool -- ^ Print the `pepper` command to stdout
  , _dry     :: Bool -- ^ Print the command and exit
  } deriving (Show, Generic)

instance Default ExtraFlag where
  def = ExtraFlag False False False

makeLenses ''ExtraFlag

data Arg
  = Arg
  { _role      :: Maybe Text
  , _node      :: Maybe Text
  , _subgroup  :: Maybe Text
  , _stack     :: Maybe Text
  , _extraFlag :: ExtraFlag
  } deriving Show

makeFieldsNoPrefix ''Arg

data SetfactArg
  = SetfactArg
  { _node      :: Text
  , _subgroup  :: Maybe Text
  , _role      :: Maybe Text
  , _hostgroup :: Maybe Text
  , _zone      :: Maybe Text
  , _extraFlag :: ExtraFlag
  } deriving Show

makeFieldsNoPrefix ''SetfactArg
