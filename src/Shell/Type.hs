{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}

module Shell.Type where

import           Data.Text.Prettyprint.Doc
import           GHC.Show                  (Show (..))
import           Shell.Prelude

newtype Zone = Zone Text deriving Show
newtype Subgroup = Subgroup Text deriving (Show,Eq)

newtype ServiceName = ServiceName Text deriving Show
newtype Down = Down Bool deriving Show
newtype Refresh = Refresh Bool deriving Show

data ServiceAction = ServiceStatus | ServiceRestart deriving (Show)

data Role = Role (Maybe Subgroup) Text deriving (Show, Eq)

instance StringConv Role Text  where
  strConv _ (Role Nothing r)             = r
  strConv _ (Role (Just (Subgroup g)) r) = g <> "." <> r


data ExtraFlag
  = ExtraFlag
  { _raw   :: Bool -- ^ Display the result with no `jq` pretty printer
  , _quiet :: Bool -- ^ Print the `pepper` command to stdout
  , _dry   :: Bool -- ^ Print the command and exit
  } deriving (Show, Generic)

defExtraFlag = ExtraFlag False False False

makeLenses ''ExtraFlag

data Arg
  = Arg
  { _role      :: Maybe Role
  , _node      :: Maybe Text
  , _subgroup  :: Maybe Text
  , _stack     :: Maybe Text
  , _extraFlag :: ExtraFlag
  } deriving Show

makeFieldsNoPrefix ''Arg

data SetfactArg
  = SetfactArg
  { _node      :: Text
  , _hostgroup :: Maybe Text
  , _subgroup  :: Maybe Text
  , _role      :: Maybe Text
  , _inst      :: Maybe Text
  , _zone      :: Maybe Text
  , _extraFlag :: ExtraFlag
  } deriving Show

makeFieldsNoPrefix ''SetfactArg
