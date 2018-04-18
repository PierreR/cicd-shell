{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}

module Shell.Type where

import qualified Data.Text                 as Text
import qualified Data.List.NonEmpty        as NonEmpty
import           GHC.Show                  (Show (..))
import           Shell.Prelude
import           Data.Text.Prettyprint.Doc

newtype Zone = Zone Text deriving Show
newtype Subgroup = Subgroup Text deriving Show

newtype ServiceName = ServiceName Text deriving Show
newtype Down = Down Bool deriving Show
newtype Refresh = Refresh Bool deriving Show

data ServiceAction = ServiceStatus | ServiceRestart deriving (Show)

data Role = Role (Maybe Subgroup) Text deriving Show

instance StringConv Role Text  where
  strConv _ (Role Nothing r)             = r
  strConv _ (Role (Just (Subgroup g)) r) = g <> "." <> r

data Target = Target
  { _node     :: Maybe Text
  , _stacks   :: NonEmpty Text
  , _subgroup :: Maybe Text
  , _role     :: Maybe Role
  , _zone     :: Text
  } deriving Show

makeFieldsNoPrefix ''Target

instance Pretty Target where
  pretty t = pretty $ Text.intercalate "." (NonEmpty.head (t^.stacks) : (catMaybes [fmap toS (t^.role), t^.subgroup]) <> [ t^.zone])

defTarget s = Target Nothing s Nothing Nothing mempty

instance StringConv Target Text  where
  strConv _ (Target node stacks subgroup role zone) =
    let s = catMaybes [node] <> toList stacks <> catMaybes [subgroup, toS <$> role] <> [zone]
    in Text.intercalate "-" s

data ExtraFlag
  = ExtraFlag
  { _raw     :: Bool -- ^ Display the result with no `jq` pretty printer
  , _verbose :: Bool -- ^ Print the `pepper` command to stdout
  , _dry     :: Bool -- ^ Print the command and exit
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
