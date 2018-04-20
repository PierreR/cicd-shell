{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLists        #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}

module Shell.Type where

import qualified Data.List.NonEmpty        as NonEmpty
import qualified Data.Text                 as Text
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

data Target = Target
  { _node     :: Maybe Text
  , _stacks   :: NonEmpty Text
  , _subgroup :: Maybe Text
  , _role     :: Maybe Role
  , _zone     :: Text
  } deriving (Show,Eq)

makeFieldsNoPrefix ''Target

instance Pretty Target where
  pretty t = pretty $ Text.intercalate "." (NonEmpty.head (t^.stacks) : (catMaybes [fmap toS (t^.role), t^.subgroup]) <> [ t^.zone])

-- | Text reader for Target
-- Expect the following pattern:
-- $hostgroup.$subgroup.$role.$zone
-- $hostgroup.$role.$zone
readTarget :: Text -> Maybe Target
readTarget s =
  case Text.splitOn "." s of
    [h,g,r,z] ->
      mk_target (Just g) (Just r) z <$> read_hostgroup h
    [h,r,z]   ->
      mk_target Nothing (Just r) z <$> read_hostgroup h
    [h,z]   ->
      mk_target Nothing Nothing z <$> read_hostgroup h
    _         -> Nothing
  where
    mk_target (Just g) (Just r) z h = Target Nothing h Nothing (Just (Role (Just (Subgroup g)) r)) z
    mk_target Nothing (Just r) z h = Target Nothing h Nothing (Just (Role Nothing r)) z
    mk_target Nothing Nothing z h = Target Nothing h Nothing Nothing z
    mk_target (Just _) Nothing _ _ = panic "Can't pass a subgroup without a role in this (local) context"

    read_hostgroup :: Text -> Maybe (NonEmpty Text)
    read_hostgroup s = do
      s' <- Text.stripPrefix "[" s
      s'' <- Text.stripSuffix "]" s'
      pure $ NonEmpty.fromList $ Text.splitOn "," s''

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
