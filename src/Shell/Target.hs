{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Shell.Target where

import qualified Data.List.NonEmpty        as NonEmpty
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc

import           Shell.Prelude
import           Shell.Type
import           Shell.Config (ShellConfig)
import qualified Shell.Config as Config

data Target = Target
  { _node     :: Maybe Text
  , _stacks   :: NonEmpty Text
  , _subgroup :: Maybe Text
  , _role     :: Maybe Role
  , _zone     :: Text
  } deriving (Show,Eq)

makeFieldsNoPrefix ''Target

instance Pretty Target where
  pretty t = pretty $ Text.intercalate "." (NonEmpty.head (t^.stacks) : catMaybes [fmap toS (t^.role), t^.subgroup] <> [ t^.zone])

instance StringConv Target Text  where
  strConv _ (Target node stacks subgroup role zone) =
    let s = catMaybes [node] <> toList stacks <> catMaybes [subgroup, toS <$> role] <> [zone]
    in Text.intercalate "-" s

-- | Return a non empty list of stacks.
-- When one stack is specified though the command line, ignore the defaultstacks defined by the configuration file.
getStacks :: (MonadIO m , MonadReader ShellConfig m) => Maybe Text -> m (NonEmpty Text)
getStacks s = do
  ds <- Config.userDefaultStacks
  pure $ fromList $ maybe ds (:[]) s

mkTarget :: (MonadIO m , MonadReader ShellConfig m) => Zone -> Arg -> m Target
mkTarget (Zone _zone) Arg{..} = do
  _stacks <- getStacks _stack
  pure Target{..}
