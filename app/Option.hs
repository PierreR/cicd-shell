{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Option where

import           Protolude
import           Turtle    hiding ((<>))
import           Control.Lens (makeLenses)
import           Type


data ResultArg
  = ResultJob Text
  | ResultNum Int
  deriving (Show)

data Command
  = Console
  | Data (Maybe Key, Arg)
  | Facts (Bool, Arg)
  | Orchestrate (Cmd, (Maybe Stack))
  | Stats
  | Du Arg
  | Ping Arg
  | Runpuppet Arg
  | Sync Arg
  | Result ResultArg
  deriving (Show)


data Options = Options Text Command

data Arg
  = Arg
  { _argRole :: Maybe Role
  , _argNode :: Maybe Node
  , _argSubgroup :: Maybe Subgroup
  , _argStack :: Maybe Stack
  } deriving Show

makeLenses ''Arg

argParser :: Parser Arg
argParser
  = Arg
  <$> optional (argText "role" "Role name maybe prefixed by a subgroup ('subgroup.role')")
  <*> optional (optText "node" 'n' "Target node")
  <*> optional (optText "subgroup" 'g' "Target subgroup")
  <*> optional (optText "stack" 's' "Target stack/hostgroup")

commandParser :: Parser Command
commandParser =
      Console     <$  subcommand "console" "Open the cicd console" (pure ())
  <|> Stats       <$  subcommand "stats" "Stats (special permission required)" (pure ())
  <|> Data        <$> subcommand "data" "Return configuration data for a specific property" data_parser
  <|> Orchestrate <$> subcommand "orch" "Run an orchestration command on the infrastructure" orch_parser
  <|> Facts       <$> subcommand "facts" "Return essential facts about nodes" fact_parser
  <|> Ping        <$> subcommand "ping" "Ping nodes" argParser
  <|> Du          <$> subcommand "du" "Return disk usage" argParser
  <|> Runpuppet   <$> subcommand "runpuppet" "Apply puppet configuration" argParser
  <|> Sync        <$> subcommand "sync" "Sync data from master to nodes" argParser
  <|> Result      <$> subcommand "result" ("Display the results of the most recent jobs executed by the user or for a specific id") result_parser
  where
    result_parser = ResultNum <$> (optInt "Num" 'n' "Number of results to display") <|>  ResultJob <$> (optText "job" 'j' "Job id")
    data_parser = (,) <$> optional (optText "key" 'k' "Property to look up for" ) <*> argParser
    fact_parser = (,) <$> switch "all" 'a' "Target whole the known stacks" <*> argParser
    orch_parser = (,) <$> (argText "cmd" "Command to run") <*> optional (optText "stack" 's' "Target stack/hostgroup" )


parser :: Parser Options
parser
  = Options <$> argText "zone" "ZONE such as dev, staging, testing or prod"
  <*> commandParser


-- -- | One or none.
  -- let nix_file = format (s%"/"%s%".nix") projectDir zone
-- optional' :: Alternative f => f a -> f (Optional a)
-- optional' v = Optional.Specific <$> v <|> pure Optional.Default
