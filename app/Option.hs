{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Option where

import           Protolude
import           Turtle    hiding ((<>))

type Target = Text
type StackName = Text
type Cmd = Text
type Key = Text

data Options = Options Text Command

data Command
  = Console
  | Stats
  | Stack StackCommand
  | Node NodeCommand
  | Result ResultArg
  deriving (Show)

data ResultArg
  = ResultJob Text
  | ResultNum Int
  deriving (Show)

data StackCommand
  = StackData (Maybe StackName) Key
  | StackFacts (Maybe StackName) (Maybe Target)
  | StackOrchestrate (Maybe StackName) Cmd
  | StackPing (Maybe StackName)
  | StackRunPuppet (Maybe StackName) Target
  | StackSync (Maybe StackName)
  deriving (Show)

data NodeCommand
  = NodeData Text
  | NodeDu Text
  | NodeFacts Text
  | NodeRunPuppet Text
  deriving (Show)

stackParser :: Parser StackCommand
stackParser =
      StackData <$> (subcommand "data" "Return configuration data for a specific property" stackArg) <*> (optText "key" 'k' "Property to look up for")
  <|> StackFacts <$> (subcommand "facts" "Return essential node static information" stackArg) <*> optional (optText "target" 't' "Target subgroup.role")
  <|> StackOrchestrate <$> (subcommand "orch" "Run an orchestration command on the infrastructure" stackArg) <*> (optText "cmd" 'c' "Command to run")
  <|> StackPing <$> subcommand "ping" "Ping nodes" stackArg
  <|> StackRunPuppet <$> (subcommand "runpuppet" "Apply puppet configuration on a specific subgroup.role (async)" stackArg) <*> (optText "target" 't' "Target subgroup.role")
  <|> StackSync  <$> subcommand "sync" "Sync data from master to nodes" stackArg
  where
    stackArg = optional (argText "stack" "Name of stack")

nodeParser :: Parser NodeCommand
nodeParser =
      NodeFacts <$> subcommand "facts" "Return static facts" node_parser
  <|> NodeData  <$> subcommand "data" "Return configuration data" node_parser
  <|> NodeDu <$> subcommand "du" "Return disk usage" node_parser
  <|> NodeRunPuppet <$> subcommand "runpuppet" "Apply configuration by running puppet agent" node_parser
  where
    node_parser = argText "node" "Target node"

commandParser :: Parser Command
commandParser =
      Console     <$  subcommand "console" "Open the specialized salt console" (pure ())
  <|> Stats       <$  subcommand "stats" "Stats (special permission required)" (pure ())
  <|> Stack       <$> subcommand "stack" "Target all nodes in a stack" stackParser
  <|> Node        <$> subcommand "node" "Target one specific node" nodeParser
  <|> Result <$> subcommand "result" ("Display the results of the most recent jobs executed by the user or for a specific id") result_parser
  where
    result_parser = ResultNum <$> (optInt "Num" 'n' "Number of results to display") <|>  ResultJob <$> (optText "job" 'j' "Job id")

parser :: Parser Options
parser
  = Options <$> argText "zone" "ZONE such as dev, staging, testing or prod"
  <*> commandParser

-- -- | One or none.
  -- let nix_file = format (s%"/"%s%".nix") projectDir zone
-- optional' :: Alternative f => f a -> f (Optional a)
-- optional' v = Optional.Specific <$> v <|> pure Optional.Default
