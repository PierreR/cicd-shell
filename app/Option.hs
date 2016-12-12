{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Option where

import           Protolude
import           Turtle    hiding ((<>))

type Role = Text
type StackName = Text
type Cmd = Text
type Key = Text

data Options = Options Text Command

data Command
  = Console
  | Stats
  | Stack (Maybe StackName, StackCommand)
  | Node NodeCommand
  | Result ResultArg
  deriving (Show)

data ResultArg
  = ResultJob Text
  | ResultNum Int
  deriving (Show)

data StackCommand
  = StackData (Key, Maybe Role)
  | StackFacts (Maybe Role)
  | StackOrchestrate Cmd
  | StackPing (Maybe Role)
  | StackRunPuppet Role
  | StackSync (Maybe Role)
  deriving (Show)

data NodeCommand
  = NodeData Text
  | NodeDu Text
  | NodeFacts Text
  | NodeRunPuppet Text
  deriving (Show)

stackParser :: Parser (Maybe StackName, StackCommand)
stackParser =
  let role_parser = argText "role" "Role might be prefixed by a subgroup ('subgroup.role')"
      stackGroupParser
        = StackData <$> subcommand "data" "Return configuration data for a specific property" ((,) <$> argText "key" "Property to look up for" <*> optional role_parser)
        <|> StackFacts <$> subcommand "facts" "Return essential facts about nodes" (optional role_parser)
        <|> StackOrchestrate <$> subcommand "orch" "Run an orchestration command on the infrastructure" (argText "cmd" "Command to run")
        <|> StackPing <$> subcommand "ping" "Ping nodes" (optional role_parser)
        <|> StackRunPuppet <$> subcommand "runpuppet" "Apply puppet configuration (async)" role_parser
        <|> StackSync  <$> subcommand "sync" "Sync data from master to nodes" (optional role_parser)
  in
    (,) <$> (optional (optText "stack" 's' "Name of stack")) <*> stackGroupParser

nodeParser :: Parser NodeCommand
nodeParser =
      NodeFacts <$> subcommand "facts" "Return static facts" node_parser
  <|> NodeData  <$> subcommand "data" "Return configuration data" node_parser
  <|> NodeDu <$> subcommand "du" "Return disk usage" node_parser
  <|> NodeRunPuppet <$> subcommand "runpuppet" "Apply configuration by running puppet agent" node_parser
  where
    node_parser = argText "node" "Role node"

commandParser :: Parser Command
commandParser =
      Console     <$  subcommand "console" "Open the specialized salt console" (pure ())
  <|> Stats       <$  subcommand "stats" "Stats (special permission required)" (pure ())
  <|> Stack       <$> subcommand "stack" "Role all nodes in a stack" stackParser
  <|> Node        <$> subcommand "node" "Role one specific node" nodeParser
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
