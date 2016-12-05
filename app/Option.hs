{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Option where

import           Protolude
import           Turtle    hiding ((<>))

data Options = Options Text Command

type JobId = Int

data Command
  = Console
  | Stack StackCommand
  | Node NodeCommand
  | Orchestrate Text
  | Result (Maybe Int, Maybe JobId)
  deriving (Show)

data StackCommand
  = StackPing (Maybe Text)
  | StackFacts (Maybe Text)
  | StackSync (Maybe Text)
  deriving (Show)

data NodeCommand
  = NodeFacts Text
  | NodeData Text
  | NodeRunPuppet (Bool, Text)
  deriving (Show)

stackParser :: Parser StackCommand
stackParser =
      StackPing  <$> subcommand "ping" "Ping nodes" stackArg
  <|> StackFacts <$> subcommand "facts" "Return essential node static information" stackArg
  <|> StackSync  <$> subcommand "sync" "Sync data from master to nodes" stackArg
  where
    stackArg = optional (argText "stack" "Name of stack")

nodeParser :: Parser NodeCommand
nodeParser =
      NodeFacts <$> subcommand "facts" "Return static facts" node_parser
  <|> NodeData  <$> subcommand "data" "Return configuration data" node_parser
  <|> NodeRunPuppet <$> subcommand "run-puppet" "Apply configuration by running puppet agent" runpuppet_parser
  where
    node_parser = argText "node" "Target node"
    runpuppet_parser = (,) <$> switch "init" '0' "initial/first time run" <*> node_parser

commandParser :: Parser Command
commandParser =
      Console     <$  subcommand "console" "Open the specialized salt console" (pure ())
  <|> Stack       <$> subcommand "stack" "Target all nodes in a stack" stackParser
  <|> Node        <$> subcommand "node" "Target one specific node" nodeParser
  <|> Orchestrate <$> subcommand "orch" "Run an orchestration command on the infrastructure" (argText "CMD" "Command to run")
  <|> Result <$> subcommand "result" ("Display the results of the most recent jobs executed by the user or for a specific id") result_parser
  where
    result_parser = (,) <$> optional (argInt "NUM" "Number of results to display (default to 5)") <*> optional (optInt "job" 'j' "Job id")

parser :: Parser Options
parser
  = Options <$> argText "zone" "ZONE such as dev, staging, testing or prod"
  <*> commandParser

-- -- | One or none.
  -- let nix_file = format (s%"/"%s%".nix") projectDir zone
-- optional' :: Alternative f => f a -> f (Optional a)
-- optional' v = Optional.Specific <$> v <|> pure Optional.Default
