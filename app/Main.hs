{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude
import Turtle hiding ((<>))
import qualified Data.Optional as Optional
import Data.Optional(Optional)
import qualified Data.Text as Text
import Data.Maybe (fromMaybe)
import qualified System.Process as Process hiding (FilePath)

projectDir :: Turtle.FilePath
projectDir = "/home/vagrant/projects/cicd/shell"

defaultStack = "middleware"

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

nixCommand :: Text -> Optional Text -> Maybe Text -> Shell ExitCode
nixCommand zone cmd stack  =  do
  let
    pgr = "nix-shell " <> zone <> ".nix"
    cmdStr = case cmd of
      Optional.Specific c -> pgr <> " --command '" <> (maybe c (\x -> Text.unwords [c, x]) stack) <> "'"
      Optional.Default -> pgr
  pushd projectDir
  liftIO $ interactive cmdStr

run (Options zone Console) = sh (nixCommand zone empty empty)
run (Options zone (Stack (StackPing stack))) = sh (nixCommand zone "ping" stack)
run (Options zone (Stack (StackFacts stack))) = sh (nixCommand zone "facts" stack)
run (Options zone (Stack (StackSync stack))) = sh (nixCommand zone "sync" stack)
run (Options zone (Node (NodeFacts node))) = sh (nixCommand zone "facts_on" (Just node))
run (Options zone (Node (NodeData node))) = sh (nixCommand zone "data_on" (Just node))
run (Options zone (Node (NodeRunPuppet (False, node)))) = sh (nixCommand zone "run_puppet_on" (Just node))
run (Options zone (Node (NodeRunPuppet (True, node)))) = sh (nixCommand zone "run_first_puppet_on" (Just node))
run (Options zone (Orchestrate cmd)) = sh (nixCommand zone "orchestrate" (Just cmd))
run (Options zone (Result (Nothing, Nothing))) = sh (nixCommand zone "result" (Just (show 5)))
run (Options zone (Result (Just num, Nothing))) = sh (nixCommand zone "result" (Just (show num )))
run (Options zone (Result (_, Just jobid))) = sh (nixCommand zone "result_for" (Just (show jobid)))


main :: IO ()
main = options "CICD command line utility" parser >>= run

interactive :: Text -> IO ExitCode
interactive c = do
    let
      c' = Text.unpack c
      cp = (Process.shell c')
            { Process.std_in  = Process.Inherit
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            , Process.delegate_ctlc = True
            }
    (_, _, _, ph) <- Process.createProcess cp
    Process.waitForProcess ph

-- -- | One or none.
  -- let nix_file = format (s%"/"%s%".nix") projectDir zone
-- optional' :: Alternative f => f a -> f (Optional a)
-- optional' v = Optional.Specific <$> v <|> pure Optional.Default
