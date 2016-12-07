{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Optional  (Optional)
import qualified Data.Optional  as Optional
import qualified Data.Text      as Text
import           Protolude      hiding ((<>), FilePath)
import qualified System.Process as Process hiding (FilePath)
import           Turtle

import           Option

projectDir = "/home/vagrant/projects/cicd/shell"
defaultStack = "middleware"
nixFile z = z <> ".nix"
nixCommand z = Text.unwords ["nix-shell", nixFile z]

runCommand :: Text -> Optional Text -> [Text] -> Shell ExitCode
runCommand zone cmd cargs  =  do
  let
    pgr = nixCommand zone
    cmdStr = Optional.optional pgr (\c -> pgr <> " --command '" <> Text.unwords (c : cargs) <> "'" ) cmd
  pushd projectDir
  initEnv zone
  liftIO $ interactive cmdStr

initEnv :: Text -> Shell ExitCode
initEnv zone = do
  let nixfile = fromText (nixFile zone)
  foundFile <- testfile nixfile
  if foundFile
    then pure ExitSuccess
    else proc "./set_nix.sh" [zone] empty

run (Options zone Console) = sh (runCommand zone empty empty)
run (Options zone Stats) = sh (runCommand zone "stats" empty)
run (Options zone (Stack (StackData stack key))) = sh (runCommand zone "stack_data_for" (key : toList stack))
run (Options zone (Stack (StackFacts stack Nothing ))) = sh (runCommand zone "stack_facts"  (toList stack))
run (Options zone (Stack (StackFacts stack (Just target)))) = sh (runCommand zone "stack_facts_on" (target: toList stack))
run (Options zone (Stack (StackOrchestrate stack cmd))) = sh (runCommand zone "stack_orch" (cmd : toList stack))
run (Options zone (Stack (StackPing stack))) = sh (runCommand zone "stack_ping" (toList stack))
run (Options zone (Stack (StackRunPuppet stack target ))) = sh (runCommand zone "stack_runpuppet_on" (target : toList stack))
run (Options zone (Stack (StackSync stack))) = sh (runCommand zone "stack_sync" (toList stack))
run (Options zone (Node (NodeFacts node))) = sh (runCommand zone "node_facts" [node])
run (Options zone (Node (NodeData node))) = sh (runCommand zone "node_data" [node])
run (Options zone (Node (NodeDu node))) = sh (runCommand zone "node_du" [node])
run (Options zone (Node (NodeRunPuppet node))) = sh (runCommand zone "node_runpuppet" [node])
run (Options zone (Result (ResultNum n))) = sh (runCommand zone "result" [show n])
run (Options zone (Result (ResultJob n ))) = sh (runCommand zone "result_for" [n])


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
