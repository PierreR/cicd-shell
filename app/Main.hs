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

nixpkgs = "12a057cbe07a0ee30b28b4edb39c0a453a4d0556"
-- nixpkgs= "ca9ce9ace4405acfeb1ff3eb7edaa54827f9b340"
projectDir = "/home/vagrant/projects/cicd/shell"
defaultStack = "middleware"
nixFile z = z <> ".nix"
nixCommand z = Text.unwords ["nix-shell", nixFile z]

runCommand :: Text -> Optional Text -> [Text] -> IO ()
runCommand zone cmd cargs  =  sh $ do
  let
    pgr = nixCommand zone <>  " -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/" <> nixpkgs <> ".tar.gz"
    cmdStr = Optional.optional pgr (\c -> pgr <> " --command '" <> Text.unwords (c : cargs) <> "'" ) cmd

  pushd projectDir
  initEnv zone
  interactive cmdStr

  where
    initEnv :: Text -> Shell ExitCode
    initEnv zone = do
      let nixfile = fromText (nixFile zone)
      foundFile <- testfile nixfile
      if foundFile
        then pure ExitSuccess
        else proc "./set_nix.sh" [zone] empty

run (Options zone Console)                                       = runCommand zone empty empty
run (Options zone Stats)                                         = runCommand zone "stats" empty
run (Options zone (Stack (stack, StackData (key, Nothing))))     = runCommand zone "stack_data_for" (key : toList stack)
run (Options zone (Stack (stack, StackData (key, Just target)))) = runCommand zone "stack_data_for_on" (key : target: toList stack)
run (Options zone (Stack (stack, StackFacts Nothing)))           = runCommand zone "stack_facts"  (toList stack)
run (Options zone (Stack (stack, StackFacts (Just target))))     = runCommand zone "stack_facts_on" (target: toList stack)
run (Options zone (Stack (stack, StackOrchestrate cmd)))         = runCommand zone "stack_orch" (cmd : toList stack)
run (Options zone (Stack (stack, StackPing Nothing)))            = runCommand zone "stack_ping" (toList stack)
run (Options zone (Stack (stack, StackPing (Just target))))      = runCommand zone "stack_ping_on" (target: toList stack)
run (Options zone (Stack (stack, StackRunPuppet target )))       = runCommand zone "stack_runpuppet_on" (target : toList stack)
run (Options zone (Stack (stack, StackSync Nothing)))            = runCommand zone "stack_sync" (toList stack)
run (Options zone (Stack (stack, StackSync (Just target))))      = runCommand zone "stack_sync_on" (target : toList stack)
run (Options zone (Node (NodeFacts node)))                       = runCommand zone "node_facts" [node]
run (Options zone (Node (NodeData node)))                        = runCommand zone "node_data" [node]
run (Options zone (Node (NodeDu node)))                          = runCommand zone "node_du" [node]
run (Options zone (Node (NodeRunPuppet node)))                   = runCommand zone "node_runpuppet" [node]
run (Options zone (Result (ResultNum n)))                        = runCommand zone "result" [show n]
run (Options zone (Result (ResultJob n )))                       = runCommand zone "result_for" [n]


-- run (Options zone (Stack (stack, StackFacts (Just target)))) = putText (Text.unwords (zone: "facts" : target : toList stack))
-- run (Options zone (Stack (stack, StackFacts Nothing ))) = putText (Text.unwords (zone: "facts" :toList stack))
-- run (Options zone (Node target)) = putText (zone <> " " <> target)

main :: IO ()
main = options "CICD command line utility" parser >>= run

interactive :: MonadIO io => Text -> io ExitCode
interactive c = do
    let
      c' = Text.unpack c
      cp = (Process.shell c')
            { Process.std_in  = Process.Inherit
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            , Process.delegate_ctlc = True
            }
    (_, _, _, ph) <- liftIO $ Process.createProcess cp
    liftIO $ Process.waitForProcess ph
