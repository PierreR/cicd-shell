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

runCommand :: Text -> Optional Text -> Maybe Text -> Shell ExitCode
runCommand zone cmd stack  =  do
  let
    pgr = nixCommand zone
    cmdStr = Optional.optional pgr (\c -> pgr <> " --command '" <> (maybe c (\x -> Text.unwords [c, x]) stack) <> "'" ) cmd
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
run (Options zone (Stack (StackPing stack))) = sh (runCommand zone "ping" stack)
run (Options zone (Stack (StackFacts stack))) = sh (runCommand zone "facts" stack)
run (Options zone (Stack (StackSync stack))) = sh (runCommand zone "sync" stack)
run (Options zone (Node (NodeFacts node))) = sh (runCommand zone "facts_on" (Just node))
run (Options zone (Node (NodeData node))) = sh (runCommand zone "data_on" (Just node))
run (Options zone (Node (NodeRunPuppet (False, node)))) = sh (runCommand zone "run_puppet_on" (Just node))
run (Options zone (Node (NodeRunPuppet (True, node)))) = sh (runCommand zone "run_first_puppet_on" (Just node))
run (Options zone (Orchestrate cmd)) = sh (runCommand zone "orchestrate" (Just cmd))
run (Options zone (Result (ResultNum n))) = sh (runCommand zone "result" (Just (show n )))
run (Options zone (Result (ResultJob n ))) = sh (runCommand zone "result_for" (Just (show n)))


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
