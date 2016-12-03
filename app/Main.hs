{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import qualified Data.Optional as Optional
import Data.Optional(Optional)
import qualified Data.Text as Text
import Data.Maybe (fromMaybe)
import qualified System.Process as Process hiding (FilePath)

projectDir :: Turtle.FilePath
projectDir = "/home/vagrant/projects/cicd/shell"

defaultStack = "middleware"


data Options = Options Text Command

data Command
  = Console
  | Ping (Maybe Text)
  deriving (Show)

secParser :: Parser Command
secParser =
  Console <$ (subcommand "console" "Open the specialized salt console" (pure ()))
  <|> Ping <$> (subcommand "ping" "Ping your nodes"
               (optional (argText "stack" "Name of a stack")))

parser :: Parser Options
parser
  = Options <$> argText "zone" "ZONE such as dev, staging, testing or prod"
  <*> secParser


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
run (Options zone (Ping stack)) = sh (nixCommand zone "ping" stack)

main :: IO ()
main = options "Orchestration (pepper) command line utility" parser >>= run

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
