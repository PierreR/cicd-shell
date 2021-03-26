-- | Parse command line arguments
module Shell.Cli (
  optionParser
  , AcrossArg(..)
  , FactArg(..)
  , RunpuppetArg(..)
  , RunArg(..)
  , Options(..)
  , SubCommand(..)
  ) where

import qualified Data.Text     as Text

import           Shell.Options
import           Shell.Prelude
import           Shell.Type

data Options
  = ZoneCommand Zone SubCommand
  | Password

data SubCommand
  = Console
  | Facts FactArg
  | Foreman Arg
  | Run RunArg
  | Stats
  | Du Arg
  | Ping AcrossArg
  | Runpuppet RunpuppetArg
  | Setfacts SetfactArg
  | Sync AcrossArg
  | GenTags
  | Service (ServiceAction, ServiceName, Arg)
  deriving (Show)

data RunpuppetArg =
  RunpuppetArg Arg Bool
  deriving Show

data RunArg =
  RunArg Text (Maybe Text) ExtraFlag
  deriving Show

data FactArg
  = FactArg Refresh AcrossArg -- ^ disconnect & across flags
  deriving Show

-- we still want to be able to do cicd prod jenkins.slave --all
data AcrossArg
  = AcrossArg Arg Bool-- ^ Query with the across all stacks flag
  deriving Show


extraFlagParser :: Parser ExtraFlag
extraFlagParser
  = ExtraFlag <$> rawParser <*> quietParser <*> dryParser

argParser :: Parser Arg
argParser
  = Arg
  <$> optional (arg roleParse (metavar "ROLE" <> help "Role name maybe prefixed by a subgroup ('subgroup.role')"))
  <*> optional (optText (metavar "NODE" <> short 'n' <> long "node" <> help "Target node"))
  <*> optional (optText (metavar "STACK" <> short 's' <> long "stack" <> help "Target stack/hostgroup"))
  <*> optional (optText (metavar "GROUP" <> short 'g' <> long "group" <> help "Target subgroup"))
  <*> optional (optText (metavar "INSTANCE" <> short 'i' <> long "instance" <> help "Target instance"))
  <*> extraFlagParser

runParser :: Parser RunArg
runParser =
  RunArg
  <$> argText (metavar "CMD" <> help "SubCommand to run")
  <*> optional (optText (metavar "NODE" <> short 'n' <> help "Target node"))
  <*> extraFlagParser

rawParser :: Parser Raw
rawParser = flag (Raw False) (Raw True) (long "raw" <> help "Raw output (no jq)")

refreshParser = Refresh <$> switch (long "refresh" <> help "Refresh the cache")

quietParser :: Parser Verbosity
quietParser = flag Verbose Quiet (long "quiet" <> help "Quieter output. Won't display the command.")

dryParser :: Parser Dry
dryParser = flag (Dry False) (Dry True) (long "dry" <> help "Display the command without execution")

roleParse :: Text -> Maybe Role
roleParse = parse_role . Text.splitOn "."
  where parse_role [g, r] = Just $ Role (Just (Subgroup g)) r
        parse_role [r] =  Just $ Role Nothing r
        parse_role _ = Nothing

serviceParse :: Text -> Maybe ServiceAction
serviceParse "status"  = Just ServiceStatus
serviceParse "restart" = Just ServiceRestart
serviceParse _         = Nothing

setfactParser :: Parser SetfactArg
setfactParser
  = SetfactArg
  <$> optText  (short 'n' <> metavar "NODE" <> help "Target node")
  <*> optional (optText (long "hostgroup" <> metavar "HOSTGROUP" <> help "Set hostgroup fact"))
  <*> optional (optText (long "subgroup" <> metavar "SUBGROUP" <> help "set subgroup fact"))
  <*> optional (optText (long "role" <> metavar "ROLE" <> help "Set role fact"))
  <*> optional (optText (long "instance" <> metavar "INSTANCE" <> help "Set instance fact"))
  <*> optional (optText (long "zone" <> metavar "ZONE" <> help "Set zone fact"))
  <*> optional (optText (long "dc" <> metavar "DATACENTER" <> help "Set dc fact"))
  <*> extraFlagParser

statusParser :: Parser (ServiceAction, ServiceName, Arg)
statusParser
  = (,,)
  <$> arg serviceParse (metavar "ACTION" <> help "Use 'status' or 'restart'")
  <*> (ServiceName <$> argText (metavar "SERVICE" <> help "Service name"))
  <*> argParser

subCommandParser :: Parser SubCommand
subCommandParser =
      Console     <$  subcommand "console" "Open the cicd console" (pure ())
  <|> Facts       <$> subcommand "facts" "Return essential facts about nodes" fact_parser
  <|> Runpuppet   <$> subcommand "runpuppet" "Apply puppet configuration" runpuppet_parser
  <|> Setfacts    <$> subcommand "setfacts" "Set/update the 4 base machine facts" setfactParser
  <|> Foreman     <$> subcommand "foreman" "Display the foreman report in a browser" argParser
  <|> Run         <$> subcommand "run" "Execute a command on the master" runParser
  <|> Service     <$> subcommand "service" "Service management for a specific node" statusParser
  <|> Du          <$> subcommand "du" "Return disk usage" argParser
  <|> Ping        <$> subcommand "ping" "Ping nodes" across_parser
  <|> Sync        <$> subcommand "sync" "Sync metavar  data from master to nodes" across_parser
  <|> Stats       <$  subcommand "stats" "Stats (special permission required)" (pure ())
  <|> GenTags     <$  subcommand "gentags" "Generate node completion file" (pure ())
  where
    fact_parser   = FactArg <$> refreshParser <*> across_parser
    across_parser = AcrossArg <$> argParser <*> switch (long "all" <> help "Target whole the known stacks" )
    runpuppet_parser = RunpuppetArg <$> argParser <*> switch (long "noop" <> help "Run puppet with the --noop argument" )

optionParser :: Parser Options
optionParser =
      Password <$ subcommand "pass" "Change password" (pure ())
  <|> ZoneCommand . Zone <$> argText (metavar "ZONE" <> help "ZONE (dev|testing|staging|prod)" <> completeWith ["dev", "testing", "staging", "prod"]) <*> subCommandParser
