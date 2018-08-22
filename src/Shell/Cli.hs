-- | Parse command line arguments
module Shell.Cli (
  optionParser
  , AcrossArg(..)
  , FactArg(..)
  , ResultArg(..)
  , ResultType(..)
  , RunpuppetArg(..)
  , OrchArg(..)
  , StateArg(..)
  , DocType(..)
  , Options(..)
  , SubCommand(..)
  ) where

import qualified Data.Text     as Text

import           Shell.Options
import           Shell.Prelude
import           Shell.Type

data ResultArg = ResultArg ExtraFlag ResultType deriving Show

data ResultType
  = ResultJob Text
  | ResultNum Natural
  deriving (Show)

data Options
  = ZoneCommand Zone SubCommand
  | DocCommand DocType
  | Password

data DocType
  = HtmlDoc
  | ModListDoc
  | ModDoc Text

data SubCommand
  = Console
  | Facts FactArg
  | Foreman Arg
  | State StateArg
  | Orchestrate OrchArg
  | Stats
  | Du Arg
  | Ping AcrossArg
  | Runpuppet RunpuppetArg
  | Setfacts SetfactArg
  | Sync AcrossArg
  | Result ResultArg
  | GenTags
  | Service (ServiceAction, ServiceName, Arg)
  | Validate Arg
  deriving (Show)

-- | Orchestrate command with an optional stack.
data OrchArg =
  OrchArg Text (Maybe Text) ExtraFlag
  deriving Show

data RunpuppetArg =
  RunpuppetArg Arg Bool
  deriving Show

data StateArg =
  StateArg Text Text ExtraFlag
  deriving Show

data FactArg
  = FactArg Refresh Down AcrossArg -- ^ disconnect & across flags
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
  <*> optional (optText (metavar "NODE" <> short 'n' <> help "Target node"))
  <*> optional (optText (metavar "STACK" <> short 's' <> help "Target stack/hostgroup"))
  <*> optional (optText (metavar "GROUP" <> short 'g' <> help "Target subgroup"))
  <*> optional (optText (metavar "INSTANCE" <> short 'i' <> help "Target instance"))
  <*> extraFlagParser


stateParser :: Parser StateArg
stateParser =
  StateArg
  <$> argText (metavar "CMD" <> help "SubCommand to run")
  <*> optText (metavar "NODE" <> short 'n' <> help "Target node")
  <*> extraFlagParser

orchParser :: Parser OrchArg
orchParser =
  OrchArg
  <$> argText (metavar "CMD" <> help "SubCommand to run")
  <*> optional (optText (short 's' <> help "Target stack/hostgroup" ))
  <*> extraFlagParser


rawParser :: Parser Bool
rawParser = switch (long "raw" <> help "Raw output (no jq)")

downParser :: Parser Down
downParser = Down <$> switch (long "down" <> help "Query disconnected node")
refreshParser = Refresh <$> switch (long "refresh" <> help "Refresh the cache")

quietParser :: Parser Bool
quietParser = switch (long "quiet" <> help "Quieter output. Won't display the command.")

dryParser :: Parser Bool
dryParser = switch (long "dry" <> help "Display the command without execution")

resultParser :: Parser ResultArg
resultParser
  = ResultArg
  <$> extraFlagParser
  <*> (ResultNum <$> optRead auto (short 'n' <> help "Number of results to display") <|> ResultJob <$> optText (metavar "JOB" <> short 'j' <> help "Job id"))

roleParse :: Text -> Maybe Role
roleParse = parse_role . Text.splitOn "."
  where parse_role [g, r] = Just $ Role (Just (Subgroup g)) r
        parse_role [r] =  Just $ Role Nothing r
        parse_role _ = Nothing

serviceParse :: Text -> Maybe ServiceAction
serviceParse "status"  = Just ServiceStatus
serviceParse "restart" = Just ServiceRestart
serviceParse _         = Nothing

docTypeParser =
      HtmlDoc <$ subcommand "html" "Open the documentation in a browser" (pure ())
  <|> ModListDoc <$ subcommand "modules" "Output all possible salt execution modules" (pure ())
  <|> ModDoc <$> subcommand "mod" "Doc about a specific salt module" (argText (metavar "NAME" <> help "Module name"))

setfactParser :: Parser SetfactArg
setfactParser
  = SetfactArg
  <$> optText  (short 'n' <> metavar "NODE" <> help "Target node")
  <*> optional (optText (long "hostgroup" <> metavar "HOSTGROUP" <> help "Set hostgroup fact"))
  <*> optional (optText (long "subgroup" <> metavar "SUBGROUP" <> help "set subgroup fact"))
  <*> optional (optText (long "role" <> metavar "ROLE" <> help "Set role fact"))
  <*> optional (optText (long "instance" <> metavar "INSTANCE" <> help "Set instance fact"))
  <*> optional (optText (long "zone" <> metavar "ZONE" <> help "Set zone fact"))
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
  <|> Stats       <$  subcommand "stats" "Stats (special permission required)" (pure ())
  <|> Orchestrate <$> subcommand "orch" "Run an orchestration command on the infrastructure" orchParser
  <|> Facts       <$> subcommand "facts" "Return essential facts about nodes" fact_parser
  <|> Ping        <$> subcommand "ping" "Ping nodes" across_parser
  <|> Du          <$> subcommand "du" "Return disk usage" argParser
  <|> State       <$> subcommand "state" "Apply a specific configuration" stateParser
  <|> Service     <$> subcommand "service" "Service management for a specific node" statusParser
  <|> Foreman     <$> subcommand "foreman" "Display the foreman report in a browser" argParser
  <|> Runpuppet   <$> subcommand "runpuppet" "Apply puppet configuration" runpuppet_parser
  <|> Sync        <$> subcommand "sync" "Sync metavar  data from master to nodes" across_parser
  <|> Setfacts    <$> subcommand "setfacts" "Set/update the 4 base machine facts" setfactParser
  <|> Result      <$> subcommand "result" "Results of the most user recent jobs or for a specific id" resultParser
  <|> GenTags     <$  subcommand "gentags" "Generate node completion file" (pure ())
  <|> Validate    <$> subcommand "validate" "Validate node with inspec" argParser
  where
    fact_parser   = FactArg <$> refreshParser <*> downParser <*> across_parser
    across_parser = AcrossArg <$> argParser <*> switch (long "all" <> help "Target whole the known stacks" )
    runpuppet_parser = RunpuppetArg <$> argParser <*> switch (long "noop" <> help "Run puppet with the --noop argument" )

optionParser :: Parser Options
optionParser =
      Password <$ subcommand "pass" "Change password" (pure ())
  <|> DocCommand <$> subcommand "doc" "Documentation utilities" docTypeParser
  <|> ZoneCommand . Zone <$> argText (metavar "ZONE" <> help "ZONE (dev|testing|staging|prod)") <*> subCommandParser
