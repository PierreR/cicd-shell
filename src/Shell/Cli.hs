-- | Parse command line arguments
module Shell.Cli (
  optionParser
  , AcrossArg(..)
  , DataArg(..)
  , FactArg(..)
  , ResultArg(..)
  , ResultType(..)
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

data DocType
  = HtmlDoc
  | ModListDoc
  | ModDoc Text

data SubCommand
  = Console
  | Data DataArg
  | Facts FactArg
  | Foreman Arg
  | State StateArg
  | Orchestrate OrchArg
  | Stats
  | Du Arg
  | Ping AcrossArg
  | Runpuppet Arg
  | Setfacts SetfactArg
  | Sync AcrossArg
  | Result ResultArg
  | GenTags
  | Service (ServiceAction, ServiceName, Arg)
  deriving (Show)

-- | Orchestrate command with an optional stack.
data OrchArg =
  OrchArg Text (Maybe Text) ExtraFlag
  deriving Show


data DataArg =
  DataArg (Maybe Text) AcrossArg -- ^ Query config data optionally with a key
  deriving Show

data StateArg =
  StateArg Text Text ExtraFlag
  deriving Show

data ForemanArg =
  ForemanArg Text ExtraFlag
  deriving Show

data FactArg
  = FactArg Down AcrossArg -- ^ disconnect & across flags
  deriving Show

data AcrossArg
  = AcrossArg Bool Arg -- ^ Query with the across all stacks flag
  deriving Show


extraFlagParser :: Parser ExtraFlag
extraFlagParser
  = ExtraFlag <$> rawParser <*> verboseParser <*> dryParser

argParser :: Parser Arg
argParser
  = Arg
  <$> optional (arg roleParse (metavar "ROLE" <> help "Role name maybe prefixed by a subgroup ('subgroup.role')"))
  <*> optional (optText (metavar "NODE" <> short 'n' <> help "Target node"))
  <*> optional (optText (metavar "GROUP" <> short 'g' <> help "Target subgroup"))
  <*> optional (optText (metavar "STACK" <> short 's' <> help "Target stack/hostgroup"))
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

verboseParser :: Parser Bool
verboseParser = switch (long "verbose" <> short 'v' <> help "Display the executed command")

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

docTypeParser :: Parser DocType
docTypeParser =
      HtmlDoc <$ subcommand "html" "Open the documentation in a browser" (pure ())
  <|> ModListDoc <$ subcommand "modules" "Output all possible salt execution modules" (pure ())
  <|> ModDoc <$> subcommand "mod" "Doc about a specific salt module" (argText (metavar "NAME" <> help "Module name"))

setfactParser :: Parser SetfactArg
setfactParser
  = SetfactArg
  <$> optText  (short 'n' <> metavar "NODE" <> help "Target node")
  <*> optional (optText (long "subgroup" <> metavar "SUBGROUP" <> help "set subgroup fact"))
  <*> optional (optText (long "role" <> metavar "ROLE" <> help "Set role fact"))
  <*> optional (optText (long "hostgroup" <> metavar "HOSTGROUP" <> help "Set hostgroup fact"))
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
  <|> Data        <$> subcommand "data" "Return configuration data for a specific property" data_parser
  <|> Orchestrate <$> subcommand "orch" "Run an orchestration command on the infrastructure" orchParser
  <|> Facts       <$> subcommand "facts" "Return essential facts about nodes" fact_parser
  <|> Ping        <$> subcommand "ping" "Ping nodes" across_parser
  <|> Du          <$> subcommand "du" "Return disk usage" argParser
  <|> State       <$> subcommand "state" "Apply a specific configuration" stateParser
  <|> Service     <$> subcommand "service" "Service management for a specific node" statusParser
  <|> Foreman     <$> subcommand "foreman" "Display the foreman report in a browser for the specific node" argParser
  <|> Runpuppet   <$> subcommand "runpuppet" "Apply puppet configuration" argParser
  <|> Sync        <$> subcommand "sync" "Syncmetavar  data from master to nodes" across_parser
  <|> Setfacts    <$> subcommand "setfacts" "Set/update the 4 base machine facts" setfactParser
  <|> Result      <$> subcommand "result" "Display the results of the most recent jobs executed by the user or for a specific id" resultParser
  <|> GenTags     <$  subcommand "gentags" "Generate node completion file" (pure ())
  where
    data_parser   = DataArg <$> optional (optText (long "key" <> short 'k' <> metavar "KEY" <> help "Property to look up for" )) <*> across_parser
    fact_parser   = FactArg <$> downParser <*> across_parser
    across_parser = AcrossArg <$> switch (long "all" <> help "Target whole the known stacks" ) <*> argParser

optionParser :: Parser Options
optionParser =
      DocCommand <$> subcommand "doc" "Documentation utilities" docTypeParser
  <|> ZoneCommand . Zone <$> argText (metavar "ZONE" <> help "ZONE (dev|testing|staging|prod)") <*> subCommandParser
