module Shell.Option where

import           Turtle.Options

import           Shell.Type
import           Shell.Prelude

data ResultArg
  = ResultJob Text
  | ResultNum Natural
  deriving (Show)

data Command
  = ZoneCommand Zone SubCommand
  | HelpCommand HelpType

data HelpType = HtmlHelp

data SubCommand
  = Console
  | Data DataArg
  | Facts FactArg
  | Orchestrate OrchArg
  | Stats
  | Du Arg
  | Ping AcrossArg
  | Runpuppet Arg
  | Sync AcrossArg
  | Result ResultArg
  | GenTags
  | Service (ServiceAction, ServiceName, Arg)
  deriving (Show)


data OrchArg =
  OrchArg Text (Maybe Text) -- ^ Orchestrate a command optionally with a stack
  deriving Show

data DataArg =
  DataArg (Maybe Text) Arg -- ^ Query config data optionally with a key
  deriving Show

data FactArg
  = FactArg Bool Bool Arg -- ^ Query facts with the across all stacks and disconnect flags
  deriving Show

data AcrossArg
  = AcrossArg Bool Arg -- ^ Query with the across all stacks flag
  deriving Show

data Options = Options Command

data Arg
  = Arg
  { _role     :: Maybe Text
  , _node     :: Maybe Text
  , _subgroup :: Maybe Text
  , _stack    :: Maybe Text
  } deriving Show

argParser :: Parser Arg
argParser
  = Arg
  <$> optional (argText "role" "Role name maybe prefixed by a subgroup ('subgroup.role')")
  <*> optional (optText "node" 'n' "Target node")
  <*> optional (optText "subgroup" 'g' "Target subgroup")
  <*> optional (optText "stack" 's' "Target stack/hostgroup")

optNatural :: ArgName -> ShortName -> Optional HelpMessage -> Parser Natural
optNatural = optRead

serviceParse :: Text -> Maybe ServiceAction
serviceParse "status" = Just ServiceStatus
serviceParse "restart" = Just ServiceRestart
serviceParse _        = Nothing

helpTypeParse :: Text -> Maybe HelpType
helpTypeParse "html" = Just HtmlHelp
helpTypeParse _ = Nothing

subCommandParser :: Parser SubCommand
subCommandParser =
      Console     <$  subcommand "console" "Open the cicd console" (pure ())
  <|> Stats       <$  subcommand "stats" "Stats (special permission required)" (pure ())
  <|> Data        <$> subcommand "data" "Return configuration data for a specific property" data_parser
  <|> Orchestrate <$> subcommand "orch" "Run an orchestration command on the infrastructure" orch_parser
  <|> Facts       <$> subcommand "facts" "Return essential facts about nodes" fact_parser
  <|> Ping        <$> subcommand "ping" "Ping nodes" across_parser
  <|> Du          <$> subcommand "du" "Return disk usage" argParser
  <|> Service     <$> subcommand "service" "Service management for a specific node" status_parser
  <|> Runpuppet   <$> subcommand "runpuppet" "Apply puppet configuration" argParser
  <|> Sync        <$> subcommand "sync" "Sync data from master to nodes" across_parser
  <|> Result      <$> subcommand "result" "Display the results of the most recent jobs executed by the user or for a specific id" result_parser
  <|> GenTags     <$  subcommand "gentags" "Generate node completion file" (pure ())
  where
    result_parser = ResultNum <$> optNatural "Num" 'n' "Number of results to display" <|>  ResultJob <$> optText "job" 'j' "Job id"
    data_parser   = DataArg <$> optional (optText "key" 'k' "Property to look up for" ) <*> argParser
    fact_parser   = FactArg <$> switch "all" 'a' "Target whole the known stacks" <*> switch "down" 'd' "Query down node" <*> argParser
    across_parser = AcrossArg <$> switch "all" 'a' "Target whole the known stacks" <*> argParser
    orch_parser   = OrchArg <$> argText "cmd" "SubCommand to run" <*> optional (optText "stack" 's' "Target stack/hostgroup" )
    status_parser = (,,) <$> arg serviceParse "action" "Use 'status' or 'restart'" <*> (ServiceName <$> argText "service" "Service name") <*> argParser

commandParser :: Parser Command
commandParser =
      HelpCommand <$> subcommand "help" "Help utilities" (arg helpTypeParse "type" "(html) to open the guide in a browser")
  <|> ZoneCommand . Zone <$> argText "zone" "ZONE such as dev, staging, testing or prod" <*> subCommandParser

parser :: Parser Options
parser =
      Options <$> commandParser

-- -- | One or none.
  -- let nix_file = format (s%"/"%s%".nix") projectDir zone
-- optional' :: Alternative f => f a -> f (Optional a)
-- optional' v = Optional.Specific <$> v <|> pure Optional.Default
