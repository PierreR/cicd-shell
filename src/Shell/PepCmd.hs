{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}


-- | Generate Pepper command (PepCmd).
module Shell.PepCmd (
    PepCmd
  , CmdMode (..)
  , CmdMsg (CmdMsg)
  , HasPepCmd(..)
  , runCmd
  , consoleCmd
  , duCmd
  , factCmd
  , foremanCmd
  , genTagsCmd
  , pingCmd
  , runpuppetCmd
  , serviceCmd
  , setfactsCmd
  , statCmd
  , syncCmd
) where

import qualified Data.Text         as Text
import           Text.RawString.QQ
import qualified Data.List.NonEmpty as NonEmpty

import           Shell.Prelude
import           Shell.Type
import           Shell.Target
import           Shell.PepCmd.Utils


-- | Each command determines an appropriate mode to be run with.
data CmdMode
  = NormalMode
  | ConsoleMode -- ^ Request for the console
  | RetryMode -- ^ Command that keep requesting a result until it gets one (such as `report`)
  | ProgressMode Integer -- ^ Long running command such as `runpuppet`
  deriving Show

-- | Command data
-- See 'HasPepCmd'
data PepCmd
  = PepCmd
  { _pep :: Text -- Salt (pep) command
  , _jq  :: Text -- Jq pretty printer
  , _beforeMsg :: Maybe CmdMsg -- Message to be displayed before launching the command
  , _cmdMode :: CmdMode -- ^ Command mode
  } deriving Show

defCmd = PepCmd mempty "jq ." empty NormalMode

-- | A message to be displayed to the user.
-- Ask for confirmation if 'Bool' is set to True.
data CmdMsg =
  CmdMsg Bool (Doc AnsiStyle)
  deriving Show

makeClassy ''PepCmd

-- | Command to open the specialized console.
consoleCmd :: Zone -> FilePath -> PepCmd
consoleCmd (Zone zone) datadir =
  let
    fp = datadir </> "share/default.nix"
  in
  defCmd & pep .~ "nix-shell " <> toS fp <> " --argstr zone " <> zone
         & cmdMode .~ ConsoleMode

-- | Regenerate the list of nodes cached for the auto-completion feature.
genTagsCmd :: Zone -> FilePath -> PepCmd
genTagsCmd (Zone zone) cfdir =
  let nodefile = cfdir <> "/.nodes-" <> toS zone
  in defCmd & pep .~ "pepper \"*\" test.ping"
            & jq .~ ("jq '.return[0]' | jq keys | jq -r 'join (\" \")' > " <> toS nodefile)
            & (beforeMsg ?~ CmdMsg False ("Generating " <> pretty nodefile <> line))


-- | Command to gather stats about up and down nodes.
statCmd :: PepCmd
statCmd =
  defCmd & pep .~ "pepper --client=runner manage.status"
         & jq .~
           [r|
              jq '.return[0] | [.up , .up + .down | length] as $stats | {up, down, stats: "\($stats[0]) up of \($stats[1])"}'
           |]

-- | Run puppet command.
runpuppetCmd :: Maybe Text -> Bool -> Target -> PepCmd
runpuppetCmd stack noop =
  let
    stack_arg = case stack of
      Nothing -> mempty
      Just s  ->  " hostgroup=" <> s
    noop_arg = if noop then " noop=True" else mempty
  in \case
    target@Target {_node = Nothing} ->
      defCmd & pep .~ ( pepperCompoundTarget False target <> " --client=local_async cicd.run_puppet zone=" <> target^.zone <> noop_arg)
             & jq .~ "jq '.return'"
             & (beforeMsg ?~ CmdMsg True ("Run puppet on " <> pretty target))

    target@Target {_node = Just n} ->
      defCmd & pep .~ ( "pepper " <> n <> " -t 300 cicd.run_puppet zone=" <> target^.zone <> noop_arg <> stack_arg)
             & jq .~ [r|
                       jq -r '.return[] |
                       to_entries |
                       (.[] |
                       if (.value|type) == "object" then
                         ( if .value.retcode? == 0 then
                             "\n\u001B[1;32mSUCCESS\u001B[0m for "
                           else
                            "\n\u001B[1;31mFAILURE\u001B[0m for "
                           end
                           + .key + ":" ,
                           if .value.stderr? != "" then
                             .value.stdout + "\n******\n" + .value.stderr?
                           else
                             .value.stdout?
                           end
                         )
                       else
                         .value
                       end )'
                     |]
             & cmdMode .~ ProgressMode 300

--   | Write one or more of [subgroup, role, hostgroup, zone] fact(s).
-- The command always executes on a single node.
setfactsCmd :: SetfactArg -> PepCmd
setfactsCmd SetfactArg {..} =
  defCmd & pep .~ ("pepper '" <> _node <> "' cicd.set_facts " <> join_facts)
  where
    join_facts =
      let join_values = Text.intercalate " " . catMaybes
      in join_values
           [ ("hostgroup=" <>) <$> _hostgroup
           , ("subgroup=" <>) <$> _subgroup
           , ("role=" <>) <$> _role
           , ("instance=" <>) <$> _inst
           , ("zone=" <>) <$> _zone
           , ("dc=" <>) <$> _dc
           ]

-- | Sync minion with the saltmaster
syncCmd :: Bool -> Target -> PepCmd
syncCmd across target@Target { _node = Nothing} =
  defCmd & pep .~ (pepperCompoundTarget across target <> " saltutil.sync_all")
syncCmd _ Target {_node = Just n} =
  defCmd & pep .~ ("pepper '" <> n <> "' saltutil.sync_all")

-- | Run a command to a specific node (salt-run)
runCmd :: Text -> (Maybe Text) -> PepCmd
runCmd cmd n =
  defCmd & pep .~ "pepper --client=runner " <> cmd <> (maybe mempty (" minion_id=" <>) n)

-- | Disk usage
duCmd :: Target -> PepCmd
duCmd  Target {_node = Just n} =
  defCmd & pep .~ ("pepper " <> n <> " disk.percent")
         & jq .~ "jq '.return[0]'"
duCmd target@Target {_node = Nothing} =
  defCmd & pep .~ (pepperCompoundTarget False target <> " disk.percent")
         & jq .~ "jq '.return[0]'"

-- | Restart or ask for the status of any linux service.
serviceCmd :: ServiceAction -> ServiceName -> Target -> PepCmd
serviceCmd ServiceStatus (ServiceName name) target@Target {_node = Nothing} =
  defCmd & pep .~ (pepperCompoundTarget False target <> " service.status " <> name)
         & jq .~ "jq '.return[0]'"
serviceCmd ServiceStatus (ServiceName name) Target {_node = Just n} =
  defCmd & pep .~ ("pepper " <> n <> " service.status " <> name)
         & jq .~ "jq '.return[0]'"
serviceCmd ServiceRestart (ServiceName name) Target {_node = Just n} =
  defCmd & pep .~ ("pepper " <> n <> " service.restart " <> name)
         & jq .~ "jq '.return[0]'"
serviceCmd ServiceRestart _ Target {_node = Nothing} =
  panic "To restart a service, you need to specify a node with -n"

-- | Display a set of interesting facts such as fqdn, ip, role, ...
factCmd :: Maybe FilePath -> Bool -> Target -> PepCmd
factCmd fpath across target@Target {_node = Nothing} =
  defCmd & pep .~ (pepperCompoundTarget across target <> " grains.item os osrelease fqdn fqdn_ip4 hostgroup subgroup role instance dc puppetmaster_timestamp puppetmaster_jenkins_job")
         & jq .~
           [r|
             jq '.return[] | .[] | { fqdn, ip: .fqdn_ip4[0], os:  "\(.os) \(.osrelease)", hostgroup, subgroup, role, instance, dc, "puppet run": .puppetmaster_timestamp, "jenkins job" : .puppetmaster_jenkins_job}' \
           |]
           <> maybe mempty (\p -> " | tee " <> toS p) fpath
           <> " | jq ."
factCmd fpath _ Target {_node = Just n} =
  defCmd & pep .~ ("pepper " <> n <> " grains.item os osrelease fqdn fqdn_ip4 hostgroup subgroup role instance dc puppetmaster_timestamp puppetmaster_jenkins_job")
         & jq .~
           [r|
              jq '.return[0] |
              .[] |
              { fqdn, ip: .fqdn_ip4[0], os:  "\(.os) \(.osrelease)", hostgroup, subgroup, role, instance, dc, "puppet run": .puppetmaster_timestamp, "jenkins job" : .puppetmaster_jenkins_job }' \
           |]
           <> maybe mempty (\p -> " | tee " <> toS p) fpath
           <> " | jq ."

pingCmd :: Bool -> Target -> PepCmd
pingCmd across target@Target {_node = Nothing} =
  defCmd & pep .~ ( pepperCompoundTarget across target <> " test.ping")
      & jq .~ "jq '.return[0]'"
pingCmd _ Target {_node = Just n} =
  defCmd & pep .~ ( "pepper '" <> n <> "' test.ping")
      & jq .~ "jq '.return[0]'"

foremanCmd :: Text -> Target -> Text
foremanCmd foremanUrl target =
  let role_facts (Role Nothing r) = "facts.role=" <> r
      role_facts (Role (Just (Subgroup g)) r) = "facts.subgroup=" <> g <> "+and+" <> "facts.role=" <> r
      url = case target of
        Target{_node = Just n} -> "/hosts/" <>  n <> "/config_reports"
        target@Target{_node = Nothing} -> "?search=" <> joinTargetWith "+and+"
                                                          [ Just ("facts.zone=" <> target^.zone)
                                                          , Just ("facts.hostgroup=" <> NonEmpty.head(target^.stacks))
                                                          , ("facts.subgroup=" <>) <$> target^.subgroup
                                                          , role_facts <$> target^.role
                                                          ]
  in
  "xdg-open " <> foremanUrl <> url <> " &> /dev/null"
