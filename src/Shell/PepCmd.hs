{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}


-- | Generate a 'PepCmd' command
module Shell.PepCmd (
    PepCmd
  , CmdMode (..)
  , CmdMsg (CmdMsg)
  , HasPepCmd(..)
  , consoleCmd
  , dataCmd
  , duCmd
  , factCmd
  , foremanCmd
  , genTagsCmd
  , genSaltModListCmd
  , genSaltModjsonCmd
  , orchCmd
  , pingCmd
  , resultCmd
  , runpuppetCmd
  , serviceCmd
  , setfactsCmd
  , statCmd
  , stateCmd
  , syncCmd
  , validateCmd
) where

import qualified Data.Text         as Text
import           Text.RawString.QQ
import qualified Data.List.NonEmpty as NonEmpty

import           Shell.Prelude
import           Shell.Type
import           Shell.Target
import           Shell.PepCmd.Utils

panic' = panic "The impossible happened. The option parser should void the case"

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
    completion_cmd = "source " <> toS (datadir </> "share/completion.sh ") <> zone <> "; return"
  in
  defCmd & pep .~ completion_cmd
         & cmdMode .~ ConsoleMode

-- | Regenerate the list of nodes cached for the auto-completion feature.
genTagsCmd :: Zone -> FilePath -> PepCmd
genTagsCmd (Zone zone) cfdir =
  let nodefile = cfdir <> "/.nodes-" <> toS zone
  in defCmd & pep .~ "pepper \"*\" test.ping"
            & jq .~ ("jq '.return[0]' | jq keys | jq -r 'join (\" \")' > " <> toS nodefile)
            & (beforeMsg ?~ CmdMsg False ("Generating " <> pretty nodefile))

-- | Regenerate the salt module list used for auto-completion
genSaltModListCmd :: Text -> PepCmd
genSaltModListCmd fpath =
  let jsonfile = fpath <> ".json"
  in defCmd & pep .~ "pepper --client=runner doc.execution"
            & jq .~ ("jq '.return[0] | keys' | tee " <> jsonfile <> " | jq -r 'join (\" \")' > " <>  fpath)

-- | Regenerate the module documentation file.
genSaltModjsonCmd :: Text -> PepCmd
genSaltModjsonCmd fpath =
  defCmd & pep .~ "pepper --client=runner doc.execution"
         & jq .~ ("jq '.return[0]' > " <> fpath <> ".json")

-- | Command to gather stats about up and down nodes.
statCmd :: PepCmd
statCmd =
  defCmd & pep .~ "pepper --client=runner manage.status"
         & jq .~
           [r|
              jq '.return[0] | [.up , .up + .down | length] as $stats | {up, down, stats: "\($stats[0]) up of \($stats[1])"}'
           |]

-- | Orchestration command.
orchCmd :: Text -> Text -> PepCmd
orchCmd cmd stack =
  defCmd & pep .~ ("pepper state.orchestrate --client=runner mods=orch." <> cmd <> " saltenv=" <> stack)

-- | Run puppet command.
runpuppetCmd :: Target -> PepCmd
runpuppetCmd = \case
  target@Target {_node = Nothing} ->
    defCmd & pep .~ ( pepperCompoundTarget False target <> " --client=local_async cicd.run_puppet zone=" <> target^.zone <> " hostgroup=" <> NonEmpty.head(target^.stacks))
           & jq .~ "jq '.return'"
           & (beforeMsg ?~ CmdMsg True ("Run puppet on " <> pretty target))

  target@Target {_node = Just n} ->
    defCmd & pep .~ ( "pepper " <> n <> " -t 300 cicd.run_puppet zone=" <> target^.zone)
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

-- | Write one or more of [subgroup, role, hostgroup, zone] fact(s).
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
           ]

-- | Sync minion with the saltmaster
syncCmd :: Bool -> Target -> PepCmd
syncCmd across target@Target { _node = Nothing} =
  defCmd & pep .~ (pepperCompoundTarget across target <> " saltutil.sync_all")
syncCmd _ Target {_node = Just n} =
  defCmd & pep .~ ("pepper '" <> n <> "' saltutil.sync_all")

-- | Apply a configuration with state.
stateCmd :: Text -> Text -> PepCmd
stateCmd  cmd n =
  defCmd & pep .~ ("pepper " <> n <> " state.apply " <> cmd)
         & jq .~ "jq '.return[0]'"

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
factCmd :: FilePath -> Text -> Bool -> Down -> Target -> PepCmd
factCmd fpath _ across _ target@Target {_node = Nothing} =
  defCmd & pep .~ (pepperCompoundTarget across target <> " grains.item os osrelease fqdn fqdn_ip4 hostgroup subgroup role instance puppetmaster_timestamp puppetmaster_jenkins_job")
         & jq .~
           [r|
             jq '.return[] | .[] | { fqdn, ip: .fqdn_ip4[0], os:  "\(.os) \(.osrelease)", hostgroup, subgroup, role, instance, "puppet run": .puppetmaster_timestamp, "jenkins job" : .puppetmaster_jenkins_job}' \
           |] <> " | tee " <> toS fpath <> " | jq ."
factCmd _ pdbUrl _ (Down True) Target {_node = Just n} =
  defCmd & pep .~ ("pdbquery -t remote  -l " <> pdbUrl <> " facts " <> Text.toLower n)
         & jq .~
           [r|
             jq 'map({"key": .name, value}) |
             from_entries |
             {hostgroup, subgroup, role, "os": "\(.operatingsystem) \(.operatingsystemrelease)", "ip": .ipaddress, "puppet run": .puppetmaster_timestamp, "jenkins job" : .puppetmaster_jenkins_job}'
           |]
factCmd fpath _ _ (Down False) Target {_node = Just n} =
  defCmd & pep .~ ("pepper " <> n <> " grains.item os osrelease fqdn fqdn_ip4 hostgroup subgroup role instance puppetmaster_timestamp puppetmaster_jenkins_job")
         & jq .~
           [r|
              jq '.return[0] |
              .[] |
              { fqdn, ip: .fqdn_ip4[0], os:  "\(.os) \(.osrelease)", hostgroup, subgroup, role, instance, "puppet run": .puppetmaster_timestamp, "jenkins job" : .puppetmaster_jenkins_job }' \
           |] <> " | tee " <> toS fpath <> " | jq ."

pingCmd :: Bool -> Target -> PepCmd
pingCmd across target@Target {_node = Nothing} =
  defCmd & pep .~ ( pepperCompoundTarget across target <> " test.ping")
      & jq .~ "jq '.return[0]'"
pingCmd _ Target {_node = Just n} =
  defCmd & pep .~ ( "pepper '" <> n <> "' test.ping")
      & jq .~ "jq '.return[0]'"

-- | Display configuration (puppet) data.
dataCmd :: Bool -> Maybe Text -> Target -> PepCmd
dataCmd _ Nothing Target {_node= Just n} =
  defCmd & pep .~ ("pepper " <> n <> " pillar.items delimiter='/'")
         & jq .~ "jq '.return[0]'"
dataCmd True Nothing Target {_node= Nothing} = panic'
dataCmd False Nothing target@Target {_node= Nothing} =
  defCmd & pep .~ (pepperCompoundTarget False target  <> " pillar.items delimiter='/'")
         & jq .~ "jq '.return[0]'"
dataCmd across (Just key) target@Target {_node= Nothing} =
  defCmd & pep .~ "( " <> pepperCompoundTarget across target <> " grains.item fqdn subgroup role ; " <> pepperCompoundTarget across target <> " pillar.item " <> key <> " delimiter='/' )"
         & jq .~ "jq -s '.[0].return[0] * .[1].return[0]' | jq '.[] | { fqdn, subgroup, role, " <> key <> "}'"
dataCmd _ (Just key) Target {_node= Just n} =
  defCmd & pep .~ ("pepper " <> n <> " pillar.item " <> key <> " delimiter='/'")
         & jq .~ "jq '.return[0]'"

-- | Fetch the result of previous commands from the pgserver.
resultCmd :: Text -> Bool -> Maybe Text -> Maybe Natural -> Text -> PepCmd
resultCmd _ _ Nothing (Just 0) _ = panic "NUM should be > 0"
resultCmd pgUrl _ Nothing (Just num) user =
  defCmd & pep .~ "curl -f -s -H \"Range: 0-" <> show (num - 1) <> "\" \"" <> pgUrl <> "?user=eq." <> user <> "&order=jid.desc\""
         & jq .~ "jq -C '.'"
resultCmd pgUrl raw (Just jobid) Nothing _ =
  defCmd & pep .~ ("curl -s " <> (if raw then mempty else "-f -H \"Accept: application/vnd.pgrst.object+json\" ") <> "\"" <> pgUrl <> "?select=ret&jid=eq." <> jobid <> "\"" )
         & jq .~
           [r|
             jq -r '(.ret | .[] |
                     if .return.retcode and .return.retcode != 0
                     then "\u001B[1;31mFAILURE\u001B[0m for "
                     else "\u001B[1;32mSUCCESS\u001B[0m for "
                     end
                     + .id + ":",
                     if .return.stderr and .return.stderr != ""
                     then .return.stdout + "\n******\n" + .return.stderr + "\n"
                     elif .return.stdout and .return.stdout != ""
                     then .return.stdout + "\n"
                     elif .return | ([(.[] | type == "object" )] | all ) and map (has("changed"))
                     then .return | .[] | select(.changes.stdout) | .changes.stdout | tostring
                     else .return
                     end)'
           |]
         & cmdMode .~ RetryMode
resultCmd _ _ Nothing Nothing _ = panic'
resultCmd _ _ (Just _) (Just _) _ = panic'

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

validateCmd :: Target -> PepCmd
validateCmd Target {_node= Just n} =
  defCmd & pep .~ "pepper " <> n <> " --client=local_async state.apply validate.host"
validateCmd target@Target{_node= Nothing} =
  defCmd & pep .~ (pepperCompoundTarget False target  <> " --client=local_async state.apply validate.host")
