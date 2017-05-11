{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Shell.PepCmd where

import qualified Data.Text         as Text
import           Text.RawString.QQ
import qualified Turtle
import           Turtle.Format

import           Shell.Prelude
import           Shell.Type

panic' = panic "The impossible happened. The option parser should void the case"

pepperCompoundTarget :: Bool -> Target -> Text
pepperCompoundTarget across t
  = "pepper -C \"" <> compound_target (t^.zone)
                                      (if across then Nothing else Just $ t^.stack)
                                      (t^.subgroup)
                                      (t^.role)
                   <> "\" "
  where
    compound_target z s g r =
      let join_target = Text.intercalate " and " . catMaybes
          role_target [g', r'] = "G@subgroup:" <> g' <> " and G@role:" <> r'
          role_target [r'] = "G@role:" <> r'
          role_target _ = panic "Role should follow the pattern 'subgroup.role' where subgroup is optional" -- TODO replace this with an exceptT
      in join_target
           [ Just ("G@zone:" <> z)
           , ("G@hostgroup:" <>) <$> s
           , ("G@subgroup:" <>) <$> g
           , (role_target . Text.splitOn ".") <$> r
           ]

data PepCmd
  = PepCmd
  { _pep :: Text
  , _jq  :: Optional Text
  , _beforeMsg :: Maybe CmdMsg -- ^ A message to be displayed before launching the command
  } deriving Show

data CmdMsg =
  CmdMsg Bool Text -- True for interactive message
  deriving Show

makeLenses ''PepCmd

consoleCmd :: Zone -> FilePath -> PepCmd
consoleCmd (Zone zone) datadir =
  let
    completion_cmd = format ("source "%w%"/share/completion.sh "%s%"; return") datadir zone
  in
  PepCmd completion_cmd empty empty

genTagsCmd :: Zone -> Turtle.FilePath -> PepCmd
genTagsCmd (Zone zone) cfdir =
  let nodefile = format fp cfdir <> "/.nodes-" <> zone
  in PepCmd
  "pepper \"*\" test.ping"
  ("jq '.return[0]' | jq keys | jq -r 'join (\" \")' > " <> pure nodefile)
  (Just $ CmdMsg False ("Generating " <> nodefile))

genSaltModListCmd :: Text -> PepCmd
genSaltModListCmd fpath =
  let jsonfile = fpath <> ".json"
  in
    PepCmd
    "pepper --client=runner doc.execution"
    ("jq '.return[0] | keys' | tee " <> pure jsonfile <> " | jq -r 'join (\" \")' > " <> pure fpath)
    empty

genSaltModjsonCmd :: Text -> PepCmd
genSaltModjsonCmd fpath = PepCmd
  "pepper --client=runner doc.execution"
  ("jq '.return[0]' > " <> pure (fpath <> ".json"))
  empty

statCmd :: PepCmd
statCmd = PepCmd
  "pepper --client=runner manage.status"
  [r|
     jq '.return[0] | [.up , .up + .down | length] as $stats | {up, down, stats: "\($stats[0]) up of \($stats[1])"}'
  |]
  empty

orchCmd :: Text -> Text -> PepCmd
orchCmd cmd stack = PepCmd
  ("pepper state.orchestrate --client=runner mods=orch." <> cmd <> " saltenv=" <> stack)
  empty
  empty

runpuppetCmd :: Target -> PepCmd
runpuppetCmd target@Target {_node = Nothing} = PepCmd
  ( pepperCompoundTarget False target <> "--client=local_async puppetutils.run_agent zone=" <> target^.zone <> " hostgroup=" <> target^.stack)
  "jq '.return'"
  (Just $ CmdMsg True ("Run puppet on " <> Text.intercalate "." (catMaybes [target^.role, target^.subgroup] <> [target^.stack, target^.zone])))
runpuppetCmd target@Target {_node = Just n} = PepCmd
  ( "pepper " <> n <> " -t 240 puppetutils.run_agent zone=" <> target^.zone <> " hostgroup=" <> target^.stack)
  [r|
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
  empty

setfactsCmd :: SetfactArg -> PepCmd
setfactsCmd SetfactArg {..} = PepCmd
  ("pepper '" <> _node <> "' cicd.set_facts " <> join_facts)
  empty
  empty

  where
    join_facts =
      let join_values = Text.intercalate " " . catMaybes
      in join_values
           [ ("subgroup=" <>) <$> _subgroup
           , ("role=" <>) <$> _role
           , ("hostgroup=" <>) <$> _hostgroup
           , ("zone=" <>) <$> _zone
           ]

syncCmd :: Bool -> Target -> PepCmd
syncCmd across target@Target { _node = Nothing} = PepCmd
  (pepperCompoundTarget across target <> "saltutil.sync_all")
  empty
  empty
syncCmd _ Target {_node = Just n} = PepCmd
  ("pepper '" <> n <> "' saltutil.sync_all")
  empty
  empty

duCmd :: Target -> PepCmd
duCmd  Target {_node = Just n} = PepCmd
  ("pepper " <> n <> " disk.percent")
  "jq '.return[0]'"
  empty
duCmd target@Target {_node = Nothing} = PepCmd
  (pepperCompoundTarget False target <> " disk.percent")
  "jq '.return[0]'"
  empty

serviceCmd :: ServiceAction -> ServiceName -> Target -> PepCmd
serviceCmd ServiceStatus (ServiceName name) target@Target {_node = Nothing} = PepCmd
  (pepperCompoundTarget False target <> " service.status " <> name)
  "jq '.return[0]'"
  empty
serviceCmd ServiceStatus (ServiceName name) Target {_node = Just n} = PepCmd
  ("pepper " <> n <> " service.status " <> name)
  "jq '.return[0]'"
  empty
serviceCmd ServiceRestart (ServiceName name) Target {_node = Just n} = PepCmd
  ("pepper " <> n <> " service.restart " <> name)
  "jq '.return[0]'"
  empty
serviceCmd ServiceRestart _ Target {_node = Nothing} =
  panic ("To restart a service, you need to specify a node with -n")

factCmd :: Text -> Bool -> Bool -> Target -> PepCmd
factCmd _ across _ target@Target {_node = Nothing} = PepCmd
  (pepperCompoundTarget across target <> "grains.item os osrelease fqdn fqdn_ip4 hostgroup subgroup role puppetmaster_timestamp puppetmaster_jenkins_job")
  [r|
     jq '.return[] | .[] | { fqdn, ip: .fqdn_ip4[0], os:  "\(.os) \(.osrelease)", hostgroup, subgroup, role, "puppet run": .puppetmaster_timestamp, "jenkins job" : .puppetmaster_jenkins_job}'
  |]
  empty
factCmd pdbUrl _ True Target {_node = Just n} = PepCmd
  ("pdbquery -t remote  -l " <> pdbUrl <> " facts " <> Text.toLower n)
  [r|
    jq 'map({"key": .name, value}) | from_entries | {hostgroup, subgroup, role, "os": "\(.operatingsystem) \(.operatingsystemrelease)", "ip": .ipaddress, "puppet run": .puppetmaster_timestamp, "jenkins job" : .puppetmaster_jenkins_job}'
  |]
  empty
factCmd _ _ False Target {_node = Just n} = PepCmd
  ("pepper " <> n <> " grains.item os osrelease fqdn fqdn_ip4 hostgroup subgroup role puppetmaster_timestamp puppetmaster_jenkins_job")
  [r|
     jq '.return[0] | .[] | { fqdn, ip: .fqdn_ip4[0], os:  "\(.os) \(.osrelease)", hostgroup, subgroup, role, "puppet run": .puppetmaster_timestamp, "jenkins job" : .puppetmaster_jenkins_job }'
  |]
  empty

pingCmd :: Bool -> Target -> PepCmd
pingCmd across target@Target {_node = Nothing} = PepCmd
  ( pepperCompoundTarget across target <> "test.ping")
  "jq '.return[0]'"
  empty
pingCmd _ Target {_node = Just n} = PepCmd
  ( "pepper '" <> n <> "' test.ping")
  "jq '.return[0]'"
  empty

dataCmd :: Bool -> Maybe Text -> Target -> PepCmd
dataCmd _ Nothing Target {_node= Just n} = PepCmd
  ("pepper " <> n <> " pillar.items delimiter='/'")
  "jq '.return[0]'"
  empty
dataCmd True Nothing Target {_node= Nothing} = panic'
dataCmd False Nothing target@Target {_node= Nothing} = PepCmd
  (pepperCompoundTarget False target  <> " pillar.items delimiter='/'")
  "jq '.return[0]'"
  empty
dataCmd across (Just key) target@Target {_node= Nothing}
  = let pep = "( " <> pepperCompoundTarget across target <> "grains.item fqdn subgroup role ; " <> pepperCompoundTarget across target <> "pillar.item " <> key <> " delimiter='/' )"
        jq = "jq -s '.[0].return[0] * .[1].return[0]' | jq '.[] | { fqdn, subgroup, role, "<> pure key <> "}'"
    in
      PepCmd pep jq empty
dataCmd _ (Just key) Target {_node= Just n} = PepCmd
  ("pepper " <> n <> " pillar.item " <> key <> " delimiter='/'")
  ("jq '.return[0]'")
  empty

resultCmd :: Text -> Raw -> Maybe Text -> Maybe Natural -> Text -> PepCmd
resultCmd _ _ Nothing (Just 0) _ = panic "NUM should be > 0"
resultCmd pgUrl _ Nothing (Just num) user = PepCmd
  (format ("curl -f -s -H \"Range: 0-"%d%"\" \""%s%"?user=eq."%s%"&order=jid.desc\"") (num - 1) pgUrl user)
  "jq -C '.'"
  empty
resultCmd pgUrl (Raw raw) (Just jobid) Nothing _ = PepCmd
  ("curl -s " <> (if raw then mempty else "-f -H \"Accept: application/vnd.pgrst.object+json\" ") <> "\"" <> pgUrl <> "?select=ret&jid=eq." <> jobid <> "\"" )
  [r|
    jq -r '(.ret | if .return.retcode == 0 then "\u001B[1;32mSUCCESS\u001B[0m for " else "\u001B[1;31mFAILURE\u001B[0m for " end + .id + ":", if .return.stderr != "" then .return.stdout + "\n******\n" + .return.stderr + "\n" else .return.stdout + "\n" end)'
  |]
  empty
resultCmd _ _ Nothing Nothing _ = panic'
resultCmd _ _ (Just _) (Just _) _ = panic'
