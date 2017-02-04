{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module PepCmd where

import           Control.Lens
import           Data.Maybe        (maybe, catMaybes, fromMaybe)
import           Data.Optional     (Optional)
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Text.RawString.QQ
import           Turtle
import           Type

pepperCompoundTarget :: Bool -> Text -> Stack -> Maybe Subgroup -> Maybe Role -> Text
pepperCompoundTarget across zone stack role subgroup = "pepper -C \"" <> target zone (if across then Nothing else Just stack) role subgroup <>"\" "
  where
    target :: Text -> Maybe Stack -> Maybe Subgroup -> Maybe Role -> Text
    target z s g r =
      let join_target = Text.intercalate " and " . catMaybes
          role_target [subgroup, role] = "G@subgroup:" <> subgroup <> " and G@role:" <> role
          role_target [role] = "G@role:" <> role
          role_target _ = error "Role should follow the pattern 'subgroup.role' where subgroup is optional" -- TODO replace this with an exceptT
      in join_target
           [ Just ("G@zone:" <> z)
           , ("G@hostgroup:" <>) <$> s
           , ("G@subgroup:" <>) <$> g
           , (role_target . Text.splitOn ".") <$> r
           ]

data PepCmd
  = PepCmd
  { _cmdpep :: Text
  , _cmdjq  :: Optional Text
  , _cmdmsg :: Maybe CmdMsg -- ^ A message to be displayed before launching the command
  } deriving Show


data CmdMsg =
  CmdMsg Bool Text -- True for interactive message
  deriving Show

makeLenses ''PepCmd

consoleCmd :: PepCmd
consoleCmd = PepCmd Text.empty empty empty

genTagsCmd :: Text -> Turtle.FilePath -> PepCmd
genTagsCmd zone cfdir =
  let nodefile = format fp cfdir <> "/.nodes-" <> zone
  in PepCmd
  "pepper \"*\" test.ping"
  ("jq '.return[0]' | jq keys | jq -r 'join (\" \")' > " <> pure nodefile)
  (Just $ CmdMsg False ("Generating " <> nodefile))

statCmd :: PepCmd
statCmd = PepCmd
  "pepper --client=runner manage.status"
  [r|
     jq '.return[0] | [.up , .up + .down | length] as $stats | {up, down, stats: "\($stats[0]) up of \($stats[1])"}'
  |]
  empty

orchCmd :: Cmd -> Stack -> PepCmd
orchCmd cmd stack = PepCmd
  ("pepper state.orchestrate --client=runner mods=orch." <> cmd <> " saltenv=" <> stack)
  empty
  empty

runpuppetCmd :: Text -> Maybe Role -> Maybe Node -> Maybe Subgroup -> Stack -> PepCmd
runpuppetCmd zone role Nothing subgroup stack = PepCmd
  ( pepperCompoundTarget False zone stack subgroup role <> "--client=local_async puppetutils.run_agent")
  "jq '.return'"
  ( case role of
      Nothing -> Just $ CmdMsg True ("Run puppet on " <> stack)
      Just r  -> Just $ CmdMsg True ("Run puppet on " <> (Text.intercalate "." [r, stack, zone]))
  )
runpuppetCmd _ _ (Just node) _ _ = PepCmd
  ( "pepper " <> node <> " puppetutils.run_agent")
  [r|
    jq -r '.return[] | to_entries | (.[] | if .value.retcode == 0 then "\nSUCCESS for " else "\nFAILURE for " end + .key + ":" , if .value.stderr != "" then .value.stdout + "\n******\n" + .value.stderr else .value.stdout end)'
  |]
  empty

syncCmd :: Text -> Maybe Role -> Maybe Node -> Maybe Subgroup -> Bool -> Stack -> PepCmd
syncCmd zone role Nothing subgroup across stack = PepCmd
  (pepperCompoundTarget across zone stack subgroup role <> "saltutil.sync_all")
  empty
  empty
syncCmd _ _ (Just node) _ _ _ = PepCmd
  ("pepper '" <> node <> "' saltutil.sync_all")
  empty
  empty

duCmd :: Text ->  Maybe Role -> Maybe Node -> Maybe Subgroup -> Stack -> PepCmd
duCmd  _ _ (Just n) _ _ = PepCmd
  ("pepper " <> n <> " disk.percent")
  "jq '.return[0]'"
  empty
duCmd  zone role Nothing subgroup stack = PepCmd
  (pepperCompoundTarget False zone stack subgroup role <> " disk.percent")
  "jq '.return[0]'"
  empty

serviceCmd :: ServiceAction -> Text -> Zone -> Maybe Role -> Maybe Node -> Maybe Subgroup -> Stack -> PepCmd
serviceCmd ServiceStatus name zone role Nothing subgroup stack = PepCmd
  (pepperCompoundTarget False zone stack subgroup role <> " service.status " <> name)
  "jq '.return[0]'"
  empty
serviceCmd ServiceStatus name _ _ (Just n) _ _ = PepCmd
  ("pepper " <> n <> " service.status " <> name)
  "jq '.return[0]'"
  empty

serviceCmd ServiceReload name _ _ (Just n) _ _ = PepCmd
  ("pepper " <> n <> " service.status " <> name)
  "jq '.return[0]'"
  empty

factCmd :: Text -> Zone -> Maybe Role -> Maybe Node -> Maybe Subgroup -> Bool -> Bool -> Stack -> PepCmd
factCmd _ zone role Nothing subgroup across _ stack = PepCmd
  (pepperCompoundTarget across zone stack subgroup role <> "grains.item os osrelease fqdn fqdn_ip4 hostgroup subgroup role puppetmaster_timestamp puppetmaster_jenkins_job")
  [r|
     jq '.return[] | .[] | { fqdn, ip: .fqdn_ip4[0], os:  "\(.os) \(.osrelease)", hostgroup, subgroup, role, "puppet run": .puppetmaster_timestamp, "jenkins job" : .puppetmaster_jenkins_job}'
  |]
  empty
factCmd pdbUrl _ _ (Just node) _ _ True _ = PepCmd
  ("pdbquery -t remote  -l " <> pdbUrl <> " facts " <> node)
  [r|
    jq 'map({"key": .name, value}) | from_entries | {hostgroup, subgroup, role, "os": "\(.operatingsystem) \(.operatingsystemrelease)", "ip": .ipaddress_eth0, "puppet run": .puppetmaster_timestamp, "jenkins job" : .puppetmaster_jenkins_job}'
  |]
  empty
factCmd pdbUrl _ _ (Just node) _ _ False _ = PepCmd
  ("pepper " <> node <> " grains.item os osrelease fqdn fqdn_ip4 hostgroup subgroup role puppetmaster_timestamp puppetmaster_jenkins_job")
  [r|
     jq '.return[0] | .[] | { fqdn, ip: .fqdn_ip4[0], os:  "\(.os) \(.osrelease)", hostgroup, subgroup, role, "puppet run": .puppetmaster_timestamp, "jenkins job" : .puppetmaster_jenkins_job }'
  |]
  empty

pingCmd :: Text -> Maybe Role -> Maybe Node -> Maybe Subgroup -> Bool -> Stack -> PepCmd
pingCmd zone role Nothing subgroup across stack = PepCmd
  ( pepperCompoundTarget across zone stack subgroup role <> "test.ping")
  "jq '.return[0]'"
  empty
pingCmd _ _ (Just node) _ _ _ = PepCmd
  ( "pepper '" <> node <> "' test.ping")
  "jq '.return[0]'"
  empty

dataCmd :: Maybe Key -> Text -> Maybe Role -> Maybe Node -> Maybe Subgroup -> Stack -> PepCmd
dataCmd Nothing _ _ (Just node) _ _ = PepCmd
  ("pepper " <> node <> " pillar.items delimiter='/'")
  "jq '.return[0]'"
  empty
dataCmd Nothing zone role Nothing subgroup stack = PepCmd
  (pepperCompoundTarget False zone stack subgroup role  <> " pillar.items delimiter='/'")
  "jq '.return[0]'"
  empty
-- TODO: use pdbquery instead
dataCmd (Just key) _ _ (Just node) _ _ = PepCmd
  ("pepper " <> node <> " pillar.item "<> key <> " delimiter='/'")
  "jq '.return[0]'"
  empty

dataCmd (Just key) zone role _ subgroup stack
  = let pep = "( " <> pepperCompoundTarget False zone stack subgroup role <> "grains.item fqdn subgroup role ; " <> pepperCompoundTarget False zone stack subgroup role <> "pillar.item " <> key <> " delimiter='/' )"
        jq = "jq -s '.[0].return[0] * .[1].return[0]' | jq '.[] | { fqdn, subgroup, role, "<> pure key <> "}'"
    in
      PepCmd pep jq empty

resultCmd :: Text -> Maybe Jobid -> Maybe Int -> User -> PepCmd
resultCmd pgUrl Nothing (Just num) user = PepCmd
  (format ("curl -f -s -H \"Range: 0-"%d%"\" \""%s%"?user=eq."%s%"&order=jid.desc\"") num pgUrl user)
  "jq -C '.'"
  empty
resultCmd pgUrl (Just jobid) Nothing _ = PepCmd
   ("curl -f -s \"" <> pgUrl <> "?select=ret&jid=eq." <> jobid <> "\"" )
  [r|
    jq -r '(.[].ret | if .return.retcode == 0 then "SUCCESS for " else "FAILURE for " end + .id + ":", if .return.stderr != "" then .return.stdout + "\n******\n" + .return.stderr + "\n" else .return.stdout + "\n" end)'
  |]
  empty
