{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module PepCmd where

import           Control.Lens
import           Data.Maybe        (maybe, catMaybes)
import           Data.Optional     (Optional)
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Text.RawString.QQ
import           Turtle
import           Type

pepperCompoundTarget s r g = "pepper -C \"" <> target s r g <>"\" "

target :: Maybe Stack -> Maybe Subgroup -> Maybe Role -> Text
target stack subgroup role =
  let
    join_ = Text.intercalate " and " . catMaybes
    stack_target = fmap ("G@hostgroup:" <>)
    subgroup_target = fmap ("G@subgroup:" <>)
  in
  join_ [stack_target stack, subgroup_target subgroup, split_role <$> role]
  where
    split_role :: Text -> Text
    split_role r =
      let rx = Text.splitOn "." r
      in case rx of
        ([pre, post]) -> "G@subgroup:" <> pre <> " and G@role:" <> post
        ([pre])       -> "G@role:" <> pre

data PepCmd
  = PepCmd
  { _cmdpep :: Text
  , _cmdjq  :: Optional Text
  , _cmdmsg :: Maybe Line
  } deriving Show

makeLenses ''PepCmd

consoleCmd :: PepCmd
consoleCmd = PepCmd Text.empty empty empty

genTagsCmd :: Text -> Turtle.FilePath -> PepCmd
genTagsCmd zone cfdir =
  PepCmd
  "pepper \"*\" test.ping"
  ("jq '.return[0]' | jq keys | jq -r 'join (\" \")' > " <> pure (format fp cfdir) <> "/.nodes-" <> pure zone)
  empty

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

runpuppetCmd :: Maybe Role -> Maybe Node -> Maybe Subgroup -> Stack -> PepCmd
runpuppetCmd role Nothing subgroup stack = PepCmd
  ( pepperCompoundTarget (Just stack) subgroup role <> "--client=local_async puppetutils.run_agent")
  "jq '.return'"
  ( case role of
      Nothing -> textToLine $ "Run puppet on " <> stack
      Just r  -> textToLine $ "Run puppet on " <> stack <> "." <> r
  )
runpuppetCmd _ (Just node) _ _ = PepCmd
  ( "pepper " <> node <> " puppetutils.run_agent")
  [r|
    jq -r '.return[] | to_entries | (.[] | if .value.retcode == 0 then "\nSUCCESS for " else "\nFAILURE for " end + .key + ":" , if .value.stderr != "" then .value.stdout + "\n******\n" + .value.stderr else .value.stdout end)'
  |]
  empty

syncCmd :: Maybe Role -> Maybe Node -> Maybe Subgroup -> Stack -> PepCmd
syncCmd role Nothing subgroup stack = PepCmd
  (pepperCompoundTarget (Just stack) subgroup role <> "saltutil.sync_all")
  empty
  empty
syncCmd _ (Just node) _ _ = PepCmd
  ("pepper '" <> node <> "' saltutil.sync_all")
  empty
  empty

duCmd :: Maybe Role -> Maybe Node -> Maybe Subgroup -> Stack -> PepCmd
duCmd  _ (Just n) _ _ = PepCmd
  ("pepper " <> n <> " disk.percent")
  "jq '.return[0]'"
  empty
duCmd  role Nothing subgroup stack = PepCmd
  (pepperCompoundTarget (Just stack) subgroup role <> " disk.percent")
  "jq '.return[0]'"
  empty

factCmd :: Text -> Maybe Role -> Maybe Node -> Maybe Subgroup -> Bool -> Stack -> PepCmd
factCmd _ role Nothing subgroup across stack = PepCmd
  (pepperCompoundTarget (if across then Nothing else Just stack) subgroup role <> "grains.item os osrelease fqdn fqdn_ip4 hostgroup subgroup role")
  [r|
     jq '.return[] | .[] | { fqdn, ip: .fqdn_ip4[], os:  "\(.os) \(.osrelease)", hostgroup, subgroup, role}'
  |]
  empty

factCmd pdbUrl _ (Just node) _ _ _ = PepCmd
  ("pdbquery -t remote  -l " <> pdbUrl <> " facts " <> node)
  [r|
    jq 'map({"key": .name, value}) | from_entries | {hostgroup, subgroup, role, "os": "\(.operatingsystem) \(.operatingsystemrelease)", "ip": .ipaddress_eth0, "puppet run": .puppetmaster_timestamp, "jenkins job" : .puppetmaster_jenkins_job}'
  |]
  empty

pingCmd :: Maybe Role -> Maybe Node -> Maybe Subgroup -> Stack -> PepCmd
pingCmd role Nothing subgroup stack = PepCmd
  ( pepperCompoundTarget (Just stack) subgroup role <> "test.ping")
  "jq '.return[0]'"
  empty
pingCmd _ (Just node) _ _  = PepCmd
  ( "pepper '" <> node <> "' test.ping")
  "jq '.return[0]'"
  empty

dataCmd :: Maybe Key -> Maybe Role -> Maybe Node -> Maybe Subgroup -> Stack -> PepCmd
dataCmd Nothing _ (Just node) _ _ = PepCmd
  ("pepper " <> node <> " pillar.items delimiter='/'")
  "jq '.return[0]'"
  empty
dataCmd Nothing role Nothing subgroup stack = PepCmd
  (pepperCompoundTarget (Just stack) subgroup role  <> " pillar.items delimiter='/'")
  "jq '.return[0]'"
  empty
-- TODO: use pdbquery instead
dataCmd (Just key) _ (Just node) _ _ = PepCmd
  ("pepper " <> node <> " pillar.item "<> key <> " delimiter='/'")
  "jq '.return[0]'"
  empty

dataCmd (Just key) role _ subgroup stack
  = let pep = "( " <> pepperCompoundTarget (Just stack) subgroup role <> "grains.item fqdn subgroup role ; " <> pepperCompoundTarget (Just stack) subgroup role <> "pillar.item " <> key <> " delimiter='/' )"
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
