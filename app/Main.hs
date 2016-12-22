module Main where

import           Control.Lens.Operators hiding ((<.>))
import           Data.Maybe             (fromJust, fromMaybe)
import           Data.Optional          (Optional (..))
import qualified Data.Optional          as Optional
import qualified Data.Text              as Text
import qualified System.Process         as Process hiding (FilePath)
import           Turtle

import           Option
import           PepCmd
import           Type

-- need to come from http://hydra.nixos.org/job/nixpkgs/trunk/haskellPackages.language-puppet.x86_64-linux
-- 14 nov 2016
nixpkgs = "12a057cbe07a0ee30b28b4edb39c0a453a4d0556"
-- nixpkgs = "b02e6cc70d3e458eccb99ea20c4746238ebf52c2"

user :: Shell Text
user = do
  h <- home
  lineToText <$> input (h <> ".user_id")

saltUrl zone =
  case zone of
    "dev"     -> "https://salt.dev.srv.cirb.lan:8000"
    "testing" -> "https://saltmaster.sandbox.srv.cirb.lan:8000"
    "staging" -> "https://salt.sta.srv.cirb.lan:8000"
    "prod"    -> "https://salt.prd.srv.cirb.lan:8000"

pgUrl :: Text -> Text
pgUrl zone =
  let
    pgserver_prod     = "http://pgserver-cicd.prd.srv.cirb.lan/saltstack"
    pgserver_sandbox = "http://pgserver.sandbox.srv.cirb.lan/saltstack"
    result_suffix = "/salt_result"
  in
  case zone of
    "testing" -> pgserver_sandbox <> result_suffix
    "prod"    -> pgserver_prod <> result_suffix
    _         -> pgserver_prod <> "-" <> zone <> result_suffix

puppetdbUrl zone
  | zone == "testing" = "http://puppetdb.sandbox.srv.cirb.lan:8080"
  | otherwise         = "http://puppetdb.prd.srv.cirb.lan:8080"

configDir :: Shell Turtle.FilePath
configDir = (</> ".config/cicd") <$> home

nixFileName :: Text -> Text
nixFileName zone = zone <> ".nix"

userPwd = do
  let pwd_file = (</> ".user_pwd") <$> home
  lineToText <$> (input =<< pwd_file)

getStack :: Maybe Stack -> Shell Stack
getStack s = do
  h <- home
  ds <- input ( h </> ".user_stack")
  return $ fromMaybe (lineToText ds) s

writeConfig :: Turtle.FilePath -> Text -> Text -> IO ()
writeConfig file zone user = do
  let
    lines = [ "{user_pwd}:"
              , "(import ./.) {"
              , "  zone = \"" <> zone <> "\";"
              , "  salt-user = \"" <> user <> "\";"
              , "  salt-pass = \"${user_pwd}\";"
              , "  salt-url = \"" <> saltUrl  zone <> "\";"
              , "}"
              ]
  writeTextFile file (Text.unlines lines)

runCommand :: Text -> PepCmd -> Shell ExitCode
runCommand zone cmd =  do
  let
    nixcommand z = Text.unwords ["nix-shell", nixFileName z]
    msg = cmd ^. cmdmsg
    pgr pwd = nixcommand zone <>  " --argstr user_pwd " <> pwd <> " -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/" <> nixpkgs <> ".tar.gz"
    pepcmd pwd = if Text.null (cmd^.cmdpep) then pgr pwd else pgr pwd <> " --command '" <> cmd^.cmdpep <> "'"

    initEnv z u = do
      configdir <- configDir
      let configfile = configdir </> fromText (nixFileName z)
      found <- testfile configfile
      unless found $ do
        liftIO $ writeConfig configfile z u
        echo $ fpToLine configfile <> " created"

  salt_pass <- userPwd
  foundconfdir <- testdir =<< configDir
  unless foundconfdir $ mkdir =<< configDir
  pushd =<< configDir
  initEnv zone =<< user
  unless (null msg) $ confirm (fromJust msg)
  -- liftIO $ print (pepcmd salt_pass)
  case cmd^.cmdjq of
    Default -> interactive (pepcmd salt_pass)
    Specific jq -> do
      -- liftIO $ print jq
      inshell (pepcmd salt_pass) empty & shell jq


-- prohibited options
run (Options zone (Data (Nothing, Arg Nothing Nothing Nothing s)))  = die "Running data on the whole stack is currently prohibited"

-- valid options
run (Options zone Console)                       = runCommand zone consoleCmd
run (Options zone Stats)                         = runCommand zone statCmd
run (Options zone (Facts (across, Arg r n g s))) = getStack s >>= runCommand zone . factCmd (puppetdbUrl zone) r n g across
run (Options zone (Ping (Arg r n g s)))          = getStack s >>= runCommand zone . pingCmd r n g
run (Options zone (Runpuppet (Arg r n g s )))    = getStack s >>= runCommand zone . runpuppetCmd r n g
run (Options zone (Sync (Arg r n g s)))          = getStack s >>= runCommand zone . syncCmd r n g
run (Options zone (Data (key, Arg r n g s)))     = getStack s >>= runCommand zone . dataCmd key r n g
run (Options zone (Orchestrate (cmd, s)))        = getStack s >>= runCommand zone . orchCmd cmd
run (Options zone (Du (Arg r n g s)))            = getStack s >>= runCommand zone . duCmd r n g
run (Options zone (Result (ResultNum n)))        = user >>= runCommand zone . resultCmd (pgUrl zone) Nothing (Just n)
run (Options zone (Result (ResultJob j )))       = user >>= runCommand zone . resultCmd (pgUrl zone) (Just j) Nothing


main :: IO ()
main = sh $
  options "CICD command line utility" parser >>= run

confirm msg = do
  echo $ msg <> "? (Y/N)"
  r <- readline
  case r of
    Just "Y" -> return ()
    _        -> die "Abort by the user"


fpToLine :: Turtle.FilePath -> Line
fpToLine = unsafeTextToLine . format fp

interactive :: MonadIO io => Text -> io ExitCode
interactive c = do
    let
      cp = (Process.shell (Text.unpack c))
            { Process.std_in  = Process.Inherit
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            , Process.delegate_ctlc = True
            }
    (_, _, _, ph) <- liftIO $ Process.createProcess cp
    liftIO $ Process.waitForProcess ph
