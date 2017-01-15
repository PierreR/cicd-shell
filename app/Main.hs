module Main where

import           Control.Lens.Operators hiding ((<.>))
import           Data.Foldable          (for_)
import           Data.Maybe             (fromMaybe)
import           Data.Optional          (Optional (..))
import qualified Data.Optional          as Optional
import qualified Data.Text              as Text
import qualified System.Process         as Process hiding (FilePath)
import           Turtle
import qualified Data.Version (showVersion)
import qualified Paths_cicd_shell

import           Option
import           PepCmd
import           Type


version = Data.Version.showVersion Paths_cicd_shell.version

nixpkgs :: Shell Text
nixpkgs = do
  h <- home
  lineToText <$> input (h <> ".nixpkgs_ref")

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

-- | configDir sits in user space
configDir :: Shell Turtle.FilePath
configDir = (</> ".local/share/cicd") <$> home

nixFileName :: Text -> Text
nixFileName zone = zone <> "-" <> Text.pack version <> ".nix"
-- we could locate the default.nix relatively to bin where the exec sits
-- for now we simply find it in the nixpkgs custom user folder
defaultNixFilePath = do
  h <- home
  pure $ h </> ".nixpkgs/pkgs/cicd-shell/share"

userPwd = do
  let pwd_file = (</> ".user_pwd") <$> home
  lineToText <$> (input =<< pwd_file)

getStack :: Maybe Stack -> Shell Stack
getStack s = do
  h <- home
  ds <- input ( h </> ".user_stack")
  return $ fromMaybe (lineToText ds) s

-- sensitive information such as a password won't be copy in the configfile
writeConfig :: Turtle.FilePath -> Text -> Text -> IO ()
writeConfig file zone user = do
  let
    lines dnfp = [ "{user_pwd}:"
                , "(import "<> dnfp <> "/.) {"
                , "  zone = \"" <> zone <> "\";"
                , "  salt-user = \"" <> user <> "\";"
                , "  salt-pass = \"${user_pwd}\";"
                , "  salt-url = \"" <> saltUrl  zone <> "\";"
                , "}"
                ]
  dnfp <- defaultNixFilePath
  writeTextFile file (Text.unlines (lines (format fp dnfp)))

runCommand :: Text -> PepCmd -> Shell ExitCode
runCommand zone cmd =  do
  let
    nixcommand z = Text.unwords ["nix-shell", nixFileName z]
    msg = cmd ^. cmdmsg
    pgr pwd nixref = nixcommand zone <>  " --argstr user_pwd " <> pwd <> " -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/" <> nixref <> ".tar.gz"
    pepcmd pwd nixref = if Text.null (cmd^.cmdpep) then pgr pwd nixref else pgr pwd nixref <> " --command '" <> cmd^.cmdpep <> "'"

    initEnv z u = do
      configdir <- configDir
      let configfile = configdir </> fromText (nixFileName z)
      found <- testfile configfile
      unless found $ do
        liftIO $ writeConfig configfile z u
        printf (fp%" created\n") configfile
        void $ shell ("cicd " <> zone <> " gentags") empty

  salt_pass <- userPwd
  nixpkgsref <- nixpkgs
  foundconfdir <- testdir =<< configDir
  unless foundconfdir $ mkdir =<< configDir
  pushd =<< configDir
  initEnv zone =<< user
  for_ msg confirm
  -- liftIO $ print (pepcmd salt_pass nixpkgsref)
  case cmd^.cmdjq of
    Default -> interactive (pepcmd salt_pass nixpkgsref)
    Specific jq -> do
      -- liftIO $ print jq
      inshell (pepcmd salt_pass nixpkgsref) empty & shell jq


-- prohibited options
run (Options zone (Data (Nothing, Arg Nothing Nothing Nothing s)))  = die "Running data on the whole stack is currently prohibited"

-- valid options
run (Options zone Console)                       = runCommand zone consoleCmd
run (Options zone Stats)                         = runCommand zone statCmd
run (Options zone GenTags)                       = configDir >>= runCommand zone . genTagsCmd zone
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
main = sh $ options (fromString ("CICD - command line utility (v" <> version <> ")")) parser >>= run

confirm msg = do
  echo $ msg <> "? (Y/N)"
  r <- readline
  case r of
    Just "Y" -> return ()
    _        -> die "Abort by the user"


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
