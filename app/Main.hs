{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Data.Text        as Text
import qualified Data.Text.IO     as Text
import qualified Data.Text.Lazy   as Text.Lazy
import qualified Data.Version     (showVersion)
import qualified Dhall
import qualified Paths_cicd_shell
import qualified System.Process   as Process
import           Turtle           hiding (FilePath, strict, view)
import qualified Turtle

import           Shell.Cli
import           Shell.PepCmd
import           Shell.Prelude
import           Shell.Type

version = Data.Version.showVersion Paths_cicd_shell.version

-- ROOT_DIR on the host
-- TODO: remove devbox/vagrant deps
configFilePath = "/vagrant/config/shell"

data ShellConfig
  = ShellConfig
  { _loginId      :: LText
  , _password     :: LText
  , _defaultStack :: LText
  } deriving (Generic, Show)

makeLenses ''ShellConfig

instance Dhall.Interpret ShellConfig

shellConfig :: MonadIO m => m ShellConfig
shellConfig = do
  liftIO $ Dhall.input auto (fromStrict configFilePath)
  where
    auto ::  Dhall.Interpret a => Dhall.Type a
    auto = Dhall.autoWith
      ( Dhall.defaultInterpretOptions { Dhall.fieldModifier = Text.Lazy.dropWhile (== '_') })

userId :: MonadIO io => io Text
userId = do
  user_id <- view (loginId.strict) <$> shellConfig
  if Text.null user_id
    then die  ("Your loginId is empty. Have you filled in " <> configFilePath <> " ?")
    else pure user_id

userPwd :: MonadIO io => io Text
userPwd = do
  password <- view (password.strict) <$> shellConfig
  if Text.null password
    then die  ("Your password is empty. Have you filled in " <> configFilePath <> " ?")
    else pure password

getStack :: MonadIO io => Maybe Text -> io Text
getStack s = do
  def <- view (defaultStack.strict) <$> shellConfig
  if Text.null def
    then die  ("The default stack is empty. Have you filled in " <> configFilePath <> " ?")
    else pure $ fromMaybe def s

getTarget :: Zone -> Arg -> Shell Target
getTarget (Zone _zone) Arg{..} = do
  _stack <- getStack _stack
  pure $ Target{..}

pgUrl :: Zone -> Text
pgUrl (Zone zone) =
  let
    pgserver_prod     = "http://pgserver-cicd.prd.srv.cirb.lan/saltstack"
    pgserver_sandbox = "http://pgserver.sandbox.srv.cirb.lan/saltstack"
    result_suffix = "/salt_result"
  in
  case zone of
    "sandbox" -> pgserver_sandbox <> result_suffix
    "prod"    -> pgserver_prod <> result_suffix
    _         -> pgserver_prod <> "_" <> zone <> result_suffix

puppetdbUrl (Zone zone)
  | zone == "sandbox" = "http://puppetdb.sandbox.srv.cirb.lan:8080"
  | otherwise         = "http://puppetdb.prd.srv.cirb.lan:8080"

-- | localDir sits in user space
-- It is the location for the gentags generated files that help with completion
localDir :: Shell Turtle.FilePath
localDir = (</> ".local/share/cicd") <$> home

dataDir:: Shell FilePath
dataDir =
  liftIO Paths_cicd_shell.getDataDir

nixShellCmd :: Zone -> Text -> Shell Text
nixShellCmd (Zone zone) pep = do
  userid <- userId
  userpwd <- userPwd
  datadir <- dataDir
  let pgr = format ("nix-shell "%w%"/share/"%s%".nix --argstr user_id "%s%" --argstr user_pwd "%s) datadir zone userid userpwd
  if Text.null pep
    then pure pgr
    else pure $ pgr <> " --command '" <> pep <> "'"

initTags :: Zone -> Shell ()
initTags z@(Zone zone) = do
  localdir <- localDir
  let tagfile = localdir </> fromText (".nodes-" <> zone)
  found <- testfile tagfile
  unless found $ do
    mktree localdir
    touch tagfile -- avoid the infine loop ...
    runCommand z (Verbose False) (Raw False) (genTagsCmd z localdir) >>= \case
      ExitSuccess -> printf ("`cicd "%s% " gentags` completed successfully.\n") zone
      ExitFailure _ -> printf "WARNING: cannot generate node completion file.\n"

initHelp :: Shell ()
initHelp = do
  gen_help genSaltModListCmd "modlist"
  gen_help genSaltModjsonCmd "modhelp"
  where
    gen_help cmd tag = do
      localdir <- localDir
      let fpath = localdir </> fromText ("." <> tag)
      found <- testfile fpath
      unless found $ do
        touch fpath -- avoid the infine loop ...
        runCommand (Zone "dev") (Verbose False) (Raw False) (cmd (format fp fpath)) >>= \case
          ExitSuccess -> printf (fp%" generated successfully.\n") fpath
          ExitFailure _ -> printf ("WARNING: cannot generate '"%fp%"' (completion).\n") fpath

runCommand :: Zone -> Verbose -> Raw -> PepCmd -> Shell ExitCode
runCommand z (Verbose verbose) (Raw raw) cmd =  do
  void $ shell "ping -c1 stash.cirb.lan > /dev/null 2>&1" empty .||. die "cannot connect to stash.cirb.lan, check your connection"
  initTags z
  initHelp
  maybe (pure ()) interactWith (cmd ^. beforeMsg)
  nixshell <- nixShellCmd z (cmd^.pep)
  if verbose then putText (cmd^.pep) else pure()
  case cmd^.jq of
    Default -> interactive nixshell
    Specific jq -> do
      -- liftIO $ print jq
      e <- loopN 10 $ do
        o0 <- shellStrictWithErr nixshell empty
        case o0 of
          (ExitFailure _, _, stderr) ->
            if Text.null stderr
            then do
              echo "Failing to get a response from the server. Retrying in 15 sec. Press Ctrl-C to abort."
              liftIO $ threadDelay (15 * 1000 * 1000)
              continue
            else do
              printf ("Failing to execute:\n"%s%"\n"%s) (cmd^.pep) stderr
              break
          (ExitSuccess, stdout, _) -> do
            let stdout' = select (textToLines stdout)
            if raw then procs "jq" [ "."] stdout' else shells jq stdout' -- .[].ret | {id, return}
            break
      case e of
        Just _ -> pure ExitSuccess
        Nothing -> do
          echo "Could not get a response from the server after 12 attempts (3 min). You might want to try again later."
          pure $ ExitFailure 1


run :: Options -> Shell ExitCode
run = \case

  DocCommand HtmlDoc -> do
    datadir <- dataDir
    browser <- fromMaybe "firefox" <$> need "BROWSER"
    let help_fp = Text.pack (datadir <> "/share/doc/cicd-shell.html")
    proc browser
         [help_fp] empty
  DocCommand ModListDoc -> do
    localdir <- localDir
    let fpath = format (fp%"/.modlist.json") localdir
    proc "jq" [ ".", fpath ] empty
  DocCommand (ModDoc mod) -> do
    localdir <- localDir
    let fpath = format (fp%"/.modhelp.json") localdir
    proc "jq" [ "-r", (".[\"" <> mod <> "\"]"), fpath ] empty

  ZoneCommand zone Stats ->
    runCommand zone (Verbose False) (Raw False) statCmd
  ZoneCommand zone Console ->
    dataDir >>= runCommand zone (Verbose False) (Raw True) . consoleCmd zone
  ZoneCommand zone GenTags ->
    localDir >>= runCommand zone (Verbose False) (Raw False) . genTagsCmd zone
  ZoneCommand zone (Runpuppet arg) ->
    getTarget zone arg >>= runCommand zone (arg^.extraFlag.verbose) (arg^.extraFlag.raw) . runpuppetCmd
  ZoneCommand zone (Ping (AcrossArg across arg)) ->
    getTarget zone arg >>= runCommand zone (arg^.extraFlag.verbose) (arg^.extraFlag.raw) . pingCmd across
  ZoneCommand zone (Sync (AcrossArg across arg)) ->
    getTarget zone arg >>= runCommand zone (arg^.extraFlag.verbose) (arg^.extraFlag.raw) . syncCmd across
  ZoneCommand zone (Facts (FactArg down (AcrossArg across arg))) ->
    getTarget zone arg >>= runCommand zone (arg^.extraFlag.verbose) (arg^.extraFlag.raw) . factCmd (puppetdbUrl zone) across down
  ZoneCommand _ (Data (DataArg Nothing (AcrossArg False (Arg Nothing Nothing Nothing _ _ )))) ->
    die "Running data on all nodes within a stack without providing a key is currently prohibited"
  ZoneCommand _ (Data (DataArg Nothing (AcrossArg True _))) ->
    die "Running data across all stacks without providing a key is currently prohibited"
  ZoneCommand zone (Data (DataArg key (AcrossArg across arg))) ->
    getTarget zone arg >>= runCommand zone (arg^.extraFlag.verbose) (arg^.extraFlag.raw) . dataCmd across key
  ZoneCommand zone (Du arg) ->
    getTarget zone arg >>= runCommand zone (arg^.extraFlag.verbose) (arg^.extraFlag.raw) . duCmd
  ZoneCommand zone (Service (action, name, arg)) ->
    getTarget zone arg >>= runCommand zone (arg^.extraFlag.verbose) (arg^.extraFlag.raw) . serviceCmd action name
  ZoneCommand zone (Orchestrate (OrchArg cmd s)) ->
    getStack s >>= runCommand zone (Verbose False) (Raw True) . orchCmd cmd
  ZoneCommand zone (Result (ResultArg raw (ResultNum n))) ->
    userId >>= runCommand zone (Verbose False) raw . resultCmd (pgUrl zone) raw Nothing (Just n)
  ZoneCommand zone (Result (ResultArg raw (ResultJob j))) ->
    userId >>= runCommand zone (Verbose False) raw . resultCmd (pgUrl zone) raw (Just j) Nothing
  ZoneCommand zone (Setfacts arg) ->
    runCommand zone (arg^.extraFlag.verbose) (arg^.extraFlag.raw) (setfactsCmd arg)

main :: IO ()
main = sh $ options (fromString ("CICD - command line utility (v" <> version <> ")")) optionParser >>= run

interactWith (CmdMsg False msg) =
  liftIO $ Text.putStrLn msg

interactWith (CmdMsg True msg) = do
  liftIO $ putStrLn (msg <> " ? (Y/N)")
  r <- readline
  case r of
    Just "Y" -> return ()
    _        -> die "Abort by the user"


interactive :: MonadIO io => Text -> io ExitCode
interactive c = do
    let
      cp = (Process.shell (toS c))
            { Process.std_in  = Process.Inherit
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            , Process.delegate_ctlc = True
            }
    (_, _, _, ph) <- liftIO $ Process.createProcess cp
    liftIO $ Process.waitForProcess ph
