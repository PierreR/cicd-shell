{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Control.Concurrent
import qualified Data.List.NonEmpty           as NonEmpty
import qualified Data.Text                    as Text
import qualified System.Console.AsciiProgress as Progress
import qualified System.Directory             as Directory
import           Turtle                       hiding (FilePath, strict, view, (</>), (<>))

import           Shell.Cli
import qualified Shell.Config                 as Config
import           Shell.PepCmd
import           Shell.Prelude                hiding (appendFile, die)
import           Shell.Target
import           Shell.Type
import qualified Shell.PuppetDB as PuppetDB

-- The Application Monad. A simple wrapper around ReaderT
newtype AppM a =
  AppM {
    unAppM :: ReaderT Config.ShellConfig IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config.ShellConfig)


-- Build the nix shell command line that will run salt remotely.
exportEnvVar :: Zone
             -> AppM ()
exportEnvVar z@(Zone zone) = do
  userid <- Config.userId
  userpwd <- Config.userPwd
  export "SALTAPI_USER" userid
  export "SALTAPI_PASS" (toS userpwd)
  export "SALTAPI_EAUTH" "ldap"
  export "SALTAPI_URL" (Config.saltUrl z)
  export "ZONE" zone

-- Generate the file required to complete node fqdn
initTags :: Zone -> AppM ()
initTags z@(Zone zone) = do
  localdir <- Config.localDir
  let tagfile = localdir </> ".nodes-" <> toS zone
  found <- liftIO $ Directory.doesFileExist tagfile
  unless found $ do
    let cmd = genTagsCmd z (toS localdir)
    cmdline <- exportEnvVar z  *> pure (cmd^.pep)
    inshell cmdline empty & shell (cmd^.jq) >>= \case
      ExitSuccess -> printf ("`cicd "%s% " gentags` completed successfully.\n") zone
      ExitFailure _ -> printf "WARNING: cannot generate node completion file.\n"

-- Generate the file required to display salt help with caching
initHelp :: AppM ()
initHelp = do
  gen_help genSaltModListCmd "modlist"
  gen_help genSaltModjsonCmd "modhelp"
  where
    gen_help cmd tag = do
      localdir <- Config.localDir
      let fpath = localdir </> "." <> tag
      found <- liftIO $ Directory.doesFileExist (fpath <> ".json")
      unless found $ do
        let cmd' = cmd (toS fpath)
        cmdline <- exportEnvVar (Zone "dev") *> pure (cmd'^.pep)
        inshell cmdline empty & shell (cmd'^.jq) >>= \case
          ExitSuccess -> putStrLn (fpath <> " generated successfully.\n")
          ExitFailure _ -> putStrLn ("WARNING: cannot generate '" <> fpath <> "' (completion).\n")

-- Generic run command
runCommand :: Zone
           -> ExtraFlag
           -> PepCmd
           -> AppM ExitCode
runCommand z flag cmd =  do
  maybe (pure ()) (liftIO . interactWith) (cmd ^. beforeMsg)
  cmdline <- exportEnvVar z *> pure (cmd^.pep)
  unless (flag^.quiet || flag^.dry) $ putText "Waiting for the following command to compute:" *> putText (cmd^.pep)
  when (flag^.dry) $ putText (cmd^.pep) *> liftIO exitSuccess
  void $ shell "ping -c1 stash.cirb.lan > /dev/null 2>&1" empty .||. die "cannot connect to stash.cirb.lan, check your connection"
  initTags z
  initHelp
  case cmd^.cmdMode of
    ConsoleMode -> interactiveShell cmdline
    NormalMode ->
      if (flag^.raw)
      then shell cmdline empty
      else inshell cmdline empty & shell (cmd^.jq)
    ProgressMode t ->
      liftIO $ Progress.displayConsoleRegions $ do
        let ticking pg =
              whileM_ (not <$> Progress.isComplete pg) $ do
                threadDelay $ 1000 * 1000
                Progress.tickN pg 1
            nixcmd' = if (flag^.raw)
                      then cmdline
                      else cmdline <> " | " <> (cmd^.jq)
        pg <- Progress.newProgressBar Progress.def { Progress.pgTotal = t
                                                   , Progress.pgFormat = "Waiting " <> show (t `div` 60) <> " min [:bar], timeout in :eta sec"
                                                   , Progress.pgOnCompletion = Just "After :elapsed sec"
                                                   }
        race (ticking pg) (shell' nixcmd') >>= \case
          Left () -> do
            outputConcurrentMsg "Timeout: the command has not returned yet (it is probably still running)."
            pure $ ExitFailure 1
          Right code -> do
            Progress.complete pg
            pure code
    RetryMode -> do
      -- liftIO $ print jq
      e <- loopN 10 $ do
        shellStrictWithErr cmdline empty >>= \case
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
            if (flag^.raw) then procs "jq" [ "."] stdout' else shells (cmd^.jq) stdout' -- .[].ret | {id, return}
            break
      case e of
        Just _ -> pure ExitSuccess
        Nothing -> do
          echo "Could not get a response from the server after 12 attempts (3 min). You might want to try again later."
          pure $ ExitFailure 1


runForeman :: (MonadIO m) => ExtraFlag -> Text -> m ExitCode
runForeman extraflag pep = do
  unless (extraflag^.quiet) $ putText pep
  when (extraflag^.dry) $ putText pep *> liftIO exitSuccess
  shell pep empty

run :: Options -> AppM ExitCode
run = \case
  Password -> do
    pwd <- liftIO $ Config.promptPassword
    file <- (</> ".pwd") <$> Config.localDir
    liftIO $ Config.writePassword file pwd *> exitSuccess
  DocCommand HtmlDoc -> do
    datadir <- liftIO Config.dataDir
    browser <- fromMaybe "firefox" <$> need "BROWSER"
    let help_fp = Text.pack (datadir <> "/share/doc/cicd-shell.html")
    proc browser
         [help_fp] empty
  DocCommand ModListDoc -> do
    localdir <- Config.localDir
    let fpath = localdir </> ".modlist.json"
    found <- liftIO $ Directory.doesFileExist fpath
    unless found $ initHelp
    proc "jq" [ ".", toS fpath ] empty
  DocCommand (ModDoc mod) -> do
    localdir <- Config.localDir
    let fpath = localdir </> ".modhelp.json"
    found <- liftIO $ Directory.doesFileExist fpath
    unless found $ initHelp
    proc "jq" [ "-r", (".[\"" <> mod <> "\"]"), toS fpath ] empty
  ZoneCommand zone Stats ->
    runCommand zone defExtraFlag statCmd
  ZoneCommand zone Console ->
    liftIO Config.dataDir >>= runCommand zone ExtraFlag{_raw = True, _quiet = True, _dry = False} . consoleCmd zone
  ZoneCommand zone GenTags ->
    Config.localDir >>= runCommand zone defExtraFlag . genTagsCmd zone
  ZoneCommand zone (Runpuppet (RunpuppetArg arg noop)) -> do
    target <- mkTarget zone arg
    let cmd = runpuppetCmd noop target
    exit <- runCommand zone (arg^.extraFlag) cmd
    putText "You can view the foreman report using:"
    void $ runForeman (ExtraFlag False False True) (foremanCmd Config.foremanUrl target) *> liftIO exitSuccess
    pure exit
  ZoneCommand zone (Ping (AcrossArg across arg)) ->
    mkTarget zone arg >>= runCommand zone (arg^.extraFlag) . pingCmd across
  ZoneCommand zone (Sync (AcrossArg across arg)) ->
    mkTarget zone arg >>= runCommand zone (arg^.extraFlag) . syncCmd across
  ZoneCommand zone (Facts (FactArg (Refresh _) down@(Down True) (AcrossArg across arg@(Arg _ (Just n) _ _ _ _ )))) -> do
    target <- mkTarget zone arg
    let cmd = factCmd Nothing across down target
    r <- liftIO $ PuppetDB.getFacts n
    shell (cmd^.jq) (pure  (unsafeTextToLine (toS r)))
  ZoneCommand zone (Facts (FactArg (Refresh refresh) down@(Down False) (AcrossArg across arg))) -> do
    localdir <- Config.localDir
    target <- mkTarget zone arg
    let fname = ".facts-" <> toS target <> ".json"
        fpath = localdir </> Text.unpack fname
        cmd = factCmd (Just fpath) across down target
    found <- liftIO $ Directory.doesFileExist fpath
    if (found && not refresh)
    then do
      exitCode <- proc "jq" [ ".", toS fpath ] empty
      when (exitCode == ExitSuccess) $ putText "\n> These facts are cached. Use --refresh for updated information."
      pure exitCode
    else
      runCommand zone (arg^.extraFlag) cmd
  ZoneCommand zone (Du arg) ->
    mkTarget zone arg >>= runCommand zone (arg^.extraFlag) . duCmd
  ZoneCommand zone (State (StateArg cmd node xflag)) ->
    runCommand zone xflag (stateCmd cmd node)
  ZoneCommand zone (Service (action, name, arg)) ->
    mkTarget zone arg >>= runCommand zone (arg^.extraFlag) . serviceCmd action name
  ZoneCommand zone (Orchestrate (OrchArg cmd s flag)) ->
    NonEmpty.head <$> (getStacks s) >>= runCommand zone flag . orchCmd cmd
  ZoneCommand zone (Result (ResultArg flag (ResultNum n))) ->
    Config.userId >>= runCommand zone flag . resultCmd (Config.pgUrl zone) (flag^.raw) Nothing (Just n)
  ZoneCommand zone (Result (ResultArg flag (ResultJob j))) ->
    Config.userId >>= runCommand zone flag . resultCmd (Config.pgUrl zone) (flag^.raw) (Just j) Nothing
  ZoneCommand zone (Setfacts arg) ->
    runCommand zone (arg^.extraFlag) (setfactsCmd arg)
  ZoneCommand zone (Foreman arg) -> do
    mkTarget zone arg >>= runForeman (arg^.extraFlag) . foremanCmd Config.foremanUrl
  ZoneCommand zone (Validate arg) -> do
    mkTarget zone arg >>= runCommand zone (arg^.extraFlag) . validateCmd

main :: IO ()
main = do
  cmd_options <- options (fromString ("CICD - command line utility (v" <> Config.version <> ")")) optionParser
  exit_code <- runApp (run cmd_options) =<< Config.mkShellConfig
  exitWith exit_code
  where
    runApp = runReaderT . unAppM

interactWith :: CmdMsg -> IO ()
interactWith = \case
  CmdMsg False msg ->
    putDoc msg
  CmdMsg True msg -> do
    putDoc (msg <> " ? (Y/N)" <> line)
    readline >>= \case
      Just "Y" -> pure ()
      _        -> die "Abort by the user"
