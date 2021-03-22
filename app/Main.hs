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

-- The Application Monad. A simple wrapper around ReaderT
newtype AppM a =
  AppM {
    unAppM :: ReaderT Config.ShellConfig IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config.ShellConfig)


-- Build the nix shell command line that will run salt remotely.
exportEnvVar :: Zone
             -> AppM ()
exportEnvVar z@(Zone zone) = do
  userid <- view Config.loginId
  userpwd <- view Config.password
  export "SALTAPI_USER" userid
  export "SALTAPI_PASS" (toS userpwd)
  export "SALTAPI_EAUTH" "ldap"
  export "SALTAPI_URL" (Config.saltUrl z)
  export "ZONE" zone

-- Generate the file required to complete node fqdn
initTags :: Zone -> AppM ()
initTags z@(Zone zone) = do
  localdir <- view Config.localdir
  let tagfile = localdir </> ".nodes-" <> toS zone
  found <- liftIO $ Directory.doesFileExist tagfile
  unless found $ do
    let cmd = genTagsCmd z (toS localdir)
    cmdline <- exportEnvVar z  *> pure (cmd^.pep)
    inshell cmdline empty & shell (cmd^.jq) >>= \case
      ExitSuccess -> printf ("`cicd "%s% " gentags` completed successfully.\n") zone
      ExitFailure _ -> printf "WARNING: cannot generate node completion file.\n"


-- Generic run command
runCommand :: Zone
           -> ExtraFlag
           -> PepCmd
           -> AppM ExitCode
runCommand z flag cmd =  do
  unless (flag^.verbosity == Quiet || flag^.dry.coerced) $ putText "Waiting for the following command to compute:" *> putText (cmd^.pep)
  when (flag^.dry.coerced) $ putText (cmd^.pep) *> liftIO exitSuccess
  maybe (pure ()) (liftIO . interactWith) (cmd ^. beforeMsg)
  void $ shell "ping -c1 stash.cirb.lan > /dev/null 2>&1" empty .||. die "cannot connect to stash.cirb.lan, check your connection"
  cmdline <- exportEnvVar z *> pure (cmd^.pep)
  initTags z
  case cmd^.cmdMode of
    ConsoleMode -> interactiveShell cmdline
    NormalMode ->
      if (flag^.raw.coerced)
      then shell cmdline empty
      else inshell cmdline empty & shell (cmd^.jq)
    ProgressMode t ->
      liftIO $ Progress.displayConsoleRegions $ do
        let ticking pg =
              whileM_ (not <$> Progress.isComplete pg) $ do
                threadDelay $ 1000 * 1000
                Progress.tickN pg 1
            nixcmd' = if (flag^.raw.coerced)
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
            if (flag^.raw.coerced) then procs "jq" [ "."] stdout' else shells (cmd^.jq) stdout' -- .[].ret | {id, return}
            break
      case e of
        Just _ -> pure ExitSuccess
        Nothing -> do
          echo "Could not get a response from the server after 12 attempts (3 min). You might want to try again later."
          pure $ ExitFailure 1


runForeman :: (MonadIO m) => ExtraFlag -> Text -> m ExitCode
runForeman extraflag pep = do
  when (extraflag^.verbosity == Verbose) $ putText pep
  when (extraflag^.dry.coerced) $ putText pep *> liftIO exitSuccess
  shell pep empty

run :: Options -> AppM ExitCode
run = \case
  Password -> do
    pwd <- liftIO $ Config.promptPassword
    file <- (</> ".pwd") <$> view Config.localdir
    liftIO $ Config.writePassword file pwd *> exitSuccess
  ZoneCommand zone Stats ->
    runCommand zone defExtraFlag statCmd
  ZoneCommand zone Console ->
    liftIO Config.dataDir >>= runCommand zone ExtraFlag{_raw = Raw True, _verbosity = Quiet, _dry = Dry False} . consoleCmd zone
  ZoneCommand zone GenTags ->
    view Config.localdir >>= runCommand zone defExtraFlag . genTagsCmd zone
  ZoneCommand zone (Ping (AcrossArg arg across)) ->
    mkTarget zone arg >>= runCommand zone (arg^.extraFlag) . pingCmd across
  ZoneCommand zone (Sync (AcrossArg arg across)) ->
    mkTarget zone arg >>= runCommand zone (arg^.extraFlag) . syncCmd across
  ZoneCommand zone (Facts (FactArg (Refresh refresh) (AcrossArg arg across))) -> do
    localdir <- view Config.localdir
    target <- mkTarget zone arg
    let fname = ".facts-" <> toS target <> ".json"
        fpath = localdir </> Text.unpack fname
        cmd = factCmd (Just fpath) across target
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
  ZoneCommand zone (Setfacts arg) ->
    runCommand zone (arg^.extraFlag) (setfactsCmd arg)
  ZoneCommand zone (Foreman arg) -> do
    mkTarget zone arg >>= runForeman (arg^.extraFlag) . foremanCmd Config.foremanUrl
  ZoneCommand zone (Run (RunArg cmd node xflag)) -> runCommand zone xflag (runCmd cmd node)

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
