{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Concurrent
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import qualified System.Console.AsciiProgress as Progress
import qualified System.Directory             as Directory
import qualified Data.List.NonEmpty as NonEmpty
import           Turtle                       hiding (FilePath, strict, view,
                                               (</>), (<>))

import           Shell.Cli
import qualified Shell.Config                 as Config
import           Shell.PepCmd
import           Shell.Prelude                hiding (appendFile, die)
import           Shell.Type

getStacks :: MonadIO io => Maybe Text -> io (NonEmpty Text)
getStacks s = do
  ds <- Config.userDefaultStacks
  pure $ fromList $ maybe ds (:ds) s

mkTarget :: Zone -> Arg -> Shell Target
mkTarget (Zone _zone) Arg{..} = do
  _stacks <- getStacks _stack
  pure Target{..}

shellCmdLine :: Zone -> Text -> Shell Text
shellCmdLine z@(Zone zone) pep = do
  userid <- Config.userId
  userpwd <- Config.userPwd
  datadir <- liftIO $ Config.dataDir
  let pgr = format ("nix-shell "%w%"/share/default.nix --argstr zone "%s%" --argstr salt-user "%s%" --argstr salt-pass "%s%" --argstr salt-url "%s)
                    datadir zone userid userpwd (Config.saltUrl z)
  if Text.null pep
    then pure pgr
    else pure $ pgr <> " --command '" <> pep <> "'"

initTags :: Zone -> Shell ()
initTags z@(Zone zone) = do
  localdir <- liftIO Config.localDir
  let tagfile = localdir </> ".nodes-" <> toS zone
  found <- liftIO $ Directory.doesFileExist tagfile
  unless found $ do
    liftIO $ Directory.createDirectoryIfMissing True localdir
    let cmd = genTagsCmd z (toS localdir)
    cmdline <- shellCmdLine z (cmd^.pep)
    inshell cmdline empty & shell (cmd^.jq) >>= \case
      ExitSuccess -> printf ("`cicd "%s% " gentags` completed successfully.\n") zone
      ExitFailure _ -> printf "WARNING: cannot generate node completion file.\n"

initHelp :: Shell ()
initHelp = do
  gen_help genSaltModListCmd "modlist"
  gen_help genSaltModjsonCmd "modhelp"
  where
    gen_help cmd tag = do
      localdir <- liftIO Config.localDir
      let fpath = localdir </> "." <> tag
      found <- liftIO $ Directory.doesFileExist (fpath <> ".json")
      unless found $ do
        let cmd' = cmd (toS fpath)
        cmdline <- shellCmdLine (Zone "dev") (cmd'^.pep)
        inshell cmdline empty & shell (cmd'^.jq) >>= \case
          ExitSuccess -> putStrLn (fpath <> " generated successfully.\n")
          ExitFailure _ -> putStrLn ("WARNING: cannot generate '" <> fpath <> "' (completion).\n")

runCommand :: Zone -> ExtraFlag -> PepCmd -> Shell ExitCode
runCommand z flag cmd =  do
  maybe (pure ()) interactWith (cmd ^. beforeMsg)
  cmdline <- shellCmdLine z (cmd^.pep)
  when (flag^.verbose) $ putText (cmd^.pep)
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


runForeman :: ExtraFlag -> PepCmd -> Shell ExitCode
runForeman extraflag cmd = do
  when (extraflag^.verbose) $ putText (cmd^.pep)
  when (extraflag^.dry) $ putText (cmd^.pep) *> liftIO exitSuccess
  shell (cmd^.pep) empty

run :: Options -> Shell ExitCode
run = \case

  DocCommand HtmlDoc -> do
    datadir <- liftIO Config.dataDir
    browser <- fromMaybe "firefox" <$> need "BROWSER"
    let help_fp = Text.pack (datadir <> "/share/doc/cicd-shell.html")
    proc browser
         [help_fp] empty
  DocCommand ModListDoc -> do
    localdir <- liftIO Config.localDir
    let fpath = localdir </> ".modlist.json"
    found <- liftIO $ Directory.doesFileExist fpath
    unless found $ initHelp
    proc "jq" [ ".", toS fpath ] empty
  DocCommand (ModDoc mod) -> do
    localdir <- liftIO $ Config.localDir
    let fpath = localdir </> ".modhelp.json"
    found <- liftIO $ Directory.doesFileExist fpath
    unless found $ initHelp
    proc "jq" [ "-r", (".[\"" <> mod <> "\"]"), toS fpath ] empty
  ZoneCommand zone Stats ->
    runCommand zone defExtraFlag statCmd
  ZoneCommand zone Console ->
    liftIO Config.dataDir >>= runCommand zone ExtraFlag{_raw = True, _verbose = False, _dry = False} . consoleCmd zone
  ZoneCommand zone GenTags ->
    liftIO Config.localDir >>= runCommand zone defExtraFlag . genTagsCmd zone
  ZoneCommand zone (Runpuppet arg) -> do
    target <- mkTarget zone arg
    let cmd = runpuppetCmd target
    exit <- runCommand zone (arg^.extraFlag) cmd
    putText "You can view the foreman report using:"
    void $ runForeman (ExtraFlag False False True) (foremanCmd Config.foremanUrl target) *> liftIO exitSuccess
    pure exit
  ZoneCommand zone (Ping (AcrossArg across arg)) ->
    mkTarget zone arg >>= runCommand zone (arg^.extraFlag) . pingCmd across
  ZoneCommand zone (Sync (AcrossArg across arg)) ->
    mkTarget zone arg >>= runCommand zone (arg^.extraFlag) . syncCmd across
  ZoneCommand zone (Facts (FactArg (Refresh refresh) down (AcrossArg across arg))) -> do
    localdir <- liftIO $ Config.localDir
    target <- mkTarget zone arg
    let fname = ".facts-" <> toS target <> ".json"
        fpath = localdir </> Text.unpack fname
        cmd = factCmd fpath Config.puppetdbUrl across down target
    found <- liftIO $ Directory.doesFileExist fpath
    if (found && not refresh)
    then do
      exitCode <- proc "jq" [ ".", toS fpath ] empty
      when (exitCode == ExitSuccess) $ putText "\n↳ These facts are cached. Use --refresh for updated information."
      pure exitCode
    else
      runCommand zone (arg^.extraFlag) cmd
  ZoneCommand _ (Data (DataArg Nothing (AcrossArg False (Arg Nothing Nothing Nothing _ _ )))) ->
    die "Running data on all nodes within a stack without providing a key is currently prohibited"
  ZoneCommand _ (Data (DataArg Nothing (AcrossArg True _))) ->
    die "Running data across all stacks without providing a key is currently prohibited"
  ZoneCommand zone (Data (DataArg key (AcrossArg across arg))) ->
    mkTarget zone arg >>= runCommand zone (arg^.extraFlag) . dataCmd across key
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

main :: IO ()
main =
  sh $ options (fromString ("CICD - command line utility (v" <> Config.version <> ")")) optionParser >>= run
  where

interactWith :: CmdMsg -> Shell ()
interactWith = \case
  CmdMsg False msg ->
    liftIO $ Text.putStrLn msg
  CmdMsg True msg -> do
    liftIO $ putStrLn (msg <> " ? (Y/N)")
    readline >>= \case
      Just "Y" -> pure ()
      _        -> die "Abort by the user"
