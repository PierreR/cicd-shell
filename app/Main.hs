{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Concurrent
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import qualified Data.Version                 (showVersion)
import qualified Paths_cicd_shell
import qualified System.Console.AsciiProgress as Progress
import           Turtle                       hiding (FilePath, strict, view)
import qualified Turtle

import           Shell.Cli
import qualified Shell.Config                 as Config
import           Shell.PepCmd
import           Shell.Prelude
import           Shell.Type


getStack :: MonadIO io => Maybe Text -> io Text
getStack s = do
  ds <- Config.userDefaultStack
  pure $ fromMaybe ds s

getTarget :: Zone -> Arg -> Shell Target
getTarget (Zone _zone) Arg{..} = do
  _stack <- getStack _stack
  pure Target{..}

-- | localDir sits in user space
-- It is the location for the gentags generated files that help with completion
localDir :: Shell Turtle.FilePath
localDir = (</> ".local/share/cicd") <$> home

dataDir :: Shell FilePath
dataDir = liftIO Paths_cicd_shell.getDataDir

nixShellCmd :: Zone -> Text -> Shell Text
nixShellCmd (Zone zone) pep = do
  userid <- Config.userId
  userpwd <- Config.userPwd
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
  nixcmd <- nixShellCmd z (cmd^.pep)
  if verbose then putText (cmd^.pep) else pure()
  case cmd^.cmdMode of
    ConsoleMode -> interactiveShell nixcmd
    NormalMode ->
      if raw
      then shell nixcmd empty
      else inshell nixcmd empty & shell (cmd^.jq)
    ProgressMode t ->
      liftIO $ Progress.displayConsoleRegions $ do
        let loop pg = do
              b <- Progress.isComplete pg
              unless b $ do
                  threadDelay $ 1000 * 1000
                  Progress.tickN pg 1
                  loop pg
            nixcmd' =
              if raw then nixcmd else nixcmd <> " | " <> (cmd^.jq)
        pg <- Progress.newProgressBar Progress.def { Progress.pgTotal = t
                                                   , Progress.pgFormat = "Waiting " <> show (t `div` 60) <> " min [:bar], timeout in :eta sec"
                                                   , Progress.pgOnCompletion = Just "After :elapsed sec"
                                                   }
        race (loop pg) (shell' nixcmd') >>= \case
          Left () -> do
            outputConcurrentMsg "Timeout: the command has not returned yet (it is probably still running)."
            pure $ ExitFailure 1
          Right code -> do
            Progress.complete pg
            pure code
    RetryMode -> do
      -- liftIO $ print jq
      e <- loopN 10 $ do
        o0 <- shellStrictWithErr nixcmd empty
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
            if raw then procs "jq" [ "."] stdout' else shells (cmd^.jq) stdout' -- .[].ret | {id, return}
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
    getTarget zone arg >>= runCommand zone (arg^.extraFlag.verbose) (arg^.extraFlag.raw) . factCmd (Config.puppetdbUrl zone) across down
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
    Config.userId >>= runCommand zone (Verbose False) raw . resultCmd (Config.pgUrl zone) raw Nothing (Just n)
  ZoneCommand zone (Result (ResultArg raw (ResultJob j))) ->
    Config.userId >>= runCommand zone (Verbose False) raw . resultCmd (Config.pgUrl zone) raw (Just j) Nothing
  ZoneCommand zone (Setfacts arg) ->
    runCommand zone (arg^.extraFlag.verbose) (arg^.extraFlag.raw) (setfactsCmd arg)

main :: IO ()
main =
  sh $ options (fromString ("CICD - command line utility (v" <> version <> ")")) optionParser >>= run
  where
    version = Data.Version.showVersion Paths_cicd_shell.version


interactWith :: CmdMsg -> Shell ()
interactWith = \case
  CmdMsg False msg ->
    liftIO $ Text.putStrLn msg
  CmdMsg True msg -> do
    liftIO $ putStrLn (msg <> " ? (Y/N)")
    r <- readline
    case r of
      Just "Y" -> return ()
      _        -> die "Abort by the user"
