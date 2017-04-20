{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text
import qualified Data.Text.Lazy            as Text.Lazy
import qualified Data.Version              (showVersion)
import qualified Dhall
import qualified Paths_cicd_shell
import qualified System.Process            as Process
import           Turtle                    hiding (FilePath, strict, view)
import qualified Turtle

import           Shell.Option
import           Shell.PepCmd
import           Shell.Type
import           Shell.Prelude

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
    touch tagfile
    runCommand z (genTagsCmd z localdir) >>= \case
      ExitSuccess -> printf ("`cicd "%s% " gentags` completed successfully.\n") zone
      ExitFailure _ -> printf "WARNING: cannot generate node completion file.\n"

runCommand :: Zone -> PepCmd -> Shell ExitCode
runCommand z cmd =  do
  shell "ping -c1 stash.cirb.lan > /dev/null 2>&1" empty .||. die "cannot connect to stash.cirb.lan, check your connection"
  initTags z
  maybe (pure ()) interactWith (cmd ^. beforeMsg)
  nixshell <- nixShellCmd z (cmd^.pep)
  -- liftIO $ print nixshell
  case cmd^.jq of
    Default -> interactive nixshell
    Specific jq -> do
      -- liftIO $ print jq
      e <- loopN 10 $ do
        o0 <- shellStrict nixshell empty
        case o0 of
          (ExitFailure _, _) -> do
            echo "Failing to get a response from the server. Retrying in 15 sec. Press Ctrl-C to abort."
            liftIO $ threadDelay (15 * 1000 * 1000)
            continue
          (ExitSuccess, stdout) -> do
            void $ shell jq (select (textToLines stdout))
            break
      case e of
        Just _ -> pure ExitSuccess
        Nothing -> do
          echo "Could not get a response from the server after 12 attempts (3 min). You might want to try again later."
          pure $ ExitFailure 1


-- help options
run (Options (HelpCommand HtmlHelp)) = do
  datadir <- dataDir
  let help_fp = Text.pack (datadir <> "/share/doc/cicd-shell.html")
  proc "google-chrome-stable"
       [help_fp] empty

-- prohibited options
run (Options (ZoneCommand _ (Data (DataArg Nothing (Arg Nothing Nothing Nothing _)))))  = die "Running data on the whole stack is currently prohibited"

-- valid options
run (Options (ZoneCommand zone Console))                           = dataDir>>= runCommand zone . consoleCmd zone
run (Options (ZoneCommand zone Stats))                             = runCommand zone statCmd
run (Options (ZoneCommand zone GenTags))                           = localDir >>= runCommand zone . genTagsCmd zone
run (Options (ZoneCommand zone (Runpuppet arg)))                   = getTarget zone arg >>= runCommand zone . runpuppetCmd
run (Options (ZoneCommand zone (Ping (AcrossArg across arg))))     = getTarget zone arg >>= runCommand zone . pingCmd across
run (Options (ZoneCommand zone (Facts (FactArg across down arg)))) = getTarget zone arg >>= runCommand zone . factCmd (puppetdbUrl zone) across down
run (Options (ZoneCommand zone (Sync (AcrossArg across arg))))     = getTarget zone arg >>= runCommand zone . syncCmd across
run (Options (ZoneCommand zone (Data (DataArg key arg))))          = getTarget zone arg >>= runCommand zone . dataCmd key
run (Options (ZoneCommand zone (Orchestrate (OrchArg cmd s))))     = getStack s >>= runCommand zone . orchCmd cmd
run (Options (ZoneCommand zone (Du arg)))                          = getTarget zone arg >>= runCommand zone . duCmd
run (Options (ZoneCommand zone (Service (action, name, arg))))     = getTarget zone arg >>= runCommand zone . serviceCmd action name
run (Options (ZoneCommand zone (Result (ResultNum n))))            = userId >>= runCommand zone . resultCmd (pgUrl zone) Nothing (Just n)
run (Options (ZoneCommand zone (Result (ResultJob j ))))           = userId >>= runCommand zone . resultCmd (pgUrl zone) (Just j) Nothing

main :: IO ()
main = sh $ options (fromString ("CICD - command line utility (v" <> version <> ")")) parser >>= run

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
