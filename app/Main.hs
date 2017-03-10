{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
module Main where

import           Control.Lens           (makeLenses, strict, view)
import           Control.Lens.Operators hiding ((<.>))
import           Data.Foldable          (for_)
import           Data.Maybe             (fromMaybe)
import           Data.Optional          (Optional (..))
import qualified Data.Optional          as Optional
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import qualified Data.Text.Lazy         as Text.Lazy
import qualified Data.Version           (showVersion)
import           Dhall                  hiding (Text, auto, input, maybe, text)
import qualified Dhall
import           GHC.Generics
import qualified Paths_cicd_shell
import qualified System.Process         as Process hiding (FilePath)
import           Turtle                 hiding (strict, view)
import Control.Exception

import           Option
import           PepCmd
import           Type

version = Data.Version.showVersion Paths_cicd_shell.version

-- | ROOT_DIR on the host
-- TODO: remove devbox/vagrant deps
configFilePath = "/vagrant/config/shell"

type LText = Text.Lazy.Text

auto :: (GenericInterpret (Rep a), Generic a) => Type a
auto = deriveAuto
  ( defaultInterpretOptions { fieldModifier = Text.Lazy.dropWhile (== '_') })

data ShellConfig
  = ShellConfig
  { _loginId      :: LText
  , _password     :: LText
  , _defaultStack :: LText
  } deriving (Generic, Show)

makeLenses ''ShellConfig

instance Interpret ShellConfig

shellConfig :: MonadIO m => m ShellConfig
shellConfig = do
  r <- liftIO $ (try $ Dhall.input auto (Text.Lazy.fromStrict configFilePath) :: IO (Either SomeException ShellConfig))
  case r of
    Left ex  -> liftIO (printf "Fail to read configuration file\n" >> throwIO ex)
    Right cf -> pure cf

userId :: MonadIO io => io Text
userId = do
  user_id <- view (loginId.strict) <$> shellConfig
  if Text.null user_id
    then die  ("Your loginId is empty. Have you filled in " <> configFilePath <> " ?")
    else pure $ user_id

userPwd :: MonadIO io => io Text
userPwd = do
  password <- view (password.strict) <$> shellConfig
  if Text.null password
    then die  ("Your password is empty. Have you filled in " <> configFilePath <> " ?")
    else pure $ password

getStack :: MonadIO io => Maybe Text -> io Text
getStack s = do
  def <- view (defaultStack.strict) <$> shellConfig
  if Text.null def
    then die  ("The default stack is empty. Have you filled in " <> configFilePath <> " ?")
    else pure $ fromMaybe def s

getTarget :: Text -> Arg -> Shell Target
getTarget zone Arg {..} = do
  stack' <- getStack _argStack
  pure $ Target _argNode _argSubgroup _argRole stack' zone

saltUrl zone =
  case zone of
    "dev"     -> "https://salt.dev.srv.cirb.lan:8000"
    "sandbox" -> "https://saltmaster.sandbox.srv.cirb.lan:8000"
    "staging" -> "https://salt.sta.srv.cirb.lan:8000"
    "testing" -> "https://salt.sta.srv.cirb.lan:8000"
    "prod"    -> "https://salt.prd.srv.cirb.lan:8000"

pgUrl :: Text -> Text
pgUrl zone =
  let
    pgserver_prod     = "http://pgserver-cicd.prd.srv.cirb.lan/saltstack"
    pgserver_sandbox = "http://pgserver.sandbox.srv.cirb.lan/saltstack"
    result_suffix = "/salt_result"
  in
  case zone of
    "sandbox" -> pgserver_sandbox <> result_suffix
    "prod"    -> pgserver_prod <> result_suffix
    _         -> pgserver_prod <> "_" <> zone <> result_suffix

puppetdbUrl zone
  | zone == "sandbox" = "http://puppetdb.sandbox.srv.cirb.lan:8080"
  | otherwise         = "http://puppetdb.prd.srv.cirb.lan:8080"

-- | localDir sits in user space
localDir :: Shell Turtle.FilePath
localDir = (</> ".local/share/cicd") <$> home

nixFileName :: Text -> Text
nixFileName zone = zone <> "-" <> Text.pack version <> ".nix"

-- | The file path of 'share/default.nix' (used by 'nix-shell' to run commands)
-- It is located in the share folder of the cicd derivation inside the nix store
defaultNixFilePath = do
  (</> "../share") . parent. fromText . lineToText <$> (inshell "readlink -f $(which cicd)" empty)

-- sensitive information such as a password won't be copy in the nixfile
writeTarget :: Turtle.FilePath -> Text -> Text -> Shell ()
writeTarget file zone user = do
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
  liftIO $ writeTextFile file (Text.unlines (lines (format fp dnfp)))

runCommand :: Text -> PepCmd -> Shell ExitCode
runCommand zone cmd =  do
  let
    nixcommand z = Text.unwords ["nix-shell", nixFileName z]
    msg = cmd ^. cmdmsg
    pgr pwd = nixcommand zone <>  " --argstr user_pwd " <> pwd
    pepcmd pwd =
      if Text.null (cmd^.cmdpep)
        then pgr pwd
        else pgr pwd <> " --command '" <> cmd^.cmdpep <> "'"

    initEnv z u = do
      localdir <- localDir
      let nixfile = localdir </> fromText (nixFileName z)
      found <- testfile nixfile
      unless found $ do
        writeTarget nixfile z u
        printf (fp%" created.\n") nixfile
        shell ("cicd " <> zone <> " gentags") empty >>= \case
          ExitSuccess -> printf ("`cicd "%s% " gentags` completed successfully.\n") z
          ExitFailure _ -> printf "WARNING: cannot generate node completion file.\n"

  salt_pass <- userPwd
  foundconfdir <- testdir =<< localDir
  unless foundconfdir $ mkdir =<< localDir
  pushd =<< localDir
  initEnv zone =<< userId
  maybe (pure ()) interactWith msg
  -- liftIO $ print (pepcmd salt_pass)
  case cmd^.cmdjq of
    Default -> interactive (pepcmd salt_pass)
    Specific jq -> do
      -- liftIO $ print jq
      inshell (pepcmd salt_pass) empty & shell jq


-- prohibited options
run (Options zone (Data (DataArg Nothing (Arg Nothing Nothing Nothing s))))  = die "Running data on the whole stack is currently prohibited"

-- valid options
run (Options zone Console)                           = defaultNixFilePath >>= runCommand zone . consoleCmd zone
run (Options zone Stats)                             = runCommand zone statCmd
run (Options zone GenTags)                           = localDir >>= runCommand zone . genTagsCmd zone
run (Options zone (Runpuppet arg))                   = getTarget zone arg >>= runCommand zone . runpuppetCmd
run (Options zone (Ping (AcrossArg across arg)))     = getTarget zone arg >>= runCommand zone . pingCmd across
run (Options zone (Facts (FactArg across down arg))) = getTarget zone arg >>= runCommand zone . factCmd (puppetdbUrl zone) across down
run (Options zone (Sync (AcrossArg across arg)))     = getTarget zone arg >>= runCommand zone . syncCmd across
run (Options zone (Data (DataArg key arg)))          = getTarget zone arg >>= runCommand zone . dataCmd key
run (Options zone (Orchestrate (OrchArg cmd s)))     = getStack s >>= runCommand zone . orchCmd cmd
run (Options zone (Du arg))                          = getTarget zone arg >>= runCommand zone . duCmd
run (Options zone (Service (action, name, arg)))     = getTarget zone arg >>= runCommand zone . serviceCmd action name
run (Options zone (Result (ResultNum n)))            = userId >>= runCommand zone . resultCmd (pgUrl zone) Nothing (Just n)
run (Options zone (Result (ResultJob j )))           = userId >>= runCommand zone . resultCmd (pgUrl zone) (Just j) Nothing


main :: IO ()
main = sh $ options (fromString ("CICD - command line utility (v" <> version <> ")")) parser >>= run

interactWith (CmdMsg False msg) =
  liftIO $ Text.putStrLn msg

interactWith (CmdMsg True msg) = do
  liftIO $ Text.putStrLn (msg <> " ? (Y/N)")
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
