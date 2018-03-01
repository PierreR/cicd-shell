{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

-- | Configuration data are either static or read from file.
--
-- The user config file is mandatory and expected to sit in
-- `/vagrant/config/shell` or `~/.config/cicd/shell` (in that order).
module Shell.Config (
    dataDir
  , localDir
  , pgUrl
  , puppetdbUrl
  , saltUrl
  , userId
  , userPwd
  , userDefaultStack
  , version
  , HasShellConfig(..)
) where

import qualified Data.List        as List
import qualified Data.Text.Lazy   as Text.Lazy
import qualified Data.Version     (showVersion)
import qualified Dhall
import qualified System.Directory          as Directory
import qualified Paths_cicd_shell

import           Shell.Prelude
import           Shell.Type

dataDir :: IO FilePath
dataDir = Paths_cicd_shell.getDataDir

version = Data.Version.showVersion Paths_cicd_shell.version

-- | Directories where gentags & genhelp files are stored.
localDir :: IO FilePath
localDir = (</> ".local/share/cicd") <$> Directory.getHomeDirectory

-- return the first found configuration file
configFilePath :: IO FilePath
configFilePath = do
  _HOME <- liftIO $ Directory.getHomeDirectory
  let paths = ["/vagrant/config/shell.dhall", _HOME </> ".config/cicd/shell.dhall"]
  findFirstPath paths >>= \case
    Nothing -> die ("no configuration file found in " <> toS (List.intercalate " or " paths))
    Just v -> pure v

data ShellConfig
  = ShellConfig
  { _loginId      :: LText
  , _password     :: LText
  , _defaultStack :: LText
  } deriving (Generic, Show)

makeClassy ''ShellConfig

instance Dhall.Interpret ShellConfig

-- | User AD login id
userId :: MonadIO io => io Text
userId = view (loginId.strict) <$> mkShellConfig

-- | User AD password
userPwd :: MonadIO io => io Text
userPwd = view (password.strict) <$> mkShellConfig

-- | User default puppet stack
userDefaultStack :: MonadIO io => io Text
userDefaultStack = view (defaultStack.strict) <$> mkShellConfig

mkShellConfig :: MonadIO m => m ShellConfig
mkShellConfig =
  liftIO $ Dhall.input auto =<< (toS <$> configFilePath)
  where
    auto ::  Dhall.Interpret a => Dhall.Type a
    auto = Dhall.autoWith
      ( Dhall.defaultInterpretOptions { Dhall.fieldModifier = Text.Lazy.dropWhile (== '_') })

-- | Pgserver url
pgUrl :: Zone -> Text
pgUrl (Zone z) =
  let
    pgserver_prod     = "http://pgserver-cicd.prd.srv.cirb.lan/saltstack"
    pgserver_sandbox = "http://pgserver.sandbox.srv.cirb.lan/saltstack"
    result_suffix = "/salt_result"
  in
  case z of
    "sandbox" -> pgserver_sandbox <> result_suffix
    "prod"    -> pgserver_prod <> result_suffix
    _         -> pgserver_prod <> "_" <> z <> result_suffix

-- | Puppetdb url
puppetdbUrl :: Zone -> Text
puppetdbUrl (Zone z)
  | z == "sandbox" = "http://puppetdb.sandbox.srv.cirb.lan:8080"
  | otherwise      = "http://puppetdb.prd.srv.cirb.lan:8080"

saltUrl :: Zone -> Text
saltUrl (Zone z) =
  case z of
    "prod" -> "https://salt.prd.srv.cirb.lan:8000"
    "staging" -> "https://salt.sta.srv.cirb.lan:8000"
    "testing" -> "https://salt.sta.srv.cirb.lan:8000"
    "dev" -> "https://salt.dev.srv.cirb.lan:8000"
    "sandbox" -> "https://saltmaster.sandbox.srv.cirb.lan:8000"
    _ -> panic $ "Unrecognized zone " <> z
