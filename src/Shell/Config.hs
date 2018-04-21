{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Configuration data are either static or read from file.
--
-- The user config file is mandatory and expected to sit in
-- `/vagrant/config/shell` or `~/.config/cicd/shell` (in that order).
module Shell.Config (
    dataDir
  , foremanUrl
  , localDir
  , pgUrl
  , puppetdbUrl
  , saltUrl
  , userId
  , userPwd
  , userDefaultStacks
  , version
  , HasShellConfig(..)
) where

import qualified Data.List        as List
import qualified Data.Text.IO     as Text
import qualified Data.Text.Lazy   as Text.Lazy
import qualified Data.Version     (showVersion)
import qualified Dhall
import qualified Paths_cicd_shell
import qualified System.Directory as Directory

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
  { _loginId       :: LText
  , _defaultStacks :: [LText]
  } deriving (Generic, Show)

makeClassy ''ShellConfig

instance Dhall.Interpret ShellConfig

-- | User AD login id
userId :: MonadIO io => io Text
userId = view (loginId.strict) <$> mkShellConfig

-- | User AD password
userPwd :: IO Text
userPwd = do
  localdir <- localDir
  let pwd_file = localdir </> ".pwd"
  ifM (Directory.doesFileExist pwd_file)
    (withFile pwd_file ReadMode $ \h ->
      Text.hGetLine h)
    (wizard pwd_file)
  where
    wizard fname = do
      putText "What's your AD password ?"
      pwd <- Text.getLine
      Text.writeFile fname pwd
      pure pwd

-- | User default puppet stack
userDefaultStacks :: MonadIO io => io [Text]
userDefaultStacks = toListOf (defaultStacks.traverse.strict) <$> mkShellConfig

mkShellConfig :: MonadIO m => m ShellConfig
mkShellConfig =
  liftIO $ Dhall.input auto =<< (toS <$> configFilePath)
  where
    auto ::  Dhall.Interpret a => Dhall.Type a
    auto = Dhall.autoWith
      ( Dhall.defaultInterpretOptions { Dhall.fieldModifier = Text.Lazy.dropWhile (== '_') })


infraPrefix = \case
  "prod" -> "prd"
  "staging" -> "sta"
  "testing" -> "sta"
  "dev" -> "dev"
  _ -> panic $ "Unrecognized zone"

-- | Pgserver url
pgUrl :: Zone -> Text
pgUrl (Zone z) =
  "http://pgserver-cicd." <> infraPrefix z <> ".srv.cirb.lan/saltstack/salt_result"

-- | Puppetdb url
puppetdbUrl :: Text
puppetdbUrl =
  "http://puppetdb.prd.srv.cirb.lan:8080"

saltUrl :: Zone -> Text
saltUrl (Zone z) =
  "https://salt." <> infraPrefix z <> ".srv.cirb.lan:8000"

foremanUrl :: Text
foremanUrl = "http://foreman.prd.srv.cirb.lan"
