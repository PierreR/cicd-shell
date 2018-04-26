{-# LANGUAGE RecordWildCards #-}
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
  , promptPassword
  , writePassword
  , puppetdbUrl
  , saltUrl
  , mkShellConfig
  , userId
  , userPwd
  , userDefaultStacks
  , version
  , HasShellConfig(..)
  , ShellConfig(..)
  , mockShellConfig
) where

import qualified Data.List        as List
import qualified Data.Text.Lazy   as Text.Lazy
import qualified Data.Version     (showVersion)
import qualified Dhall
import qualified Paths_cicd_shell
import qualified System.Directory as Directory
import qualified System.IO
import qualified System.Console.Haskeline as Haskeline

import           Shell.Prelude
import           Shell.Type

dataDir :: IO FilePath
dataDir = Paths_cicd_shell.getDataDir

version = Data.Version.showVersion Paths_cicd_shell.version

-- return the first found configuration file
configFilePath :: IO FilePath
configFilePath = do
  _HOME <- liftIO $ Directory.getHomeDirectory
  let paths = ["/vagrant/config/shell.dhall", _HOME </> ".config/cicd/shell.dhall"]
  findFirstPath paths >>= \case
    Nothing -> die ("no configuration file found in " <> toS (List.intercalate " or " paths))
    Just v -> pure v

data DhallConfig
  = DhallConfig
  { _loginId       :: LText
  , _defaultStacks :: [LText]
  } deriving (Generic, Show)

data ShellConfig
  = ShellConfig
  { _localdir :: FilePath
  , _dhall :: DhallConfig
  , _password :: String
  }

makeClassy ''DhallConfig
makeClassy ''ShellConfig

instance Dhall.Interpret DhallConfig

-- | User AD login id
userId :: (MonadIO m, MonadReader ShellConfig m) => m Text
userId = asks (view (dhall.loginId.strict))

-- | Directories where gentags & genhelp files are stored.
localDir :: (MonadIO m, MonadReader ShellConfig m) => m FilePath
localDir = asks (view localdir)

-- | User AD password
userPwd :: (MonadIO m, MonadReader ShellConfig m) => m String
userPwd = asks (view password)

wizard :: FilePath -> IO [Char]
wizard localdir = do
  let pwd_file = localdir </> ".pwd"
  ifM (Directory.doesFileExist pwd_file)
    (withFile pwd_file ReadMode $ \h ->
      System.IO.hGetLine h)
    (passwordWizard pwd_file)

passwordWizard :: FilePath -> IO String
passwordWizard fname = do
  pwd <- promptPassword
  putText "We recommend saving the password locally."
  putText "If you feel unsafe about this, you can always add a level0 running script to remove the file at shutdown"
  putText "Enter 'Y' to continue or anything else including <Enter> to prevent the password from being stored."
  getLine >>= \case
    "Y" -> writePassword fname pwd *> pure pwd
    _ -> pure pwd

writePassword :: FilePath-> String -> IO ()
writePassword fname pwd = do
  System.IO.writeFile fname pwd
  putText "Your passport has been saved. To change it, use 'cicd pass'"

promptPassword :: IO String
promptPassword = do
  Just pwd <- Haskeline.runInputT Haskeline.defaultSettings $ do
    Haskeline.getPassword (Just '*') "Enter your AD password and press Enter\n"
  pure pwd

-- | User default puppet stack
userDefaultStacks ::(MonadIO io, MonadReader ShellConfig io) => io [Text]
userDefaultStacks = do
  asks (toListOf (dhall.defaultStacks.traverse.strict))

mockShellConfig :: NonEmpty Text -> ShellConfig
mockShellConfig default_stacks =
  ShellConfig { _localdir = mempty , _dhall = DhallConfig mempty (fromStrict <$>  (toList default_stacks)) , _password = mempty}

mkShellConfig :: IO ShellConfig
mkShellConfig = do
  home <- Directory.getHomeDirectory
  let _localdir = home </> ".local/share/cicd"
  _dhall <- mkDhallConfig
  _password <- wizard _localdir
  pure $ ShellConfig {..}

mkDhallConfig :: MonadIO m => m DhallConfig
mkDhallConfig =
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
