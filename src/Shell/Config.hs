{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module Shell.Config (
    pgUrl
  , puppetdbUrl
  , userId
  , userPwd
  , userDefaultStack
  , HasShellConfig(..)
) where

import qualified Data.Text         as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Dhall
import Turtle (home, (</>), format, fp, die)

import           Shell.Prelude
import           Shell.Type

-- | return the first found configuration file
-- both "/vagrant/config/shell and "~/.config/cicd/shell" are tried in that order
configFilePath :: MonadIO io => io Text
configFilePath = do
  _HOME <- home
  let paths = ["/vagrant/config/shell", format fp (_HOME </> ".config/cicd/shell")]
  findFirstPath paths >>= \case
    Nothing -> die ("no configuration file found in " <> (Text.intercalate " or " paths))
    Just v -> pure v

data ShellConfig
  = ShellConfig
  { _loginId      :: LText
  , _password     :: LText
  , _defaultStack :: LText
  } deriving (Generic, Show)

makeClassy ''ShellConfig

instance Dhall.Interpret ShellConfig

userId :: MonadIO io => io Text
userId = view (loginId.strict) <$> mkShellConfig

userPwd :: MonadIO io => io Text
userPwd = view (password.strict) <$> mkShellConfig

userDefaultStack :: MonadIO io => io Text
userDefaultStack = view (defaultStack.strict) <$> mkShellConfig

mkShellConfig :: MonadIO m => m ShellConfig
mkShellConfig =
  liftIO $ Dhall.input auto =<< (fromStrict <$> configFilePath)
  where
    auto ::  Dhall.Interpret a => Dhall.Type a
    auto = Dhall.autoWith
      ( Dhall.defaultInterpretOptions { Dhall.fieldModifier = Text.Lazy.dropWhile (== '_') })

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

puppetdbUrl :: Zone -> Text
puppetdbUrl (Zone z)
  | z == "sandbox" = "http://puppetdb.sandbox.srv.cirb.lan:8080"
  | otherwise      = "http://puppetdb.prd.srv.cirb.lan:8080"
