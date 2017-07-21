{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Shell.Config (
    pgUrl
  , puppetdbUrl
  , userId
  , userPwd
  , userDefaultStack
  , HasShellConfig(..)
) where

import qualified Data.Text      as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Dhall
import Turtle (die)

import           Shell.Prelude
import           Shell.Type

-- ROOT_DIR on the host
-- TODO: remove devbox/vagrant deps
configFilePath = "/vagrant/config/shell"

data ShellConfig
  = ShellConfig
  { _loginId      :: LText
  , _password     :: LText
  , _defaultStack :: LText
  } deriving (Generic, Show)

makeClassy ''ShellConfig

instance Dhall.Interpret ShellConfig

userId :: MonadIO io => io Text
userId = do
  user_id <- view (loginId.strict) <$> mkShellConfig
  if Text.null user_id
    then die  ("Your loginId is empty. Have you filled in " <> configFilePath <> " ?")
    else pure user_id

userPwd :: MonadIO io => io Text
userPwd = do
  pwd <- view (password.strict) <$> mkShellConfig
  if Text.null  pwd
    then die  ("Your password is empty. Have you filled in " <> configFilePath <> " ?")
    else pure pwd

userDefaultStack :: MonadIO io => io Text
userDefaultStack = do
  s <- view (defaultStack.strict) <$> mkShellConfig
  if Text.null s
    then die  ("The default stack is empty. Have you filled in " <> configFilePath <> " ?")
    else pure s


mkShellConfig :: MonadIO m => m ShellConfig
mkShellConfig = 
  liftIO $ Dhall.input auto (fromStrict configFilePath)
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
