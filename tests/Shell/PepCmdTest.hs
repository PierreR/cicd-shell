{-# LANGUAGE OverloadedLists #-}
module Shell.PepCmdTest where

import           Shell.PepCmd.Utils
import           Shell.Prelude
import           Shell.Type

import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy as Text
import           Test.Tasty
import           Test.Tasty.Golden


test_compountTarget :: TestTree
test_compountTarget = do
  let Just t = readTarget "cicd.salt.master.dev"
  goldenVsString "golden" "tests/Shell/cicd-salt-master.golden"
    $ pure (Text.encodeUtf8 (Text.fromStrict (pepperCompoundTarget False t)))
