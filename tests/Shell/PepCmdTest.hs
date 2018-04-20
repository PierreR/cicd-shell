{-# LANGUAGE OverloadedLists #-}
module Shell.PepCmdTest where

import           Shell.PepCmd.Utils
import           Shell.Prelude
import           Shell.Type

import qualified Data.Text.Lazy          as Text
import qualified Data.Text.Lazy.Encoding as Text
import qualified System.FilePath         as FilePath
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

unit_readMinimalTarget :: IO ()
unit_readMinimalTarget =
  let expectation = Target Nothing ["cicd"] Nothing Nothing "dev"
  in readTarget "[cicd].dev" @?= Just expectation

unit_readSimpleTarget :: IO ()
unit_readSimpleTarget =
  let expectation = Target Nothing ["cicd"] Nothing (Just (Role (Just (Subgroup "salt")) "master")) "dev"
  in readTarget "[cicd].salt.master.dev" @?= Just expectation

unit_readMultipleHostgroupTarget :: IO ()
unit_readMultipleHostgroupTarget =
  let expectation = Target Nothing ["cicd", "ci"] Nothing (Just (Role (Just (Subgroup "salt")) "master")) "dev"
  in readTarget "[cicd,ci].salt.master.dev" @?= Just expectation

test_compountTarget :: IO TestTree
test_compountTarget = do
  golden_files <- findByExtension [".golden"] "tests/Shell"
  pure $ testGroup "pepper compound target"
   [ goldenVsString fname golden_file
       $ pure (Text.encodeUtf8 (Text.fromStrict (pepperCompoundTarget False target )))
   | golden_file <- golden_files
   , let fname = FilePath.takeBaseName golden_file
         Just target = readTarget (toS fname)
   ]
