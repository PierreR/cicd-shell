{-# LANGUAGE OverloadedLists #-}
module Shell.PepCmdTest where

import           Shell.PepCmd.Utils
import           Shell.Prelude
import           Shell.Type
import           Shell.Target
import qualified Shell.Config as Config


import qualified Data.Text               as Text
import qualified Data.Text.Lazy.Encoding as LText
import qualified System.FilePath         as FilePath
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit


unit_getStack = do
  let config = Config.mockShellConfig ["cicd"]
  sx <- runReaderT (getStacks (Just "bos")) config
  sx @?= ["bos"]

test_compountTarget :: IO TestTree
test_compountTarget = do
  golden_files <- findByExtension [".golden"] "tests/Shell"
  pure $ testGroup "pepper compound target"
   [ goldenVsString fname golden_file
       $ pure (LText.encodeUtf8 (fromStrict (pepperCompoundTarget False target )))
   | golden_file <- golden_files
   , let fname = FilePath.takeBaseName golden_file
         Just target = readTarget (toS fname)
   ]


--
-- UTILS
--

-- testing the readTarget test utils ...
unit_readMinimalTarget :: IO ()
unit_readMinimalTarget =
  let expectation = Target Nothing ["cicd"] Nothing Nothing Nothing "dev"
  in readTarget "[cicd].dev" @?= Just expectation

unit_readSimpleTarget :: IO ()
unit_readSimpleTarget =
  let expectation = Target Nothing ["cicd"] Nothing (Just (Role (Just (Subgroup "salt")) "master")) Nothing "dev"
  in readTarget "[cicd].salt.master.dev" @?= Just expectation

unit_readDefHostgroupsTarget :: IO ()
unit_readDefHostgroupsTarget =
  let expectation = Target Nothing ["cicd", "ci"] Nothing (Just (Role (Just (Subgroup "salt")) "master")) Nothing "dev"
  in readTarget "[cicd,ci].salt.master.dev" @?= Just expectation

-- | Text reader for Target
-- Expect the following pattern:
-- $hostgroup.$subgroup.$role.$zone
-- $hostgroup.$role.$zone
readTarget :: Text -> Maybe Target
readTarget s =
  case Text.splitOn "." s of
    [h,g,r,z] ->
      mk_target (Just g) (Just r) z <$> read_hostgroup h
    [h,r,z]   ->
      mk_target Nothing (Just r) z <$> read_hostgroup h
    [h,z]   ->
      mk_target Nothing Nothing z <$> read_hostgroup h
    _         -> Nothing
  where
    mk_target (Just g) (Just r) z h = Target Nothing h Nothing (Just (Role (Just (Subgroup g)) r)) Nothing z
    mk_target Nothing (Just r) z h = Target Nothing h Nothing (Just (Role Nothing r)) Nothing z
    mk_target Nothing Nothing z h = Target Nothing h Nothing Nothing Nothing z
    mk_target (Just _) Nothing _ _ = panic "Can't pass a subgroup without a role in this (local) context"

    read_hostgroup :: Text -> Maybe (NonEmpty Text)
    read_hostgroup s = do
      s' <- Text.stripPrefix "[" s
      s'' <- Text.stripSuffix "]" s'
      pure $ fromList $ Text.splitOn "," s''
