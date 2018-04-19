{-# LANGUAGE OverloadedLists #-}
module Shell.PepCmdTest where

import           Shell.Prelude
import           Shell.PepCmd.Utils
import           Shell.Type

import           Test.Tasty.HUnit


unit_compountTarget :: IO ()
unit_compountTarget =
  let Just t = readTarget "cicd.salt.master.dev"
      expectation = "pepper -C \"G@zone:dev and ( G@hostgroup:cicd ) and G@subgroup:salt and G@role:master\" "
  in pepperCompoundTarget False t @?= expectation
