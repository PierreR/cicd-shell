{-# LANGUAGE OverloadedLists #-}
module Shell.PepCmdTest where

import           Shell.Prelude
import           Shell.PepCmd.Utils
import           Shell.Type

import           Test.Tasty.HUnit


unit_compountTarget :: IO ()
unit_compountTarget =
  let t = defTarget ["cicd"] & zone .~ "dev"
      expectation = "pepper -C \"G@zone:dev and ( G@hostgroup:cicd )\" "
  in pepperCompoundTarget False t @?= expectation
