module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Discovery (discover)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Reporter.TeamCity (teamcityReporter)

main :: Effect Unit
main = launchAff_ do
  specs <- discover """Test\..*"""
  runSpec [teamcityReporter] specs