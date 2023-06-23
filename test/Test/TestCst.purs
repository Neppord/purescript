module Test.TestCst where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.Function ((#))

spec :: Spec Unit
spec = describe "Cst" do
    it "Works" do
        true # shouldEqual true