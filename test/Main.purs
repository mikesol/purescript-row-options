module Test.Main where

import Prelude

import Data.Row.Options (RowOptions, modify, options)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Proxy (Proxy(..))

type AllOpts = RowOptions (foo :: Int, bar :: String)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "options" do
      it "correctly modifies something" do
        modify (Proxy :: _ "foo") show (options {foo:1} :: AllOpts) `shouldEqual` (options {foo:"1"})
      it "correctly skips modification" do
        modify (Proxy :: _ "foo") show (options {bar:"hello"} :: AllOpts) `shouldEqual` (options {bar:"hello"})
