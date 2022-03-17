module Test.Main where

import Prelude

import Data.Array (fromFoldable)
import Data.Homogeneous.Row.Options (homogeneous)
import Data.Maybe (Maybe(..))
import Data.Row.Options (RowOptions, set, feelingLucky, megamap, modify, options)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Heterogeneous.Mapping (hmap)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Proxy (Proxy(..))

type AllOpts = RowOptions (foo :: Int, bar :: String)
type AllOptsI = RowOptions (foo :: Int, bar :: Int)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "options" do
    it "correctly modifies something" do
      modify (Proxy :: _ "foo") show (options { foo: 1 } :: AllOpts) `shouldEqual` (options { foo: "1" })
    it "correctly skips modification" do
      modify (Proxy :: _ "foo") show (options { bar: "hello" } :: AllOpts) `shouldEqual` (options { bar: "hello" })
    it "correctly sets" do
      set (Proxy :: _ "foo") 42 (options { bar: "hello" } :: AllOpts) `shouldEqual` (options { foo: 42, bar: "hello" })
    it "correctly applies megamap 1" do
      megamap (options { foo: 1 }) { foo: show :: Int -> _, bar: identity :: String -> _ } `shouldEqual` (options { foo: "1" })
    it "correctly applies megamap 2" do
      megamap (options { foo: 1, bar: "bar" }) { foo: show :: Int -> _, bar: identity :: String -> _ } `shouldEqual` (options { foo: "1", bar: "bar" })
    it "correctly homogenifies 1" do
      fromFoldable (homogeneous (megamap (options { foo: 1, bar: "bar" }) { foo: show :: Int -> _, bar: identity :: String -> _ })) `shouldEqual` ([ "1", "bar" ])
    it "correctly homogenifies 2" do
      fromFoldable (homogeneous (megamap (options { foo: 1 }) { foo: show :: Int -> _, bar: identity :: String -> _ })) `shouldEqual` ([ "1" ])
    it "correctly hmaps" do
      hmap (add 1) (options { foo: 1 } :: AllOptsI) `shouldEqual` (options { foo: 2 })
    it "feels lucky" do
      feelingLucky (options { foo: 1 } :: AllOpts) `shouldEqual` (Just { foo: 1 })
    it "feels lucky" do
      feelingLucky (options { bar: "hi" } :: AllOpts) `shouldEqual` (Nothing :: Maybe { foo :: Int })
