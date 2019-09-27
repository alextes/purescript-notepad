module Test.Main where

import Prelude

import Data.Map (lookup, member) as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import RatioFinder (Edge(..), entryNodes)
import Test.Unit (test)
import Test.Unit.Assert (assert, equal) as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main =
  runTest do
    test "builds the graph" do
      Assert.assert "foot in map" $ Map.member "foot" entryNodes
      Assert.equal (Just [ Edge 0.9144 "meter" ]) $ Map.lookup "yard" entryNodes
