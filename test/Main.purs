module Test.Main where

import Prelude

import Data.List (List(..), (:))
import Data.Set (fromFoldable) as Set
import Effect (Effect)
import KnightsDialer2 (DialNum(..), getDistinctNums)
import Test.Assert (assert)

main :: Effect Unit
main = do
  assert $ (getDistinctNums One 1) == Set.fromFoldable (One : Six : Nil)

