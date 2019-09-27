module KnightsDialer4 where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))

type DialNum
  = Int

getNeighbors :: DialNum -> Array DialNum
getNeighbors num = case num of
  1 -> [ 8, 6 ]
  2 -> [ 7, 9 ]
  3 -> [ 4, 8 ]
  4 -> [ 3, 9, 0 ]
  5 -> []
  6 -> [ 1, 7, 0 ]
  7 -> [ 2, 6 ]
  8 -> [ 1, 3 ]
  9 -> [ 4, 2 ]
  0 -> [ 4, 6 ]
  _ -> []

getDistinctNums :: DialNum -> Int -> Array (Array DialNum)
getDistinctNums num hops = go hops [ [ num ] ]
  where
  go hopsLeft nums =
    if hopsLeft <= 0 then
      nums
    else
      go (hopsLeft - 1) (nums >>= step)

  step :: Array DialNum -> Array (Array DialNum)
  step nums = case Array.last nums of
    Nothing -> []
    Just n -> Array.snoc nums <$> getNeighbors n
