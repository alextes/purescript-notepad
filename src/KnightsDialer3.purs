module KnightsDialer3 where

import Prelude
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set

infix 5 Set.difference as \\

type DialNum
  = Int

type Neighbors
  = (Set DialNum)

getNeighbors :: DialNum -> Neighbors
getNeighbors num = case num of
  1 -> Set.fromFoldable [ 8, 6 ]
  2 -> Set.fromFoldable [ 7, 9 ]
  3 -> Set.fromFoldable [ 4, 8 ]
  4 -> Set.fromFoldable [ 3, 9, 0 ]
  5 -> Set.empty
  6 -> Set.fromFoldable [ 1, 7, 0 ]
  7 -> Set.fromFoldable [ 2, 6 ]
  8 -> Set.fromFoldable [ 1, 3 ]
  9 -> Set.fromFoldable [ 4, 2 ]
  0 -> Set.fromFoldable [ 4, 6 ]
  _ -> Set.empty

countDistinctNums :: DialNum -> Int -> Int
countDistinctNums startingPosition hops = Set.size $ getDistinctNums startingPosition hops

getDistinctNums :: DialNum -> Int -> Set DialNum
getDistinctNums startingPosition hops = getDistinctNums' startingPosition hops (Set.singleton startingPosition)

getDistinctNums' :: DialNum -> Int -> Set DialNum -> Set DialNum
getDistinctNums' startingPosition 0 seenDialNums = seenDialNums
getDistinctNums' startingPosition hops seenDialNums =
  let
    neighbors = getNeighbors startingPosition
    unseenNeighbors = neighbors \\ seenDialNums
    nextUnseenNum = unseenNeighbors # List.fromFoldable >>> List.head
  in
    case nextUnseenNum of
      Nothing -> seenDialNums
      Just unseenNum -> getDistinctNums' unseenNum (hops - 1) (Set.insert unseenNum seenDialNums)
