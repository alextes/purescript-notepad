module KnightsDialer3 where

import Prelude
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set as Set

infix 5 Set.difference as \\

type DialNum
  = Int

type Neighbors
  = (List DialNum)

getNeighbors :: DialNum -> Neighbors
getNeighbors num = case num of
  1 -> 8 : 6 : Nil
  2 -> 7 : 9 : Nil
  3 -> 4 : 8 : Nil
  4 -> 3 : 9 : 0 : Nil
  5 -> Nil
  6 -> 1 : 7 : 0 : Nil
  7 -> 2 : 6 : Nil
  8 -> 1 : 3 : Nil
  9 -> 4 : 2 : Nil
  0 -> 4 : 6 : Nil
  _ -> Nil

countDistinctNums :: DialNum -> Int -> Int
countDistinctNums startingPosition hops = List.length $ getDistinctNums startingPosition hops

getDistinctNums :: DialNum -> Int -> List (List DialNum)
getDistinctNums startingPosition hops = getDistinctNums' hops ((startingPosition : Nil) : Nil)

getDistinctNums' :: Int -> List (List DialNum) -> List (List DialNum)
getDistinctNums' 0 sequences = sequences

getDistinctNums' hops sequences =
  getDistinctNums' (hops - 1)
    ( sequences
        >>= \sequence -> case List.last sequence of
            Nothing -> Nil
            Just num -> map (List.snoc sequence) (getNeighbors num)
    )
