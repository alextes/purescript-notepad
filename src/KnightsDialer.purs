module KnightsDialer where

import Prelude
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type Button
  = Maybe Int

board :: Array (Array Button)
board =
  [ [ Just 1, Just 2, Just 3 ]
  , [ Just 4, Just 5, Just 6 ]
  , [ Just 7, Just 8, Just 9 ]
  , [ Nothing, Just 0, Nothing ]
  ]

type Column
  = Int

type Row
  = Int

type Position
  = Tuple Row Column

-- right :: Position -> Maybe Position
-- right (Tuple row column) = case row, column of
--   0, 0 -> Just $ Tuple row (column + 1)
--   1, _ -> if column > 2 then Nothing else Just $ Tuple
--   2, _ -> if column > 2 then Nothing else Just $ Tuple
--   3, _ -> Nothing
  -- _, _ -> Nothing
  -- Just $ Tuple row (column + 1)


getNumFromPosition :: Position -> Button
getNumFromPosition (Tuple row column) = do
  mRow <- board !! row
  (mRow !! column) >>= identity

-- getNextOptions :: Position -> Array Position
-- getNextOptions Tuple row column = case row, column of
--   2, 0 -> [ Tuple 2 1, Tuple 1 2 ]

-- data DialNum = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Zero
-- getNext :: Int -> Array Int
-- getNext One = [num + 1 + 1 + 3, num + 3 + 3 + 1]
distinctNums :: Array Int
distinctNums = []

getDistinctNums :: Int -> Int -> Int
getDistinctNums startingPosition hops = hops + 1
