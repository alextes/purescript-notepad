module KnightsDialer2 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), (:))
import Data.List (filter) as List
import Data.Set (Set)
import Data.Set as Set

data DialNum
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Zero

derive instance eqDialNum :: Eq DialNum

derive instance ordDialNum :: Ord DialNum

derive instance genericDialNum :: Generic DialNum _

instance showDialNum :: Show DialNum where
  show = genericShow

type Neighbors
  = (List DialNum)

getNeighbors :: DialNum -> Neighbors
getNeighbors num = case num of
  One -> Eight : Six : Nil
  Two -> Seven : Nine : Nil
  Three -> Four : Eight : Nil
  Four -> Three : Nine : Zero : Nil
  Five -> Nil
  Six -> One : Seven : Zero : Nil
  Seven -> Two : Six : Nil
  Eight -> One : Three : Nil
  Nine -> Four : Two : Nil
  Zero -> Four : Six : Nil

getDistinctNums :: DialNum -> Int -> Set DialNum
getDistinctNums num hops = getDistinctNums' num hops (Set.singleton num)

getDistinctNums' :: DialNum -> Int -> Set DialNum -> Set DialNum
getDistinctNums' startingPosition 0 seenDialNums = seenDialNums

getDistinctNums' startingPosition hops seenDialNums = case getNeighbors startingPosition of
  Nil -> seenDialNums
  unseenNums -> case List.filter isUnseenNum unseenNums of
    Nil -> seenDialNums
    unseenNum : _ -> getDistinctNums' unseenNum (hops - 1) (Set.insert unseenNum seenDialNums)
    where
    isUnseenNum num = not $ Set.member num seenDialNums
