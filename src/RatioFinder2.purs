module RatioFinder2 where

import Prelude

import Data.Array ((:))
import Data.Array (foldl) as Array
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map (empty, insert, lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- Given a list of conversion rates (formatted in the language of your choice) as a collection of origin unit, destination, and multiplier, for example:
--
-- foot inch 12
-- foot yard 0.333
-- etc..
--
-- Such that ORIGIN * MULTIPLIER = DESTINATION, design an algorithm that takes two arbitrary unit values and returns the conversion rate between them.
type LengthUnit
  = String

type Factor
  = Number

type Conversion
  = { from :: String
    , to :: String
    , factor :: Factor
    }

givenConversions :: Array Conversion
givenConversions =
  [ { from: "foot"
    , to: "inch"
    , factor: 12.0
    }
  , { from: "foot"
    , to: "yard"
    , factor: 1.0 / 3.0 / 4.0
    }
  , { from: "yard"
    , to: "meter"
    , factor: 0.9144
    }
  , { from: "inch"
    , to: "foot"
    , factor: 0.083
    }
  ]

data Node
  = Node LengthUnit (Array (Tuple Factor Node))

derive instance genericNode :: Generic Node _

derive instance eqNode :: Eq Node

instance showNode :: Show Node where
  show x = genericShow x

-- Node1 -> Node2
-- Node1 -> Node3
-- Building graph data is wat.
allNodes :: Map LengthUnit Node
allNodes = Array.foldl addNodeToMap Map.empty givenConversions

addNodeToMap :: Map LengthUnit Node -> Conversion -> Map LengthUnit Node
addNodeToMap nodeMap { from, to, factor } =
  let
    toNode = case Map.lookup to nodeMap of
      Nothing -> Node to []
      Just node -> node

    fromNode = case Map.lookup from nodeMap of
      Nothing -> Node from [ Tuple factor toNode ]
      Just (Node unit neighors) -> Node unit $ (Tuple factor toNode) : neighors
  in
    Map.insert from fromNode nodeMap

depthFirstSearch :: LengthUnit -> LengthUnit -> Either String Factor
depthFirstSearch startingUnit targetUnit = case Map.lookup startingUnit allNodes
