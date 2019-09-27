module RatioFinder where

import Prelude
import Data.Array ((:))
import Data.Array (foldl) as Array
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Foldable (product) as Foldable
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map (empty, insert, lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set (fromFoldable, insert, member) as Set
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

data Edge = Edge Factor LengthUnit

derive instance genericEdge :: Generic Edge _
derive instance eqEdge :: Eq Edge
instance showEdge :: Show Edge where
  show = genericShow

data Node = Node LengthUnit (Array Edge)

derive instance eqNode :: Eq Node
derive instance genericNode :: Generic Node _
instance showNode :: Show Node where
  show x = genericShow x

addToNodeMap :: Map LengthUnit (Array Edge) -> Conversion -> Map LengthUnit (Array Edge)
addToNodeMap nodeMap { from, to, factor } = case Map.lookup from nodeMap of
  Nothing -> Map.insert from firstEdge nodeMap
    where
    firstEdge = [ Edge factor to ]
  Just edges -> Map.insert from newEdges nodeMap
    where
    newEdges = (Edge factor to) : edges

allNodes :: Map LengthUnit (Array Edge)
allNodes = Array.foldl addToNodeMap Map.empty givenConversions

depthFirstSearch :: LengthUnit -> LengthUnit -> Either String Factor
depthFirstSearch startingUnit targetUnit = case Map.lookup startingUnit allNodes of
  Nothing -> Left "Unknown starting unit"
  Just firstOptionEdges -> case firstOptionEdges # exploreFirstEdges of
    Nothing -> Left "No path found"
    Just factors -> factors # Foldable.product >>> Right
  where
  exploreFirstEdges :: Array Edge -> Maybe (Array Factor)
  exploreFirstEdges = map (exploreEdge (Set.fromFoldable [ startingUnit ]) []) >>> oneOf

  exploreEdge :: Set LengthUnit -> Array Factor -> Edge -> Maybe (Array Factor)
  exploreEdge visited acc (Edge factor unit)
    | unit == targetUnit = Just (factor : acc)
    | Set.member unit visited = Nothing
    | otherwise =
      Map.lookup unit allNodes
        >>= map (exploreEdge (Set.insert unit visited) (factor : acc))
        >>> oneOf
