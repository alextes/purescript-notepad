module RatioFinder2 where

import Prelude
import Data.Array ((:))
import Data.Array (foldl, null) as Array
import Data.Either (Either(..))
import Data.Foldable (oneOf, product) as Foldable
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map (empty, insert, lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (new) as Ref

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

allNodes :: Map LengthUnit Node
allNodes = ?graphbuildingwat

addNodeToMap :: Map LengthUnit Node -> Conversion -> Map LengthUnit Node
addNodeToMap nodeMap { from, to, factor } = do
  toNode <- case Map.lookup to nodeMap of
    Nothing -> Ref.new $ Node to []
    Just node -> Ref.new node
  fromNode <- case Map.lookup from nodeMap of
    Nothing -> pure $ Node from [ Tuple factor toNode ] :: Effect Node
    Just (Node unit neighors) -> pure $ Node unit $ (Tuple factor toNode) : neighors
  pure $ Map.insert from fromNode nodeMap

-- Doesn't deal with loops yet, assumes acyclical graph
depthFirstSearch :: LengthUnit -> LengthUnit -> Either String Factor
depthFirstSearch startingUnit targetUnit = case Map.lookup startingUnit allNodes of
  Nothing -> Left $ "No node of unit " <> startingUnit <> " in graph"
  Just startingNode -> case exploreNode [ 1.0 ] startingNode of
    Nothing -> Left "No conversion path found"
    Just factors -> factors # Foldable.product >>> Right
    where
    exploreNode :: Array Factor -> Node -> Maybe (Array Factor)
    exploreNode factors (Node unit neighbors)
      -- we found the target!
      | unit == targetUnit = factors # Just
      -- node is not the target, and there are no further neighbors to explore.
      | Array.null neighbors = Nothing
      -- we keep searching
      | otherwise = Foldable.oneOf $ map (\(Tuple factor neighbor) -> exploreNode (factor : factors) neighbor) neighbors
