module QueryStringSolution where

import Prelude
import Data.Array (catMaybes, find) as Array
import Data.Either (Either(..))
import Data.Either (hush) as Either
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe) as Maybe
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple (fst, snd) as Tuple
import Effect (Effect)
import Effect.Console (log, logShow) as Console

type Person
  = { name :: String, city :: String, age :: Int }

people :: Map String Person
people =
  Map.fromFoldable
    [ (Tuple "alex" { name: "alex", city: "amsterdam", age: 27 })
    , (Tuple "tijn" { name: "tijn", city: "utrecht", age: 27 })
    ]

type ParamKey
  = String

type ParamValue
  = Maybe String

type ParamPair
  = Tuple ParamKey ParamValue

exampleQueryString :: String
exampleQueryString = "?name=alex&age=27"

stripLeadingChar :: String -> String -> String
stripLeadingChar char string = Maybe.fromMaybe string mStrippedString
  where
  mStrippedString = String.stripPrefix (Pattern char) string

data SplitResult
  = Pair String String
  | NoPair
  | KeyOnly String
  | ValueOnly String
  | MultipleValues (Array String)

derive instance genericSplitResult :: Generic SplitResult _

instance showSplitResult :: Show SplitResult where
  show = genericShow

-- Here the safe parsing logic starts
pairFromCharSplit :: String -> String -> SplitResult
pairFromCharSplit char str = case String.split (Pattern char) str of
  [] -> NoPair
  [ "", "" ] -> NoPair
  [ key, "" ] -> KeyOnly key
  [ "", value ] -> ValueOnly value
  [ key, value ] -> Pair key value
  splitValues -> MultipleValues splitValues

parseQueryString :: String -> Array (Either String ParamPair)
parseQueryString =
  stripLeadingChar "?"
    >>> String.split (Pattern "&")
    >>> map (pairFromCharSplit "=")
    >>> map case _ of
        NoPair -> Left "No pair"
        ValueOnly _ -> Left "No key"
        KeyOnly key -> Right $ Tuple key Nothing
        Pair key value -> Right $ Tuple key (Just value)
        MultipleValues _ -> Left "Multiple values, ambiguous query parameter"
-- Here the safe parsing logic ends
getParamByKey :: ParamKey -> Array ParamPair -> ParamValue
getParamByKey key = Array.find (Tuple.fst >>> (==) key) >=> Tuple.snd

main :: Effect Unit
main = do
  let
    params = parseQueryString exampleQueryString

    cleanParams = Array.catMaybes $ map Either.hush (params)
  Console.logShow cleanParams
  case getParamByKey "name" cleanParams of
    Nothing -> Console.log "No name, no city"
    Just name -> case Map.lookup name people of
      Nothing -> Console.log $ "No person by name " <> name
      Just person -> Console.log $ "Alex lives in " <> person.city
