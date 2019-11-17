module Main where

import Prelude
import Data.Argonaut as Ar
import Data.Array as Array
import Data.Either (Either, either)
import Data.Maybe (fromJust) as Maybe
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial) as Partial

serializedPerson :: String
serializedPerson = """{ "name": "alex", "age": 26 }"""

decodePerson :: String -> Either String Person
decodePerson jsonString = Ar.jsonParser jsonString >>= Ar.decodeJson

type Person
  = { age :: Int, name :: String }

me :: Person
me = { name: "alex", age: 26 }

main :: Effect Unit
main =
  let
    maybeMe = decodePerson serializedPerson
  in
    either log log (show <$> maybeMe)

index' :: forall t3. Array t3 -> Int -> t3
index' array position = Partial.unsafePartial $ Maybe.fromJust $ Array.index array position
infixr 5 index' as !!

num :: Int
num = [ 1, 2 ] !! 1
