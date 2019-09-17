module Main where

import Prelude

import Data.Argonaut as Ar
import Data.Either (Either, either)
import Effect (Effect)
import Effect.Console (log)

serializedPerson :: String
serializedPerson = """{ "name": "alex", "age": 26 }"""

decodePerson :: String -> Either String Person
decodePerson jsonString = Ar.jsonParser jsonString >>= Ar.decodeJson

type Person = { age :: Int , name :: String }

me :: Person
me = { name: "alex", age: 26 }

main :: Effect Unit
main =
  let
    maybeMe = decodePerson serializedPerson
  in
    either log log (show <$> maybeMe)
