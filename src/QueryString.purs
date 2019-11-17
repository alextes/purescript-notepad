module QueryString where

import Prelude
import Effect (Effect)
import Effect.Console (log) as Console

exampleQueryString :: String
exampleQueryString = "name=alex&age=27"

main :: Effect Unit
main = Console.log "Hi There!"
