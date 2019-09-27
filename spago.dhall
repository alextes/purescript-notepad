{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "argonaut"
    , "assert"
    , "console"
    , "debug"
    , "effect"
    , "generics-rep"
    , "numbers"
    , "ordered-collections"
    , "psci-support"
    , "test-unit"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
