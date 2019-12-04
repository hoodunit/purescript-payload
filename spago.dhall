{ name = "payload"
, dependencies =
    [ "aff"
    , "affjax"
    , "console"
    , "debug"
    , "effect"
    , "foreign-generic"
    , "node-fs"
    , "node-fs-aff"
    , "node-http"
    , "prelude"
    , "psci-support"
    , "record"
    , "simple-json"
    , "stringutils"
    , "test-unit"
    , "typelevel-prelude"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "examples/**/*.purs" ]
, license = "Apache-2.0"
, repository = "https://github.com/hoodunit/purescript-payload"
}
