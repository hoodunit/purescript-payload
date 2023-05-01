{ name = "payload"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "affjax-node"
  , "arraybuffer-types"
  , "arrays"
  , "bifunctors"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "http-methods"
  , "integers"
  , "js-date"
  , "js-promise-aff"
  , "lists"
  , "maybe"
  , "media-types"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-http"
  , "node-path"
  , "node-streams"
  , "node-url"
  , "nullable"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "refs"
  , "simple-json"
  , "strings"
  , "stringutils"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-prelude"
  , "unfoldable"
  , "unsafe-coerce"
  , "web-streams"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "examples/**/*.purs" ]
, license = "Apache-2.0"
, repository = "https://github.com/hoodunit/purescript-payload"
}
