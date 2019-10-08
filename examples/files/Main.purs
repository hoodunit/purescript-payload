module Payload.Examples.Files.Main where

import Data.Either (Either)
import Data.List (List)
import Effect.Aff (Aff)
import Payload.Handlers (File)
import Payload.Handlers as Handlers
import Payload.Response (Failure)
import Payload.Spec (API(..), GET)

spec :: API
  { guards :: {}
  , routes ::
    { indexPage :: GET "/"
        { response :: File }
    , public :: GET "/<..path>"
        { params :: { path :: List String }
        , response :: File }
    }
  }
spec = API

indexPage :: forall r. { | r} -> Aff File
indexPage = Handlers.file "examples/files/index.html"

public :: forall r. { path :: List String | r } -> Aff (Either Failure File)
public { path } = Handlers.directory "examples/files/public" path 

api =
  { handlers: { indexPage, public }
  , guards: {} }
