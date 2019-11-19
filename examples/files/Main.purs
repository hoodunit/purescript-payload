module Payload.Examples.Files.Main where

import Data.Either (Either)
import Data.List (List)
import Effect.Aff (Aff)
import Payload.Server.Handlers (File)
import Payload.Server.Handlers as Handlers
import Payload.ResponseTypes (Failure)
import Payload.Spec (Spec(Spec), GET)

spec :: Spec
  { guards :: {}
  , routes ::
    { indexPage :: GET "/"
        { response :: File }
    , public :: GET "/<..path>"
        { params :: { path :: List String }
        , response :: File }
    }
  }
spec = Spec

indexPage :: {} -> Aff File
indexPage = Handlers.file "examples/files/index.html"

public :: { params :: { path :: List String } } -> Aff (Either Failure File)
public { params: {path} } = Handlers.directory "examples/files/public" path 

api =
  { handlers: { indexPage, public }
  , guards: {} }
