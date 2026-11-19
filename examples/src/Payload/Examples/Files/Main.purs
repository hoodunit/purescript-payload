module Payload.Examples.Files.Main where

import Data.Either (Either)
import Data.List (List)
import Effect.Aff (Aff)
import Node.Stream (Read, Stream)
import Payload.ResponseTypes (Failure, Response(..))
import Payload.Server.Handlers as Handlers
import Payload.Spec (Spec(Spec), GET)

spec :: Spec
  { guards :: {}
  , routes ::
    { indexPage :: GET "/"
        { response :: Stream (read :: Read) }
    , public :: GET "/<..path>"
        { params :: { path :: List String }
        , response :: Stream (read :: Read) }
    }
  }
spec = Spec

indexPage :: {} -> Aff (Either Failure (Response (Stream (read :: Read))))
indexPage _ = Handlers.file "examples/files/index.html"

public :: { params :: { path :: List String } } -> Aff (Either Failure (Response (Stream (read :: Read))))
public { params: {path} } = Handlers.directory "examples/files/public" path 

api ::
    { guards :: Record ()
    , handlers ::
        { indexPage :: {} -> Aff (Either Failure (Response (Stream (read :: Read))))
        , public :: { params :: { path :: List String } } -> Aff (Either Failure (Response (Stream (read :: Read))))        }
    }
api =
  { handlers: { indexPage, public }
  , guards: {} }
