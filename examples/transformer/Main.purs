module Payload.Examples.Transformer.Main where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, ask, asks, runReaderT)
import Data.Either (Either, note)
import Data.Foldable (find)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Node.HTTP as HTTP
import Payload.Headers as Headers
import Payload.ResponseTypes (Response)
import Payload.Server as Payload
import Payload.Server.Guards as Guards
import Payload.Server.Response as Resp
import Payload.Spec (type (:), Nil, GET, Guards, Spec(Spec))

type Env = 
  { adminKey :: String
  , dbConnectionString :: String
  }

spec :: Spec
  { guards ::
    { adminKeyMatch :: Unit
    }
  , routes ::
    { env :: GET "/env" { guards :: Guards ("adminKeyMatch" : Nil), response :: Env }
    }
  }
spec = Spec

guards ::
  { adminKeyMatch ::
      HTTP.Request -> ReaderT Env Aff (Either (Response String) Unit)
  }
guards =
  { adminKeyMatch: \request -> do
      expected <- asks _.adminKey
      headers <- liftAff $ Guards.headers request
      let provided = find (\x -> x == expected) $ Headers.lookup "x-admin-key" headers
      pure $ void $ note (Resp.unauthorized $ "\"x-admin-key\" header must match configured secret key (\"" <> expected <> "\")") provided
  }

handlers ::
  { env ::
    { guards :: { adminKeyMatch :: Unit } } -> ReaderT Env Aff Env
  }
handlers = { env: const ask }

main :: Effect Unit
main =
  launchAff_ $
    Payload.startGuarded'
      (flip runReaderT { adminKey: "secret", dbConnectionString: "postgresql://postgres@localhost/postgres" })
      Payload.defaultOpts
      spec
      { guards, handlers }
