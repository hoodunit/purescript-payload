module Payload.Examples.Transformer.Test where

import Prelude

import Control.Monad.Reader.Trans (runReaderT)
import Payload.Examples.Transformer.Main (guards, handlers, spec)
import Payload.Headers (empty, set) as Headers
import Payload.Test.Helpers (respMatches, withServer')
import Payload.Test.Helpers as Helpers
import Simple.JSON (writeJSON)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

env :: { adminKey :: String, dbConnectionString :: String }
env =
  { adminKey: "foo"
  , dbConnectionString: "bar"
  }

tests :: TestSuite
tests = do
  let withApi = withServer' (flip runReaderT env) spec { guards, handlers }
  suite "Transformer" do
    let host = "http://localhost:3000"
        path = "/env"
    test ("GET " <> path <> " with admin key succeeds") $ withApi do
      res <- Helpers.get_
               host
               path
               $ Headers.empty # Headers.set "x-admin-key" "foo"
      respMatches
        { status: 200
        , body: writeJSON env
        }
        res
    test ("GET " <> path <> " with invalid x-admin-key header fails with 401 Unauthorized") $ withApi do
      res <- Helpers.get_
               host
               path
               $ Headers.empty # Headers.set "x-admin-key" "xx"
      Assert.equal 401 res.status
    test ("GET " <> path <> " without x-admin-key header fails with 401 Unauthorized") $ withApi do
      res <- (Helpers.request host).get path
      Assert.equal 401 res.status
