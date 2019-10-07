module Payload.Examples.Hello.Main where

import Prelude
import Effect.Aff (Aff)
import Payload.Server as Payload
import Payload.Spec (API(API), GET)

type Message = 
  { id :: Int
  , text :: String }

spec :: API {
  getMessages :: GET "/users/<id>/messages?limit=<limit>" {
    params :: { id :: Int },
    query :: { limit :: Int },
    response :: Array Message
  }
}
spec = API

api = { getMessages }

getMessages :: { id :: Int, limit :: Int } -> Aff (Array Message)
getMessages { id, limit } = pure
  [{ id: 1, text: "Hey there"}, { id: 2, text: "Limit " <> show limit }]

main = Payload.launch spec api
