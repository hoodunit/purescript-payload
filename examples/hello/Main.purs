module Payload.Examples.Hello.Main where

import Prelude

import Payload.Routable (API(..))
import Payload.Spec (GET)
import Payload.Server as Payload

type Message = 
  { id :: Int
  , text :: String }

api :: API {
  getMessages :: GET "/user/<id>/messages?limit=<limit>" {
    params :: { id :: Int },
    query :: { limit :: Int },
    response :: Array Message
  }
}
api = API

getMessages { id, limit } = pure
  [{ id: 1, text: "Hey there"}, { id: 2, text: "Limit " <> show limit }]

main = Payload.start_ api { getMessages }
