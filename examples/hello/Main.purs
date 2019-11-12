module Payload.Examples.Hello.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Payload.Server as Payload
import Payload.Spec (Spec(Spec), GET)

type Message = 
  { id :: Int
  , text :: String }

spec :: Spec {
  getMessages :: GET "/users/<id>/messages?limit=<limit>" {
    params :: { id :: Int },
    query :: { limit :: Int },
    response :: Array Message
  }
}
spec = Spec

getMessages :: forall r. { params :: { id :: Int }, query :: { limit :: Int } | r } -> Aff (Array Message)
getMessages {params: {id}, query: {limit}} = pure
  [{ id: 1, text: "Hey there"}, { id: 2, text: "Limit " <> show limit }]

handlers = { getMessages }

main :: Effect Unit
main = Payload.launch spec handlers
