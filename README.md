# Payload

Payload is an HTTP server library for PureScript inspired by [Rust's Rocket](https://rocket.rs/) and [Haskell's Servant](https://haskell-servant.readthedocs.io/en/stable/). Here is a complete Payload application:

```purescript
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
```

You tell what endpoints you have and what data they take in and return by writing a type-level API spec. You write handlers as functions returning data. Based on your API spec, Payload will decode request parameters into typed values and hand them to your functions. It takes your typed responses and encodes them based on the types in the spec for the network. It checks at compile-time that your handlers match the spec.

More examples can be found in the [examples directory](./examples).

## Overview

In Payload applications you write an API *spec* telling what endpoints your API supports and then pass your server handlers as functions corresponding to the spec. A simple spec might look like this:

```purescript
type User =
  { id :: Int
  , name :: String }

spec :: API {
  getUser :: GET "/users/<id>" {
    params :: { id :: Int },
    response :: User
  }
}
spec = API
```

`GET "/users/<id>"` is a type-level string that says we have a `GET` endpoint with a URL parameter named `id`. The type of `id` is defined below it as an `Int`. The endpoint returns a `User`, with type as defined above.

When the Payload server is run, you provide handlers corresponding to each endpoint defined in the API spec. For the above example, you might run it like so:

```purescript
api = { getUser: getUser }

getUser :: { id :: Int } -> Aff User
getUser { id, limit } = pure { id: 1, name: "whodunnit"}

main = Payload.launch spec api
```

The name of the endpoint in the spec, `getMessages`, must correspond to the name of the handler provided in the record of handlers. Notice that the handler is just a function, taking in a Record with an `id` field of type `Int`. URL parameters, query parameters, and bodies are automatically decoded into typed values. The returned `User` value is also automatically encoded to JSON.

## Building

* Install Node.js.
* Install project dependencies:

```
npm install
npx --no-install bower update --force-latest
```

Build library:

```
npm run build
```

Run tests:

```
npm test
```
