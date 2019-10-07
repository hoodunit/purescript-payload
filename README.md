# Payload

Payload is an HTTP server library for PureScript. Here is a complete Payload application:

```
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

You tell what endpoints you have and what data they take in and return by writing an API spec. You write handlers as functions returning data. Payload decodes request parameters into typed values and hands them to your function. It takes your typed responses and encodes them for the network. It checks at compile-time that your handlers match the spec.

Example applications, including the above, can be found in the [examples directory](./examples).

Payload draws heavy inspiration from the [Rocket (Rust)](https://rocket.rs/), [Servant (Haskell)](https://haskell-servant.readthedocs.io/en/stable/), and [purescript-quickserve](https://github.com/paf31/purescript-quickserve) projects.

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