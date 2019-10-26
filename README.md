# Payload

Payload is an HTTP server library for PureScript inspired by [Rust's Rocket](https://rocket.rs/) and [Haskell's Servant](https://haskell-servant.readthedocs.io/en/stable/). Here is a complete Payload application:

```purescript
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

api = { getMessages }

getMessages :: { id :: Int, limit :: Int } -> Aff (Array Message)
getMessages { id, limit } = pure
  [{ id: 1, text: "Hey there"}, { id: 2, text: "Limit " <> show limit }]

main = Payload.launch spec api
```

The basic idea: write one API spec. Write handlers as functions returning data. Get for free:

* Server routing
* Decoding URL parameters, query parameters, request bodies into typed values
* Encoding server responses
* Client functions for calling the API

It's like [OpenAPI/Swagger](https://swagger.io/), but without the boilerplate and monstrous code generation. If your handlers don't match your spec, the code won't compile, so the server stays in sync with the spec.

[The above example](./examples/hello/Main.purs) and more can be found in the [examples directory](./examples).

The library is in flux and will likely have API breaking changes.

## Table of Contents

* [Getting Started](#getting-started)
* [Guide](#guide)
  * [Overview](#overview)
  * [Requests](#requests)
  * [Responses](#responses)
  * [Guards](#guards)
* [API Documentation](#api-documentation)
* [Examples](#examples)
* [Building](#building)

# Getting Started

Install Payload:

```
bower install purescript-payload
```

Then copy the above example, [peek at other examples](./examples), or read the docs below.

# Guide

## Overview

Here is a simple spec:

```purescript
spec :: Spec {
  getUser :: GET "/users/<id>" {
    params :: { id :: Int },
    response :: User
  }
}
spec = Spec

type User =
  { id :: Int
  , name :: String }

```

`GET "/users/<id>"` is a type-level string that says we have a `GET` endpoint with a URL parameter named `id`. The type of `id` is defined below it as an `Int`. The endpoint returns a `User`, with type as defined below.

When the Payload server is run, you provide handlers corresponding to each endpoint defined in the API spec. For the above example, you might run it like so:

```purescript
api = { getUser: getUser }

getUser :: { id :: Int } -> Aff User
getUser { id, limit } = pure { id: 1, name: "whodunnit"}

main = Payload.launch spec api
```

The name of the endpoint in the spec, `getMessages`, must correspond to the name of the handler provided in the record of handlers. Notice that the handler is just a function, taking in a Record with an `id` field of type `Int`. URL parameters, query parameters, and bodies are automatically decoded into typed values. The returned `User` value is also automatically encoded to JSON.

Specs can also be hierarchical, as you can see in [this larger example](./examples/movies/Main.purs).

## Requests

The route you define in the API spec tells what must be true about a request for the route's handler to be called. Based on your route spec, Payload will automatically validate:

* Method and path
* Typed URL parameters
* Typed request body
* Typed query strings
* User-defined validations ("guards")

## Responses

Server handlers can return any response that implements `ToSpecResponse` and `EncodeResponse`, including arbitrary JSON responses via the [purescript-simple-json library](https://github.com/justinwoo/purescript-simple-json). Returning any of the following types will return status 200 with appropriate headers added:

* `Empty` (empty body)
* String
* Stream
* Array (returns JSON)
* Record (returns JSON)
* `Json` (returns JSON)

Payload validates at compile time that handler responses match the type you specified in your API spec. Responses match by either being exactly the type in the spec, or by being convertable to that type via the `ToSpecResponse` type class.

What other responses can be converted to spec responses?

To modify the status or headers, handlers can return a `Response`, which is a wrapper around a record with `status` and `headers` fields. There are various helpers in the `Response` module for creating and modifying responses.

To return an error, handlers can return `Either error val`, where `error` can be any encodable value. By default any arbitrary encodable value can be returned as an error with status 500 Internal Server Error, or arbitrary responses can be returned with the `Response` type. Error responses are not represented in the API spec and do not need to match the spec.

## Guards

Payload has a concept of request guards borrowed from the [Rust Rocket library](https://rocket.rs/v0.4/guide/requests/#request-guards). A request guard is a function that is called before the handler is called that returns an arbitrary value that the handler function receives. It can also error or forward, in which case the handler is never called.

For example, this spec defines two guards. The `/admin` endpoint will only be called if an `AdminUser` can be produced by the guard from the request. Similarly the `/user` endpoint will only be called if a `User` can be obtained from the request. This provides a compile-time guarantee that handlers will only be called with authenticated requests.

```purescript
spec :: Spec
  { guards ::
    { user :: User
    , adminUser :: AdminUser }
  , routes ::
    { adminIndex :: GET "/admin"
        { guards :: Guards ("adminUser" : Nil)
        , response :: String }
    , userIndex :: GET "/user"
        { guards :: Guards ("user" : Nil)
        , response :: String }
    , unauthenticatedIndex :: GET "/"
        { response :: String }}}
spec = Spec
```

The request guard itself is defined as a function:

```purescript
getAdminUser :: HTTP.Request -> Aff (Either Resp.Failure AdminUser)
getAdminUser req = do
  headers <- Guards.headers req
  case Headers.lookup "Authorization" headers of
    (Just "Token secret") -> pure (Right (AdminUser { id: 1, name: "John Admin" }))
    _ -> pure (Left (Resp.Forward "Not an admin"))
```

and passed in to the server by starting the server with `startGuarded`:

```purescript
main = do
  let guards = { adminUser: getAdminUser, user: getUser }
  let handlers = { adminIndex, userIndex, unauthenticatedIndex }
  Payload.launch spec { handlers, guards }
```

API specs can also be hierarchical and guards can be applied to all endpoints below them in the hierarchy. For an example of this see the [Movies API Example](./examples/movies/Main.purs)

# API Documentation

[API documentation can be found on Pursuit.](https://pursuit.purescript.org/packages/purescript-payload)

# Examples

[The hello world example](./examples/hello/Main.purs) and more can be found in the [examples directory](./examples). The examples are built and run as part of the project tests.

# Building

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
