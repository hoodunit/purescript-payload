# Payload

<a href="https://pursuit.purescript.org/packages/purescript-payload">
  <img src="https://pursuit.purescript.org/packages/purescript-payload/badge"
       alt="purescript-payload on Pursuit">
  </img>
</a>

Payload is an HTTP server library for PureScript inspired by [Rust's Rocket](https://rocket.rs/) and [Haskell's Servant](https://haskell-servant.readthedocs.io/en/stable/).

The basic idea: write one API spec. Write handlers as functions returning data. Get for free:

* Request routing
* Decoding URL parameters, query parameters, and request bodies into typed values
* Encoding typed values into server responses

It's like [OpenAPI/Swagger](https://swagger.io/) without the boilerplate and code generation. Unlike OpenAPI, if your handlers don't match your spec the code won't compile, so servers always stay in sync with the spec.

Here is a complete Payload application:

```purescript
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

getMessages :: { id :: Int, limit :: Int } -> Aff (Array Message)
getMessages { id, limit } = pure
  [{ id: 1, text: "Hey there"}, { id: 2, text: "Limit " <> show limit }]

handlers = { getMessages: getMessages }

main :: Effect Unit
main = Payload.launch spec handlers
```

Visiting `http://localhost:3000/users/1/messages?limit=2` returns `'[{"text":"Hey there","id":1},{"text":"Limit 2","id":2}]'`.

This library is experimental, in flux, and will likely have breaking API changes.

### Table of Contents

* [Getting Started](#getting-started)
* [Examples](#examples)
* [API Documentation](#api-documentation)
* [Guide](#guide)
  * [Overview](#overview)
  * [Requests](#requests)
    * [Methods](#methods)
    * [URL parameters](#url-parameters)
    * [Request body](#request-body)
    * [Query strings](#query-strings)
  * [Responses](#responses)
    * [Modified status or headers](#modified-status-or-headers)
    * [Errors](#errors)
    * [Static files](#static-files)
  * [Guards](#guards)
* [Building](#building)

## Getting Started

Install Payload:

```
bower install purescript-payload
```

Then read on for examples and docs.

## Examples

[The above hello world example](./examples/hello/Main.purs) and more can be found in the [examples directory](./examples). These examples are built and run as part of the project tests, so they are always working and up to date.

## API Documentation

[API documentation can be found on Pursuit.](https://pursuit.purescript.org/packages/purescript-payload)

## Guide

### Overview

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

`GET "/users/<id>"` says we have a `GET` endpoint with a URL parameter named `id`. The type of `id` is defined below it as an `Int`. The endpoint returns a `User`, with type as defined below.

To run a Payload server, you provide a spec and a record of handlers corresponding to each endpoint defined in the API spec. The above example can be run like so:

```purescript
api = { getUser: getUser }

getUser :: { id :: Int } -> Aff User
getUser { id, limit } = pure { id: 1, name: "whodunnit"}

main = Payload.launch spec api
```

Payload will helpfully fail to compile if an endpoint was defined in the spec but no corresponding handler was provided when starting the server. A handler is just an asynchronous function taking in a Record of the request parameters defined in the spec: in this case just an `id` field of type `Int`. URL parameters, query parameters, and bodies defined in the spec are automatically decoded into typed values and merged into the handler payload record by name. The returned `User` value is also automatically encoded to JSON.

Specs can also be hierarchical:

```purescript
moviesApiSpec :: Spec {
  guards :: {
     apiKey :: ApiKey,
     sessionId :: SessionId
  },
  routes :: {
    v1 :: Routes "/v1" {
       guards :: Guards ("apiKey" : Nil),
       auth :: Routes "/authentication" {
         token :: Routes "/token" {
           new :: GET "/new" {
             response :: RequestTokenResponse
           }
         }
       },
       movies :: Routes "/movies" {
         latest :: GET "/latest" {
           response :: Movie
         },
         byId :: Routes "/<movieId>" {
           params :: { movieId :: Int },
           get :: GET "/" {
             response :: Movie
           },
           rating :: Routes "/rating" {
             guards :: Guards ("sessionId" : Nil),
             create :: POST "/rating" {
               body :: RatingValue,
               response :: StatusCodeResponse
             }
           }
         }
      }
    }
  }
}
moviesApiSpec = Spec

```

[Guards](#guards) and [URL paths and parameters](#url-parameters) specified in parent routes are merged and passed to child route handlers at compile time, so that at run time the handlers will receive those values as part of their incoming request payload. 

See the [Spec module](https://pursuit.purescript.org/packages/purescript-payload/0.1.0/docs/Payload.Spec) for the full set of supported spec keywords and the [Movies example](./examples/movies/Main.purs) for a larger example with hierarchical routes.

### Requests

The route you define in the API spec tells what must be true about a request for the route's handler to be called. Based on your route spec, Payload will automatically validate:

* Method and path
* URL parameters
* Request body
* Query strings

An endpoint handler will only be called if validations pass, or 404 Not Found will be returned by default.

#### Methods

Supported methods: `GET`, `HEAD`, `POST`, `PUT`, `DELETE` ([defined in Payload.Spec](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Spec#t:Route)).

Payload also automatically handles `HEAD` requests where a `GET` request is specified by running the handler and stripping the body from the response. This can be overridden by specifying a `HEAD` endpoint separately.

#### URL parameters

Payload supports decoding two types of URL parameters: named segments and multi-matches.

Named segments are specified by giving a name in the URL string in the form `/foo/<myName>/bar` and a corresponding type in the `params` field of the endpoint spec. The handler will only be called if URL parameters can be decoded as the given type, or 404 Not Found will be returned. Decoded parameters are merged by name into the payload record provided to handlers. For example:

```purescript
-- Spec:
getMessage :: GET "/users/<id>/messages/<messageId>" {
  params :: { id :: Int, messageId :: String },
  response :: Array Message
}

-- Handler:
getMessage :: { id :: Int, messageId :: String } -> Aff (Array Message)
getMessage { id, limit } = pure
  [{ id: 1, text: "Hey there"}, { id: 2, text: "Limit " <> show limit }]
```

Single URL parameter decoding can be extended by implementing the [FromParam](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Params#t:FromParam) type class.

Multi-matches must appear at the end of a URL and similarly given a type in the `params` field. The only supported type for multi-matches is `List String` unless the [FromSegments](https://pursuit.purescript.org/packages/purescript-payload/0.1.0/docs/Payload.Params#t:FromSegments) class is implemented. Example:

```purescript
-- Spec:
files :: GET "/<..path>" {
  params :: { path :: List String },
  response :: File
}

-- Handler:
files :: forall r. { path :: List String | r} -> Aff (Either Failure File)
files { path } = Handlers.directory "test" path
```

#### Request body

When a request body is specified on an API route, the route will only be called if a request body can be decoded into the specified type via the [DecodeBody](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.DecodeBody) type class. If decoding fails, a 404 response is returned. Handlers will be called with a payload field named `body` containing the decoded body.

Body decoding can be specified for custom types or overwritten by writing an instance of [DecodeBody](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.DecodeBody).

Example endpoint with body:

```purescript
-- Spec:
createUser :: POST "/users/new" {
  body :: NewUser,
  response :: User
}

-- Handler:
createUser :: forall r. { body :: User | r } -> Aff User
createUser {body: user} = pure user
```

#### Query strings

Payload supports three different types of query parameters: literals, keys, and multi-matches. Here is an example with all three:

```purescript
-- Spec:
search :: GET "/search?a=<a>&foo&b=<b>&<..rest>"
  { query :: { a :: Int, b :: Int, rest :: Object String }
  , response :: String }

search :: { a :: Int, b :: Int, rest :: Object String } -> Aff String
search _ = pure "Search result"
```

For query literals, the handler will only be called if the literal exists somewhere in the query string. For key matches, the query parameter must be decodable via the [FromQueryParam](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.QueryParams#t:FromQueryParam) type class. Query param decoding can also be extended via this type class.

See the [query integration test](./test/integration/Query.purs) for further examples.

### Responses

A simple Payload handler is just a function that asynchronously returns a value, which is automatically encoded and by default returned with status code 200 OK. Payload supports the following response bodies by default:

* `Empty` (empty body)
* String
* Stream
* Array (returns JSON)
* Record (returns JSON)
* `Json` (returns JSON)

JSON is encoded via the [purescript-simple-json library](https://github.com/justinwoo/purescript-simple-json). Handlers can also return any other response body that implements [EncodeResponse](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Response#t:EncodeResponse).

Payload validates at compile time that handler responses match the type specified in the API spec. Responses match the spec by either being exactly the type defined in the spec or by being convertable to that type via the [ToSpecResponse](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Response#t:ToSpecResponse) type class.

What other responses can be converted to spec responses?

#### Modified status or headers

To modify the status or headers, handlers can return a [Response](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Response#t:Response), which is a wrapper around a record with `status`, `headers`, and `body` fields. There are various helpers in the [Response](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Response#t:Response) module for creating and modifying responses.

#### Errors

To return an error, handlers can return `Either error val`, where `error` can be any encodable value. If the encodable value is a [Response](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Response#t:Response), it will be returned with the given status and headers, or by default with status 500 Internal Server Error for other error responses. Error responses are not represented in the API spec and do not need to match the spec.

#### Static files

Static files can be served using the [file](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Handlers#v:file) or [directory](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Handlers#v:directory) helpers in the [Handlers module](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Handlers). The provided handlers will add appropriate MIME types to responses and protect against directory traversal attacks. See also [the example for serving static files](./examples/files/Main.purs).

### Guards

Payload borrows a concept [from the Rust Rocket library](https://rocket.rs/v0.4/guide/requests/#request-guards) called request guards. A request guard is a function that is called before handlers are called that returns an arbitrary value which is passed to the handler function. It can also error or forward, in which case the handler is never called. A typical use case might be to decode and validate an authorization header or cookie.

Guards can be added to an endpoint by listing the guards by name in the API spec as a type-level list, e.g. `guards :: Guards ("guard1" : "guard2" : Nil)`. The listed guards will be called in order before the handler function is called. The value returned by a guard is specified separately at the root of the spec, and the guard functions themselves are passed to the server as a record separate from the server handlers. A guard name specified in a route spec must have a matching guard name in the top-level guards spec, and a corresponding guard must be passed in to the server when it is started, or the code won't compile.

For example, this API spec defines two guards:

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

The `/admin` endpoint will only be called if an `AdminUser` can be produced by the guard from the request. Similarly the `/user` endpoint will only be called if a `User` can be obtained from the request. This provides a compile-time guarantee that the handlers will only be called with authenticated requests.

The request guard itself is defined as a function:

```purescript
getAdminUser :: HTTP.Request -> Aff (Either Resp.Failure AdminUser)
getAdminUser req = do
  headers <- Guards.headers req
  case Headers.lookup "Authorization" headers of
    (Just "Token secret") -> pure (Right (AdminUser { id: 1, name: "John Admin" }))
    _ -> pure (Left (Resp.Forward "Not an admin"))
```

To use guards, the server must be started with [startGuarded](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Server#v:startGuarded) and given the guard functions as a record:

```purescript
main = do
  let guards = { adminUser: getAdminUser, user: getUser }
  let handlers = { adminIndex, userIndex, unauthenticatedIndex }
  Payload.launch spec { handlers, guards }
```

Guards can also be applied on parent routes in a hierarchical API spec. For an example of this see the [Movies API Example](./examples/movies/Main.purs).

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
