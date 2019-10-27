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
* Decoding URL parameters, query parameters, and request bodies into typed values
* Encoding server responses
* Client functions for calling the API

It's like [OpenAPI/Swagger](https://swagger.io/) without the boilerplate and code generation. Unlike OpenAPI, if your handlers or clients don't match your spec the code won't compile, so servers and clients always stay in sync with the spec.

[The above example](./examples/hello/Main.purs) and more can be found in the [examples directory](./examples).

This library is experimental, in flux, and will likely have breaking API changes.

### Table of Contents

* [Getting Started](README.md#getting-started)
* [Guide](README.md#guide)
  * [Overview](README.md#overview)
  * [Requests](README.md#requests)
    * [Methods](README.md#methods)
    * [URL parameters](README.md#url-parameters)
    * [Request body](README.md#request-body)
    * [Query strings](README.md#query-strings)
  * [Responses](README.md#responses)
    * [Modified status or headers](README.md#modified-status-or-headers)
    * [Errors](README.md#errors)
    * [Static files](README.md#static-files)
  * [Guards](README.md#guards)
* [API Documentation](README.md#api-documentation)
* [Examples](README.md#examples)
* [Building](README.md#building)

## Getting Started

Install Payload:

```
bower install purescript-payload
```

Then copy the above example, [peek at other examples](./examples), or read the docs below.

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

`GET "/users/<id>"` is a type-level string that says we have a `GET` endpoint with a URL parameter named `id`. The type of `id` is defined below it as an `Int`. The endpoint returns a `User`, with type as defined below.

When the Payload server is run, you provide handlers corresponding to each endpoint defined in the API spec. For the above example, you might run it like so:

```purescript
api = { getUser: getUser }

getUser :: { id :: Int } -> Aff User
getUser { id, limit } = pure { id: 1, name: "whodunnit"}

main = Payload.launch spec api
```

The code will helpfully fail to compile if an endpoint has been defined in the spec but there no corresponding handler has been provided when starting the server.

A handler is just an asynchronous function taking in a Record of the request parameters defined in the spec: an `id` field of type `Int`. URL parameters, query parameters, and bodies defined in the spec are automatically decoded into typed values and merged into the handler payload record by name. The returned `User` value is also automatically encoded to JSON.

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

Guards and URL specified at parent routes are combined and passed to child route handlers at compile time, so that at run time the handlers will receive those values as part of their incoming request payload. 

See also [this example for hierarchical routes](./examples/movies/Main.purs) and the [Spec module](https://pursuit.purescript.org/packages/purescript-payload/0.1.0/docs/Payload.Spec) for supported spec keywords.

### Requests

The route you define in the API spec tells what must be true about a request for the route's handler to be called. Based on your route spec, Payload will automatically validate:

* Method and path
* URL parameters
* Request body
* Query strings

#### Methods

Supported methods: `GET`, `HEAD`, `POST`, `PUT`, `DELETE` (defined in [Payload.Spec](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Spec)).

Payload also automatically handles `HEAD` requests where a `GET` request is specified by running the handler and stripping the body from the response. This can be overridden by specifying a `HEAD` endpoint separately.

#### URL parameters

Payload supports decoding two types of URL parameters: named segments and multi-matches.

Named segments are specified by giving a name in the URL string in the form `/foo/<myName>/bar` and a corresponding type in the `params` field of the endpoint spec. The handler will only be called if URL parameters can be decoded as the given type, or 404 will be returned. Decoded parameters are merged by name into the payload record provided to handlers. For example:

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

When a request body is specified on an API route, the route will only be called if a request body can be decoded into the specified type via the [FromData](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.FromData) type class. If decoding fails, a 404 response is returned. Handlers will be called with a payload field named `body` containing the decoded body.

Body decoding can be specified for custom types or overwritten by writing an instance of [FromData](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.FromData).

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

Payload supports three different types of query parameters: literals, keys, and multi-matches. See the [query integration test](./test/integration/Query.purs) for examples.

### Responses

Server handlers can return any response that implements [ToSpecResponse](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Response#t:ToSpecResponse) and [EncodeResponse](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Response#t:EncodeResponse), including arbitrary JSON responses via the [purescript-simple-json library](https://github.com/justinwoo/purescript-simple-json). Returning any of the following types will return status 200 with appropriate headers added:

* `Empty` (empty body)
* String
* Stream
* Array (returns JSON)
* Record (returns JSON)
* `Json` (returns JSON)

Payload validates at compile time that handler responses match the type you specified in your API spec. Responses match by either being exactly the type in the spec, or by being convertable to that type via the `ToSpecResponse` type class.

What other responses can be converted to spec responses?

#### Modified status or headers

To modify the status or headers, handlers can return a [Response](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Response#t:Response), which is a wrapper around a record with `status` and `headers` fields. There are various helpers in the [Response](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Response#t:Response) module for creating and modifying responses.

#### Errors

To return an error, handlers can return `Either error val`, where `error` can be any encodable value. By default any arbitrary encodable value can be returned as an error with status 500 Internal Server Error, or arbitrary responses can be returned with the `Response` type. Error responses are not represented in the API spec and do not need to match the spec.

#### Static files

Static files can be served using the [file](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Handlers#v:file) or [directory](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Handlers#v:directory) helpers in the [Handlers module](https://pursuit.purescript.org/packages/purescript-payload/docs/Payload.Handlers). The provided handlers will add appropriate MIME types to responses and protect against directory traversal attacks. There is also [an example module for serving static files](./examples/files/Main.purs).

### Guards

Payload borrows a concept called request guards from the [Rust Rocket library](https://rocket.rs/v0.4/guide/requests/#request-guards). A request guard is a function that is called before handlers are called that returns an arbitrary value that the handler function receives. It can also error or forward, in which case the handler is never called. A typical use case might be to decode and validate an authorization header or cookie.

Guards can be added to an endpoint by listing the guards by name in the API spec as a type-level list, e.g. `guards :: Guards ("guard1" : "guard2" : Nil)`. The listed guards will be called in order before the handler function is called. The value returned by a guard is specified separately at the root of the spec, and the guard functions themselves are passed to the server as a record. A guard name specified din a route must match a guard name in the top-level guards spec, and all guards must be passed in to the server by name when it is started, or the code won't compile.

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

The `/admin` endpoint will only be called if an `AdminUser` can be produced by the guard from the request. Similarly the `/user` endpoint will only be called if a `User` can be obtained from the request. This provides a compile-time guarantee that handlers will only be called with authenticated requests.

The request guard itself is defined as a function:

```purescript
getAdminUser :: HTTP.Request -> Aff (Either Resp.Failure AdminUser)
getAdminUser req = do
  headers <- Guards.headers req
  case Headers.lookup "Authorization" headers of
    (Just "Token secret") -> pure (Right (AdminUser { id: 1, name: "John Admin" }))
    _ -> pure (Left (Resp.Forward "Not an admin"))
```

and guards are passed in to the server as a record when starting the server with `startGuarded`:

```purescript
main = do
  let guards = { adminUser: getAdminUser, user: getUser }
  let handlers = { adminIndex, userIndex, unauthenticatedIndex }
  Payload.launch spec { handlers, guards }
```

API specs can also be hierarchical and guards can be applied to all endpoints below them in the hierarchy. For an example of this see the [Movies API Example](./examples/movies/Main.purs).

## API Documentation

[API documentation can be found on Pursuit.](https://pursuit.purescript.org/packages/purescript-payload)

## Examples

[The hello world example](./examples/hello/Main.purs) and more can be found in the [examples directory](./examples). The examples are built and run as part of the project tests.

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
