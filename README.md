# Payload

Payload is a PureScript HTTP server library designed to let you focus on the payload of a request and response. Given an API spec, Payload automatically decodes and validates request parameters into typed values. When handlers return values, they are automatically encoded into typed response. Front end clients can use the same spec to get functions for typed requests and responses to the API.

Examples can be found in the [examples directory](./examples).

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