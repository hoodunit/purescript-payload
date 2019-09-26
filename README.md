# Payload

Payload is a PureScript HTTP server library designed to let you focus on the payload of a request and response. Given an API spec, Payload will automatically decode and validate request parameters into typed values. When your handler returns a value, this is automatically encoded into a typed response.

Examples can be found in the [examples directory](./examples).

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