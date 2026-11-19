# Payload on today's Node bindings: Design

Date: 2026-11-19

## What is built and why

Payload's server half was written against the Node bindings of a 2023 package set, so a project using Payload with a current package set has to hold those bindings back. This moves Payload to the current Node bindings and a current registry package set, building on the March modernisation work that moved the build to the modern Spago and ported the server. The examples become a package of their own beside the library. It also stops a malformed request from crashing the server. Today's URL bindings only offer the standard URL parser, so the server now reads some unusual request paths differently.

## UI changes

None.

## API changes

What a Payload server answers changes for unusual request targets, because the request target is now read by the WHATWG `URL` parser (`node-url` 7 has no `url.parse`). Ordinary paths and query strings route as before; all 263 earlier tests pass unchanged.

| Request target | Before | Now |
|---|---|---|
| Not a URL, such as `//[` | routed as the path `//[`, so 404 | 500 `Path could not be decoded: Invalid URL` |
| Starts with `//`, such as `//users/1` | routed as `/users/1` | first segment read as a host: routed as `/1` |
| Dot segments, such as `/a/x/../b` or `/%2e%2e/b` | routed literally | resolved first: routed as `/a/b`, `/b` |
| Non-ASCII in the query | passed on raw | passed on percent-encoded |

The debug log line (`LogDebug` only) prints the raw request target instead of `show` of the parsed path.

For library users, handlers, guards and cookies take `IncomingMessage IMServer` and `ServerResponse` from `node-http` 9 in place of `HTTP.Request` and `HTTP.Response`.

## Database changes

None.

## Automerge changes

None.

## Infrastructure changes

None.

## Decisions

| Decision | Why |
|---|---|
| The build is Spago with a registry package set instead of Dhall, Bower and Pulp. | The [March modernisation design](2026-03-06-purescript-modernization-design.md). |
| PureScript 0.15.17, Spago 1.0.5 and registry package set 82.3.0, the latest of each. | Payload's dependency ranges in `spago.yaml` stay as they are, and every package Payload uses differs from the October set (81.2.0) by a minor or patch release only, so projects on older 0.15 sets still build it. |
| The toolchain move, the port and the package-set change are one pull request. | The port needs the newer bindings, which need the newer set, so neither half builds alone. |
| A request target that is not a URL is answered with the existing 500 "Path could not be decoded" instead of crashing the server, pinned by a routing test. | The port let the parser's exception escape the request listener. The error path was already there. |
| The server's `error` listener during `listen` is `on`, not `once`, as before the port. | With `once`, a second server error would go unhandled and stop the process. Nothing in the suite can raise two server errors, so it has no test. |
| The other routing differences in the table above are left as they are, for a separate change. | They come with the standard URL parser; restoring the old reading without `url.parse` is a change of its own. |
| The new routing test sits in the Routing suite, not the Server errors suite. | `test/integration/Server/Errors.purs` is not wired into `test/Main.purs`; wiring it in would change which tests run. |
| No version bump, tag or registry publish. | Publishing is left for later, as the March design did. |
| The examples are one workspace package, `payload-examples` in `examples/`, with their tests, and `npm test` runs both packages. | Spago 1.x builds only a package's `src/` and `test/`. Keeping the examples in the library's `src/` would ship them as modules of the package, and one package per example means five configurations to keep. |
| The example tests have their own copy of the few test helpers they use, on a port of their own. | A package cannot import another package's test modules, and a shared helpers package would depend on `payload` while `payload`'s tests depend on it. |
