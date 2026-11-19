# Payload on today's Node bindings: Design

Date: 2026-11-19

## What is built and why

Polyglot's admin page uses Payload's client, but Payload's server half was written against the Node bindings of a 2023 package set, so the admin page holds nine Node packages back at that set to build at all. This moves Payload to the package set the admin page already uses for everything else, so the next change in Polyglot can drop those overrides. It builds on the owner's March modernisation work, which already ported the server to the current Node bindings and moved the build to the modern Spago, and pins the package set to the one the admin page's set is made from, and stops a malformed request from crashing the server. Today's URL bindings only offer the standard URL parser, so the server now reads some unusual request paths differently; restoring the old reading is a separate task.

## UI changes

None.

## API changes

What a Payload server answers changes for unusual request targets, because the request target is now read by the WHATWG `URL` parser (`node-url` 7 has no `url.parse`). Ordinary paths and query strings route as before; all 263 earlier tests pass unchanged.

| Request target | Before | Now |
|---|---|---|
| Not a URL, such as `//[` | routed as the path `//[`, so 404 | 500 `Path could not be decoded: Invalid URL` (the port crashed the server here; fixed in this branch) |
| Starts with `//`, such as `//users/1` | routed as `/users/1` | first segment read as a host: routed as `/1` |
| Dot segments, such as `/a/x/../b` or `/%2e%2e/b` | routed literally | resolved first: routed as `/a/b`, `/b` |
| Non-ASCII in the query | passed on raw | passed on percent-encoded |

The debug log line (`LogDebug` only) prints the raw request target instead of `show` of the parsed path.

For library users, handlers, guards and cookies take `IncomingMessage IMServer` and `ServerResponse` from `node-http` 9 in place of `HTTP.Request` and `HTTP.Response`, and `Payload.Examples.*` ship as modules of the package.

## Database changes

None.

## Automerge changes

None.

## Infrastructure changes

None.

## Decisions

| Decision | Why |
|---|---|
| dependencies 1 C (Polyglot, 2026-11-19): Payload is ported to today's Node bindings in its own repository, as a pull request there; after it merges, the admin page's pin moves and its nine `node-*` overrides go. | The owner's answer to [the dependencies cards](https://github.com/hoodunit/polyglot/blob/1f6a4859f540b527da871385742e5a9bee835543/docs/plans/2026-11-19-dependencies-cards-design.md). |
| The build is Spago 1.0.3 with a registry package set, PureScript 0.15.15, and the examples live in the source root. | The owner's [March modernisation design](2026-03-06-purescript-modernization-design.md). |

## Claude's decisions

| Decision | Why |
|---|---|
| The branch is the owner's nine unpushed March commits from the local `master`, plus one commit that changes the package set. | Those commits already port the server to `node-http` 9 and the rest of today's bindings, with all tests passing; redoing the port would duplicate them. |
| The package set is registry 81.2.0. | The admin page builds on `psc-0.15.15-20261002`, and 81.2.0 is the registry set that legacy set is made from: published the same day, and the same versions for every package except `marked` and `optparse`, which Payload does not use. |
| The toolchain move and the package-set change are one pull request, not one per tool. | The March commits bundle the move to Spago 1.x with the port, and today's bindings need the newer set, so neither half builds alone on today's admin set. |
| A request target that is not a URL is answered with the existing 500 "Path could not be decoded" instead of crashing the server, pinned by a routing test. | The port let the parser's exception escape the request listener, a defect the branch introduced. The error path was already there; 500 rather than the old 404 because matching the old reading is the follow-up's job. |
| The server's `error` listener during `listen` is `on` again, not `once`. | The March port changed it; with `once`, a second server error would go unhandled and stop the process. Nothing in the suite can raise two server errors, so it has no test. |
| The other routing differences in the table above are left as they are and become their own Autobuild task on the Polyglot board. | The task's rule: a part that changes what is answered stops and becomes its own task. They come with the port itself, and the admin page never sends such paths. |
| The new routing test sits in the Routing suite, not the Server errors suite. | `test/integration/Server/Errors.purs` is not wired into `test/Main.purs`, before or after this branch; wiring it in would change which tests run. |
| No version bump, tag or registry publish. | The admin page pins Payload by commit; publishing is the owner's call and the March design left it for later. |
| The March implementation plan is deleted; the March design stays. | Implementation plans are deleted before review; the design is a dated record. |
| The examples stay in the source root, so they ship as `Payload.Examples.*` modules with the library. | It is the owner's March choice; the admin page's bundler drops them since nothing imports them. |
| Payload has no code-health script; the pull request gives line counts, test counts, build warnings and `npm audit` instead. | `tools/code-health/health.sh` is Polyglot's and measures Polyglot's code. |

## What the next task changes in Polyglot

Checked by building a copy of the admin page with `npm run build:prod`: the nine `node-*` overrides and the second package set go, the pin moves to this branch's merge commit, and Payload's dependency list in `packages.dhall` becomes the 42 packages of its `spago.yaml`. Compared with today's 46, `affjax`, `affjax-node`, `http-methods`, `js-promise-aff`, `node-fs-aff` and `test-unit` leave, and `node-event-emitter` and `node-net` join.
