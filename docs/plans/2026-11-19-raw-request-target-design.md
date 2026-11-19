# Payload routes unusual request paths as before its Node port: Design

Date: 2026-11-19

## What is built and why

The port of Payload to today's Node bindings read each request with the standard URL parser, because the old lenient parser is no longer in the bindings. That changed how a Payload server answers a few unusual requests: a path starting with two slashes, one with dot segments, one that is not a URL at all. The owner asked for the server to read them as it did before the port. The server now splits the raw request line itself, the way the old parser did for every request a client sends to a server, so no URL parser is involved and these requests route as they used to.

## UI changes

None.

## API changes

What a Payload server answers for unusual request targets goes back to what it answered before the port to `node-url` 7. Ordinary paths and query strings route as before.

| Request target | Before the port | After the port ([PR #33](https://github.com/hoodunit/purescript-payload/pull/33)) | Now | Pinned by |
|---|---|---|---|---|
| Not a URL, such as `//[` | path `//[`, so 404 | 500 `Path could not be decoded` | path `//[`, so 404 | Routing test |
| Starts with `//`, such as `//users/1` | routed as `/users/1` | routed as `/1` | routed as `/users/1` | Routing test |
| Dot segments, such as `/a/x/../b` or `/%2e%2e/b` | routed literally | resolved first | routed literally | Routing test |
| Non-ASCII in the query | raw | percent-encoded | raw | Unit test of the split |

How the target is read ([`UrlString.purs`](../../src/Payload/Server/Internal/UrlString.purs)): a fragment is dropped, an absolute-form target (`http://host/p?q`) loses its scheme and authority, and the rest splits at its first `?` into path and query.

The server no longer has a 500 `Path could not be decoded` answer, since reading the target cannot fail. `node-url` leaves Payload's dependencies.

## Database changes

None.

## Automerge changes

None.

## Infrastructure changes

None.

## Decisions

| Decision | Why |
|---|---|
| The server reads the path and query from the raw request target as before the port, without `url.parse`, and each row of the table is pinned by a test. | The owner's task, from the review of PR #33. |

## Claude's decisions

| Decision | Why |
|---|---|
| The target is split by hand rather than with another parser. | `url.parse` reads an origin-form target by splitting at the first `?` and dropping a fragment; nothing more is needed, and both remaining Node parsers (`URL`, `url.parse`) are what the task rules out or replaces. |
| An absolute-form target is still read from its path. | The old parser and the WHATWG parser both did; HTTP servers must accept that form. |
| Two quirks of `url.parse` are not kept: `//user@host/p` was read as a host with a user, and on targets starting with `///` it percent-encoded characters such as `'` in the query. | Neither is a path a client sends, and the encoded characters decode to the same query values. |
| The non-ASCII query row is pinned by a unit test of the split, not over HTTP. | The query decoder percent-decodes both forms to the same values, so the difference cannot be seen through a handler. |
| The 500 `Path could not be decoded` branch is removed rather than kept unused. | Nothing can produce it any more. |
| `node-url` is dropped from `spago.yaml`. | Nothing in Payload imports it now. The Polyglot follow-up's package list shrinks by one. |
| The pull request targets PR #33's branch. | The task says it goes on top of PR #33, which merges first; GitHub retargets it to `master` when that branch is deleted. |
