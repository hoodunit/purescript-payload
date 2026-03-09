# PureScript Modernization Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Upgrade `purescript-payload` to stable PureScript `0.15.15`, stable `spago 1.0.3`, and a Registry-era workspace layout that builds, tests, and is ready for a later publish without publishing in this session.

**Architecture:** Keep the root package as the library package `payload`, migrate the repo to `spago.yaml`, and model the examples as explicit workspace packages under `examples/` because modern Spago no longer supports arbitrary `examples/**/*.purs` source globs. Drive compatibility fixes from compiler and test failures, then finish by updating docs and local publish metadata.

**Tech Stack:** PureScript `0.15.15`, Spago `1.0.3`, PureScript Registry package set `73.3.0`, Node/npm, existing JS FFI modules, shared workspace output.

---

## Preconditions

- Work in a dedicated git worktree before touching implementation files, because the current checkout already has extensive unrelated changes.
- Keep the design doc commit `a10b2b3` intact.
- Use `spago-legacy migrate` only as a bootstrap, then hand-edit `spago.yaml` to match the approved design.
- Do not run `spago publish` in this implementation. Local validation is allowed; external publishing is not.

### Task 1: Create an isolated worktree and bootstrap modern project metadata

**Files:**
- Create: `spago.yaml`
- Modify: `package.json`
- Modify: `package-lock.json`
- Delete: `spago.dhall`
- Delete: `packages.dhall`

**Step 1: Create the dedicated worktree**

Run:

```bash
git worktree add ../purescript-payload-ps-01515 -b chore/purs-01515-modernization
```

Expected: a new sibling worktree with a clean checkout of the current branch.

**Step 2: Verify the baseline fails on the new toolchain**

Run:

```bash
npx spago@1.0.3 build
```

Expected: FAIL because the repository still depends on legacy Dhall configuration and legacy source layout.

**Step 3: Generate the first `spago.yaml`**

Run:

```bash
npx spago-legacy migrate
```

Expected: `spago.yaml` appears at the repo root with a migrated starting point.

**Step 4: Replace the migrated file with an explicit workspace config**

Write `spago.yaml` so it contains the root package plus the shared workspace package set:

```yaml
package:
  name: payload
  dependencies:
    - aff
    - aff-promise
    - affjax
    - affjax-node
    - arraybuffer-types
    - arrays
    - bifunctors
    - console
    - datetime
    - effect
    - either
    - exceptions
    - foldable-traversable
    - foreign
    - foreign-object
    - http-methods
    - integers
    - js-date
    - lists
    - maybe
    - media-types
    - newtype
    - node-buffer
    - node-fs
    - node-http
    - node-path
    - node-streams
    - node-url
    - nullable
    - ordered-collections
    - prelude
    - record
    - refs
    - simple-json
    - strings
    - stringutils
    - transformers
    - tuples
    - type-equality
    - typelevel-prelude
    - unfoldable
    - unsafe-coerce
    - web-streams
  test:
    main: Test.Main
    dependencies:
      - test-unit

workspace:
  packageSet:
    registry: 73.3.0
```

Expected: the root package is defined in modern Spago format and no longer relies on Dhall.

**Step 5: Update Node tooling**

Modify `package.json` to:

```json
{
  "private": true,
  "scripts": {
    "clean": "rimraf output .spago",
    "build": "spago build -p payload",
    "build:watch": "spago build -p payload --watch",
    "docs:local": "spago docs -p payload",
    "example:github": "spago run -p payload-example-client-github -m Payload.Examples.ClientGitHub.Main",
    "test:payload": "spago test -p payload",
    "test:example:basic": "spago test -p payload-example-basic",
    "test:example:files": "spago test -p payload-example-files",
    "test:example:hello": "spago test -p payload-example-hello",
    "test:example:movies": "spago test -p payload-example-movies"
  },
  "devDependencies": {
    "nodemon": "2.0.21",
    "purescript": "0.15.15",
    "rimraf": "4.3.1",
    "spago": "1.0.3",
    "xhr2": "https://github.com/hoodunit/node-xhr2.git#6f65a6557706842b7284191f15ec9afaba270909"
  }
}
```

Also remove `bower`, `pulp`, and `purescript-psa` from `devDependencies`.

**Step 6: Refresh the lockfile and remove legacy config**

Run:

```bash
npm install
```

Then delete `spago.dhall` and `packages.dhall`.

Expected: `package-lock.json` is updated for the new toolchain and only `spago.yaml` remains as project config.

**Step 7: Verify the new config still fails only because source layout is not migrated yet**

Run:

```bash
npm run build
```

Expected: FAIL with module or package resolution errors tied to the current `examples/**/*.purs` layout, not missing Dhall config.

**Step 8: Commit**

```bash
git add spago.yaml package.json package-lock.json spago.dhall packages.dhall
git commit -m "build: migrate project to modern Spago config"
```

### Task 2: Convert example code into explicit workspace packages

**Files:**
- Create: `examples/basic/spago.yaml`
- Create: `examples/basic/src/Payload/Examples/Basic/Main.purs`
- Create: `examples/basic/src/Payload/Examples/Basic/Client.purs`
- Create: `examples/basic/src/Payload/Examples/Basic/Spec.purs`
- Create: `examples/basic/test/Payload/Examples/Basic/Test.purs`
- Create: `examples/hello/spago.yaml`
- Create: `examples/hello/src/Payload/Examples/Hello/Main.purs`
- Create: `examples/hello/test/Payload/Examples/Hello/Test.purs`
- Create: `examples/files/spago.yaml`
- Create: `examples/files/src/Payload/Examples/Files/Main.purs`
- Create: `examples/files/test/Payload/Examples/Files/Test.purs`
- Create: `examples/movies/spago.yaml`
- Create: `examples/movies/src/Payload/Examples/Movies/Main.purs`
- Create: `examples/movies/test/Payload/Examples/Movies/Test.purs`
- Create: `examples/client-github/spago.yaml`
- Create: `examples/client-github/src/Payload/Examples/ClientGitHub/Main.purs`
- Create: `examples/client-github/src/Payload/Examples/ClientGitHub/Main.js`
- Delete: `examples/basic/Main.purs`
- Delete: `examples/basic/Client.purs`
- Delete: `examples/basic/Spec.purs`
- Delete: `examples/basic/Test.purs`
- Delete: `examples/hello/Main.purs`
- Delete: `examples/hello/Test.purs`
- Delete: `examples/files/Main.purs`
- Delete: `examples/files/Test.purs`
- Delete: `examples/movies/Main.purs`
- Delete: `examples/movies/Test.purs`
- Delete: `examples/client-github/Main.purs`
- Delete: `examples/client-github/Main.js`

**Step 1: Verify the current example layout is the blocking issue**

Run:

```bash
spago build -p payload
```

Expected: FAIL with missing modules imported from `Payload.Examples.*`.

**Step 2: Create `spago.yaml` for each example package**

Use this pattern, adjusting package names and dependencies per example:

```yaml
package:
  name: payload-example-basic
  dependencies:
    - effect
    - prelude
    - payload
  test:
    main: Payload.Examples.Basic.Test
    dependencies:
      - test-unit
      - payload
```

Expected: each example becomes a first-class workspace package that depends on `payload`.

**Step 3: Move example modules into standard `src/` and `test/` directories**

Preserve module names exactly. For example:

```text
examples/basic/Main.purs
-> examples/basic/src/Payload/Examples/Basic/Main.purs

examples/basic/Test.purs
-> examples/basic/test/Payload/Examples/Basic/Test.purs
```

Move the client GitHub FFI file with its module:

```text
examples/client-github/Main.js
-> examples/client-github/src/Payload/Examples/ClientGitHub/Main.js
```

Expected: all example PureScript and FFI files live in standard package locations understood by modern Spago.

**Step 4: Keep static assets in place and fix file-path assumptions if needed**

Validate that:

- `examples/files/public/**` stays where the example server expects it.
- `examples/files/private.json` still resolves from the moved PureScript module.
- Any relative path strings inside `examples/files/src/Payload/Examples/Files/Main.purs` are updated to match the new module location.

**Step 5: Verify each example package compiles or fails only on library compatibility issues**

Run:

```bash
spago build -p payload-example-basic
spago build -p payload-example-hello
spago build -p payload-example-files
spago build -p payload-example-movies
spago build -p payload-example-client-github
```

Expected: package discovery works; any remaining failures are normal compiler or dependency breakages.

**Step 6: Commit**

```bash
git add examples
git commit -m "build: convert examples into workspace packages"
```

### Task 3: Make the root library compile on PureScript 0.15.15

**Files:**
- Modify: `src/Payload/Client.purs`
- Modify: `src/Payload/Client/ClientApi.purs`
- Modify: `src/Payload/Client/DecodeResponse.purs`
- Modify: `src/Payload/Client/EncodeBody.purs`
- Modify: `src/Payload/Client/EncodeParam.purs`
- Modify: `src/Payload/Client/Fetch.purs`
- Modify: `src/Payload/Client/Internal/EncodeUri.purs`
- Modify: `src/Payload/Client/Internal/Query.purs`
- Modify: `src/Payload/Client/Internal/Url.purs`
- Modify: `src/Payload/Client/Options.purs`
- Modify: `src/Payload/Client/QueryParams.purs`
- Modify: `src/Payload/Client/Queryable.purs`
- Modify: `src/Payload/Client/Response.purs`
- Modify: `src/Payload/ContentType.purs`
- Modify: `src/Payload/Debug.purs`
- Modify: `src/Payload/Headers.purs`
- Modify: `src/Payload/Internal/QueryParsing.purs`
- Modify: `src/Payload/Internal/Route.purs`
- Modify: `src/Payload/Internal/UrlParsing.purs`
- Modify: `src/Payload/Internal/Utils.purs`
- Modify: `src/Payload/Method.purs`
- Modify: `src/Payload/ResponseTypes.purs`
- Modify: `src/Payload/Server.purs`
- Modify: `src/Payload/Server/Cookies.purs`
- Modify: `src/Payload/Server/DecodeBody.purs`
- Modify: `src/Payload/Server/Guards.purs`
- Modify: `src/Payload/Server/Handleable.purs`
- Modify: `src/Payload/Server/Handlers.purs`
- Modify: `src/Payload/Server/Internal/GuardParsing.purs`
- Modify: `src/Payload/Server/Internal/MimeTypes.purs`
- Modify: `src/Payload/Server/Internal/OmitEmpty.purs`
- Modify: `src/Payload/Server/Internal/Query.purs`
- Modify: `src/Payload/Server/Internal/Querystring.purs`
- Modify: `src/Payload/Server/Internal/Request.purs`
- Modify: `src/Payload/Server/Internal/ServerResponse.purs`
- Modify: `src/Payload/Server/Internal/Trie.purs`
- Modify: `src/Payload/Server/Internal/Url.purs`
- Modify: `src/Payload/Server/Internal/UrlString.purs`
- Modify: `src/Payload/Server/Params.purs`
- Modify: `src/Payload/Server/Path.purs`
- Modify: `src/Payload/Server/QueryParams.purs`
- Modify: `src/Payload/Server/Response.purs`
- Modify: `src/Payload/Server/Routable.purs`
- Modify: `src/Payload/Server/Status.purs`
- Modify: `src/Payload/Spec.purs`
- Modify: `src/Payload/TypeErrors.purs`
- Modify: `src/Payload/Client/Fetch.js`
- Modify: `src/Payload/Client/Internal/EncodeUri.js`
- Modify: `src/Payload/Debug.js`
- Modify: `src/Payload/Internal/Utils.js`
- Modify: `src/Payload/Server.js`
- Modify: `src/Payload/Server/Cookies.js`
- Modify: `src/Payload/Server/Internal/MimeTypes.js`
- Modify: `src/Payload/Server/Internal/Querystring.js`
- Modify: `src/Payload/Server/Internal/ServerResponse.js`
- Modify: `src/Payload/Server/Internal/UrlString.js`
- Modify: `src/Payload/Server/Path.js`
- Modify: `src/Payload/Server/Response.js`

**Step 1: Capture the first failing library build**

Run:

```bash
spago build -p payload
```

Expected: FAIL with the first compiler error on the upgraded toolchain.

**Step 2: Fix one compiler error family at a time**

Work in this order:

1. Missing imports and package moves
2. Kind, row, and type-equality changes
3. Record and `Foreign`-related breakages
4. Node and stream API drift
5. Warning-to-error issues caused by stricter compiler output

Do not refactor unrelated code while fixing compatibility.

**Step 3: Re-run the root build after each focused fix**

Run:

```bash
spago build -p payload
```

Expected: the next real compiler error appears, with the total error count shrinking over time.

**Step 4: Stop only when the root package builds cleanly**

Run:

```bash
spago build -p payload --purs-args "--censor-lib"
```

Expected: PASS.

**Step 5: Commit**

```bash
git add src
git commit -m "fix: restore library compatibility on PureScript 0.15.15"
```

### Task 4: Restore the test suite on the new workspace layout

**Files:**
- Modify: `test/Main.purs`
- Modify: `test/Config.purs`
- Modify: `test/Helpers.purs`
- Modify: `test/unit/Client/EncodeParam.purs`
- Modify: `test/unit/Client/QueryParams.purs`
- Modify: `test/unit/Internal/QueryParsing.purs`
- Modify: `test/unit/Internal/UrlParsing.purs`
- Modify: `test/unit/Server/Cookies.purs`
- Modify: `test/unit/Server/Internal/OmitEmpty.purs`
- Modify: `test/unit/Server/Internal/Query.purs`
- Modify: `test/unit/Server/Internal/Trie.purs`
- Modify: `test/unit/Server/Internal/Url.purs`
- Modify: `test/unit/Server/Params.purs`
- Modify: `test/unit/Server/Response.purs`
- Modify: `test/integration/Client/ContentTypes.purs`
- Modify: `test/integration/Client/Errors.purs`
- Modify: `test/integration/Client/Methods.purs`
- Modify: `test/integration/Client/Options.purs`
- Modify: `test/integration/Client/QueryParams.purs`
- Modify: `test/integration/Client/Response.purs`
- Modify: `test/integration/Client/Response.js`
- Modify: `test/integration/Client/Statuses.purs`
- Modify: `test/integration/Server/Body.purs`
- Modify: `test/integration/Server/Errors.purs`
- Modify: `test/integration/Server/Guards.purs`
- Modify: `test/integration/Server/Methods.purs`
- Modify: `test/integration/Server/QueryParams.purs`
- Modify: `test/integration/Server/Routing.purs`
- Modify: `test/integration/Server/Status.purs`

**Step 1: Remove example test imports from the root test runner**

Edit `test/Main.purs` so it runs only the root package unit and integration suites. Remove imports of:

- `Payload.Examples.Basic.Test`
- `Payload.Examples.Files.Test`
- `Payload.Examples.Hello.Test`
- `Payload.Examples.Movies.Test`

Expected: the root test package no longer depends on example modules that now live in separate workspace packages.

**Step 2: Run the root test suite and fix compatibility issues**

Run:

```bash
spago test -p payload
```

Expected: FAIL at first; then PASS after updating test code and any affected test FFI.

**Step 3: Run each example package test and fix remaining breakages**

Run:

```bash
spago test -p payload-example-basic
spago test -p payload-example-hello
spago test -p payload-example-files
spago test -p payload-example-movies
```

Expected: all example package tests eventually PASS.

**Step 4: Verify the GitHub client example still runs**

Run:

```bash
spago build -p payload-example-client-github
spago run -p payload-example-client-github -m Payload.Examples.ClientGitHub.Main
```

Expected: build passes; runtime behavior is at least as good as before the migration.

**Step 5: Commit**

```bash
git add test examples
git commit -m "test: restore test suites on modern workspace layout"
```

### Task 5: Update docs and local publish readiness

**Files:**
- Modify: `README.md`
- Modify: `examples/README.md`
- Modify: `package.json`
- Modify: `spago.yaml`
- Delete: `bower.json`
- Optionally modify: `.gitignore`

**Step 1: Replace legacy install and build docs**

Update `README.md` to replace:

- `spago install payload` guidance that assumes the old toolchain
- `bower install purescript-payload`
- any `pulp` build or publish instructions

With current guidance centered on:

```bash
npm install
spago build -p payload
spago test -p payload
```

And example package commands such as:

```bash
spago test -p payload-example-basic
spago run -p payload-example-client-github -m Payload.Examples.ClientGitHub.Main
```

**Step 2: Document the example package layout**

Update `examples/README.md` and any example links in `README.md` so they point to the new `examples/*/src/...` and `examples/*/test/...` locations.

**Step 3: Add publish metadata without publishing**

Extend the root `spago.yaml` with a `publish` section like:

```yaml
package:
  name: payload
  dependencies:
    - prelude
  publish:
    version: 0.5.0
    license: Apache-2.0
    location:
      githubOwner: hoodunit
      githubRepo: purescript-payload
    include:
      - "src/**/*.purs"
      - "src/**/*.js"
      - "README.md"
      - "LICENSE"
```

Use the next correct version for the package rather than hardcoding `0.5.0` if the repo history shows a different release line.

Do not add `owners` in this task unless the repository owner explicitly provides publish keys in a separate session.

**Step 4: Remove obsolete metadata**

Delete `bower.json` if no current workflow depends on it, and remove any remaining `bower` or `pulp` references from `package.json`, docs, and ignore rules.

**Step 5: Commit**

```bash
git add README.md examples/README.md package.json spago.yaml bower.json .gitignore
git commit -m "docs: update modern PureScript workflow and publish metadata"
```

### Task 6: Run full verification before calling the migration complete

**Files:**
- Modify: none expected

**Step 1: Run the root build and test**

Run:

```bash
npm run build
npm run test:payload
```

Expected: PASS.

**Step 2: Run the example package test matrix**

Run:

```bash
npm run test:example:basic
npm run test:example:files
npm run test:example:hello
npm run test:example:movies
```

Expected: PASS.

**Step 3: Generate local docs**

Run:

```bash
npm run docs:local
```

Expected: PASS.

**Step 4: Validate publish readiness locally without publishing**

Run:

```bash
spago publish --dry-run
```

Expected: PASS, or a precise metadata-only failure that can be fixed locally without contacting the Registry.

**Step 5: Inspect the final diff**

Run:

```bash
git status --short
git log --oneline --decorate -5
```

Expected: only the intended modernization changes remain uncommitted, or the branch is clean if you committed the verification follow-up.

**Step 6: Commit any final metadata or verification fixes**

```bash
git add .
git commit -m "chore: finalize PureScript modernization"
```

Expected: only do this commit if verification required a final adjustment. Otherwise skip it and keep the prior commits as the final history.
