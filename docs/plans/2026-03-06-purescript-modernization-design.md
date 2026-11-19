# PureScript Modernization Design

**Date:** 2026-03-06

## Overview

Upgrade `purescript-payload` from the legacy Dhall package-set workflow to the current PureScript ecosystem baseline. The repo should target stable PureScript `0.15.15`, `spago 1.0.3`, and Registry-era package management, while making only the source changes required to restore compatibility.

This session does not publish anything externally. It prepares the repository so a later publish can be done without another tooling migration.

## Current State

- Project tooling is pinned to `purescript@0.15.12` and `spago@0.21.0`.
- Dependency resolution uses `spago.dhall` plus `packages.dhall`.
- Scripts and docs still reference `bower`, `pulp`, and older install/publish workflows.
- The repository contains a large test and examples surface area, so compatibility must include library code, tests, and example programs.

## Goals

- Move the project to stable PureScript `0.15.15`.
- Move the project to stable `spago 1.0.3`.
- Replace Dhall package-set configuration with `spago.yaml` and Registry package-set resolution.
- Update project scripts and metadata so normal development works with current tooling.
- Update docs and package metadata so later Registry publishing is straightforward.
- Keep public API changes to the minimum required for compiler or dependency compatibility.

## Non-Goals

- No external publishing during this work.
- No attempt to preserve `bower` or `pulp` as supported workflows.
- No API redesign beyond compatibility fixes.
- No unnecessary refactors unrelated to the migration.

## Options Considered

### 1. Full Registry-Era Migration

Replace `spago.dhall` and `packages.dhall` with `spago.yaml`, move to Registry package sets, upgrade compiler and Spago, and modernize docs and publish metadata.

This is the recommended option because it aligns the project with the supported ecosystem and avoids paying for a second migration later.

### 2. Conservative Legacy Bump

Stay on Dhall and only bump to the newest legacy package set plus compatible compiler versions.

This keeps the diff smaller in the short term, but leaves the repo on outdated project structure and delays the inevitable modernization work.

### 3. Dual-Mode Support

Support both modern Registry workflows and the old Dhall and Bower/Pulp paths.

This would reduce migration friction for older consumers, but it expands maintenance cost and complicates testing for a library repository.

## Decision

Adopt option 1: full Registry-era migration.

The repository will treat modern Spago and Registry package sets as the only supported development workflow. Legacy Dhall, Bower, and Pulp paths will be removed or explicitly retired from normal docs and scripts.

## Design

### Tooling And Metadata

- Add `spago.yaml` as the source of truth for project metadata and dependencies.
- Retire `spago.dhall` and `packages.dhall` from active use.
- Update `package.json` to use current tooling versions, specifically `purescript@0.15.15` and `spago@1.0.3`.
- Remove obsolete development dependencies such as `pulp`, `bower`, and `purescript-psa` unless validation shows one is still required.
- Ensure a fresh checkout can build and test with `npm install`, `spago build`, and `spago test`.

### Dependency Alignment

- Let modern Spago resolve packages through the Registry package set.
- Reconstruct the direct dependency list explicitly in `spago.yaml`.
- Add any dependencies that older Dhall-based transitive resolution effectively masked.
- Expect minor package naming or direct dependency adjustments around Node, browser, and JSON-related packages.

### Source Compatibility

- Compile the library, tests, and examples against the new compiler and current package versions.
- Make the minimum code changes required to satisfy compiler errors and dependency API changes.
- Pay special attention to:
  - JavaScript FFI modules
  - Row and type-level machinery
  - Node interop
  - Example programs that may rely on older tooling behavior

### Consumer And Release Readiness

- Update README install, build, test, and publish guidance to the Registry-era workflow.
- Remove or clearly retire obsolete Bower and Pulp instructions.
- Ensure package metadata is suitable for a later Registry publish.
- Prepare local package validation steps, but do not publish in this session.

## Validation

The migration is complete when all of the following are true:

- `spago build` succeeds on the upgraded toolchain.
- `spago test` succeeds on the upgraded toolchain.
- The examples that are part of the current repository workflow still compile and any retained local run instructions are accurate.
- The repository no longer depends on Dhall package sets for normal development.
- Docs and metadata are ready for a later Registry publish without another tooling migration.

## Risks

- Some current dependencies may have moved, changed shape, or now require explicit direct declaration.
- Current FFI modules may need small compatibility updates.
- Registry publish readiness may expose metadata or package boundary issues late in the migration.
- Example programs may require script changes if they depend on old Spago or Pulp behavior.

## Mitigations

- Start by generating modern project metadata and resolving dependency issues before source edits.
- Use compiler and test failures to drive only the compatibility changes that are actually necessary.
- Keep documentation updates aligned with the final working commands instead of guessing early.
- Validate local release readiness before claiming the migration is complete.

## Success Criteria

The repository builds and tests on stable PureScript `0.15.15` with `spago 1.0.3`, uses `spago.yaml` and Registry package sets as its normal workflow, no longer documents Bower or Pulp as supported paths, and is locally ready for a later publish without publishing during this work.
