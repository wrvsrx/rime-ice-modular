# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo does

Repackages the upstream [rime-ice](https://github.com/iDvel/rime-ice) input method (vendored as the `externals/rime-ice` git submodule) into independently-installable Rime components. A Haskell [Shake](https://shakebuild.com/) build copies/transforms/generates files from the submodule into `build/`, and Nix wraps each component as a separate `rime-data` derivation so a user can install just pinyin, or just one double-pinyin layout, instead of the whole upstream tree.

The submodule must be present — clone/build with submodules (`nix build '.?submodules=1'`, or `git submodule update --init`).

## Commands

```bash
nix build '.?submodules=1'   # build everything via Nix (the canonical build)
nix develop                  # dev shell with ghc + shake + required Haskell libs
shake                        # inside the dev shell: build all components into ./build
shake <component-name>       # build a single component, e.g. `shake pinyin`
shake json                   # regenerate components.json from Components.hs (see below)
```

There is no test suite. `Werror=all` is set on all Haskell modules, so build warnings are errors.

## Architecture

The build is described once, in Haskell, as a tree of components (`Shakefile/Components.hs`):

- A `RimeComponent` is a `Data.Tree` node of `(name, [RimeTransformation])` plus child components (its dependencies). `allComponent` is the root.
- A `RimeTransformation` is the unit of work, with four variants (`Shakefile/Components.hs`):
  - `Identity path` — copy `externals/rime-ice/<path>` → `build/<path>` unchanged.
  - `Rename src dst` — copy with a different output path.
  - `Apply src dst f` — read source, run `f :: String -> String`, write result. Used for text rewrites that **must preserve YAML comments**, so it does string `replace` rather than parse/re-serialize.
  - `Produce dst content` — write a freshly generated file (no source input).
- `Shakefile/Renderer.hs` turns the tree into Shake `phony` rules (one per component, each `need`-ing its child component names) and also serializes the tree to JSON.

### components.json is a generated artifact — keep it in sync

`components.json` is produced by `shake json` and lists, per component, its `inputs`, `outputs`, and `dependencies`. It is the bridge from the Haskell build definition to Nix: `components.nix` reads it to construct one `rime-ice-<component>` derivation per entry, installing exactly that component's `outputs` and propagating its `dependencies`.

**After any change to `Shakefile/Components.hs` that adds/removes/renames a component or its files, run `shake json` and commit the updated `components.json`.** Nix packaging silently goes stale otherwise. Note `components.nix` only re-exports a curated subset of components as top-level flake packages (pinyin + the double-pinyin variants).

### Double-pinyin variants

The six double-pinyin layouts (Natural, Flypy, ABC, MSPY, Sogou, ZiGuang) are generated programmatically from a single `DoublePinyinSchema` enum via `getDoublePinyinSchema`. Each derives its schema/dict files from the base pinyin component by suffix substitution (`getDoubleSuffix` / `getDoubleSuffix'` — note these two differ for the Natural case). Adding a layout means extending that enum, not hand-writing files.

## Conventions

- Commits follow Conventional Commits (`feat:`, `fix:`, `chore(build):`, `chore(externals/rime-ice):`).
- The package version lives in `flake.nix` (`version = "..."`); bumping it is a deliberate `chore: bump version to ...` commit.
- Updating upstream = bumping the `externals/rime-ice` submodule (`chore(externals/rime-ice): ...`); often pairs with edits to the file lists in `Components.hs` when upstream adds/removes lua scripts or dicts, followed by `shake json`.
