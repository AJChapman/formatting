# Nix Usage

This project prefers `nix` for building.
Below are instructions for how to use it.

## Requirements

You will need `nix` installed to test and build.

## Building

To run a build with `nix`, simply:

  nix-build

## Nix Shell

When developing though, you may want to work in a `nix-shell`:

  nix-shell
  cabal build

You can also specify one or more of several options:

- *pkgs*: Override the nixpkgs used for the shell (defaults to those pinned by `pinned-nixpkgs.nix`),
- *compiler*: Override the default compiler,
- *doBenchmark*: Whether to enable benchmarks listed in the .cabal file (defaults to false),
- *doTest*: Whether to enable tests listed in the .cabal file (defaults to false).

Examples:

  nix-shell --arg pkgs 'import /path/to/nixpkgs/default.nix {}'
  nix-shell --arg doTest true
  nix-shell --arg compiler '"ghc865"'

## Running

To run from within a `nix-shell`:

  nix-shell
  cabal exec -- <cmdname>

Where <cmdname> is the name of the executable you want to run.

## Getting a REPL

To run a repl with the package and its dependencies:

  nix-shell
  cabal repl

or

  nix-shell --run 'cabal repl'

## GHCID

Run `ghcid` from within `nix-shell`, like this:

  nix-shell
  ghcid -c 'cabal repl'

or

  nix-shell --run 'ghcid -c "cabal repl"'

## Tags

To generate project tags use `hasktags`, e.g:

```bash
grep -i hs-source-dir *.cabal | sed 's/^.*:\s*//' | sort | uniq | xargs hasktags
```

This will create `tags` and `TAGS`, for use with various editors.

To generate tags for all project code and all of the haskell dependencies of your project, so that you can jump straight to library source code:

```bash
cp $(nix-build --no-link -A projectTags)/tags ./
```

## Updating the Pinned nixpkgs

For reproducible builds we have pinned to a specific version of nixpkgs.
If you ever need to update to a different version of nixpkgs then run:

  nix-prefetch-git --rev <revision> --url https://github.com/NixOS/nixpkgs.git > nixpkgs.json

