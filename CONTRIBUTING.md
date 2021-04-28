# Build setup

Ideally, you build this project with [`nix`](https://nixos.org/nix/):

```
git clone git@github.com:sebastiaanvisser/clay.git
cd clay
nix-build
```

This will build `clay` for the current default version of GHC.
To build for a particular version,
set the `GHC_VERSION` environment variable:

```
GHC_VERSION=ghc881 nix-build
```

To find out which GHC versions your nix setup provides:

```
$ nix eval '(builtins.attrNames ((import <nixpkgs> {}).haskell.compiler))'
[ "ghc822Binary" "ghc844" "ghc863Binary" "ghc865" "ghc881" "ghcHEAD" "ghcjs" "ghcjs86" "integer-simple" ]
```

# Building your pull request

You are heartily encouraged to close an issue by contributing a pull request that fixes it :)
To test whether your fix will build,
ideally you should build it for all GHC versions:

```
nix-build all-ghcs.nix
```

You can see all supported versions in `.travis.yml`.
If your fix breaks an older version, do not despair.
As long as the last three versions work,
you can delete older GHC versions from `.travis.yml` in your pull request,
and deprecation of that version will be considered.

If you fix an important bug or contribute a new feature,
consider adding a line to `CHANGELOG` describing what you have changed.

# Running tests locally

Tests will be run with the above `nix-build` calls, but if you want to run them
without doing a full nix build, you can do so with cabal:

```
nix-shell
cabal test
```
