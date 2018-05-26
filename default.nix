{ compiler ? "ghc841" }:

(import ./release.nix {inherit compiler;}).clay
