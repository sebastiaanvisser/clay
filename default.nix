{ compiler ? "ghc844" }:

(import ./release.nix {inherit compiler;}).clay
