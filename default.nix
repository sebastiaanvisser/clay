{ compiler ? "ghc881" }:

(import ./release.nix {inherit compiler;}).clay
