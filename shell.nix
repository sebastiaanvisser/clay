{ compiler ? "ghc841" }:

let
  release = (import ./release.nix {inherit compiler;});
in release.pkgs.stdenv.lib.overrideDerivation release.clay.env (oldAttrs: rec {
  nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [
    release.cabal
    release.pkgs.haskellPackages.cabal2nix
  ];
})
