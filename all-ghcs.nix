{ nixpkgs ? import <nixpkgs> {} }:

let
  supportedGHCVersions = [
    "ghc822Binary"
    "ghc844"
    "ghc865"
    "ghc881"
  ];

  buildClayWith = version: import ./default.nix { compiler = version; };
in builtins.map buildClayWith supportedGHCVersions
