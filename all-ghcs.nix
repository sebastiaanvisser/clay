{ nixpkgs ? import <nixpkgs> {} }:

let
  supportedGHCVersions = [
    "ghc822Binary"
    "ghc844"
    "ghc865"
    "ghc881"
  ];
  getGHCByVersion = version: builtins.getAttr version nixpkgs.haskell.compiler;
in builtins.map getGHCByVersion supportedGHCVersions
