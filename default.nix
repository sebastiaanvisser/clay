{ compiler ? builtins.getEnv "GHC_VERSION" }:

let
  compilerWithDefault = if compiler == "" then "ghc884" else compiler;
  release = import ./release.nix { compiler = compilerWithDefault; };
in { inherit (release) clay examples; }
