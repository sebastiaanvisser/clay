{ compiler ? builtins.getEnv "GHC_VERSION" }:

let
  compilerWithDefault = if compiler == "" then "ghc881" else compiler;
in
  (import ./release.nix { compiler = compilerWithDefault; }).clay
