let
  pkgs = import <nixpkgs> { };

in
  { clay = pkgs.haskellPackages.callPackage ./default.nix { };
  }