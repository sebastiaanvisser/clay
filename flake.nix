{
  description = "clay";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat.url = "github:edolstra/flake-compat";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    with builtins;
    with nixpkgs.lib;
    let
      inherit (nixpkgs) lib;
      projectName = "clay";
      localPackages = {
        clay = ./.;
        clay-examples = ./examples;
      };

      # Always keep in sync with the tested-with section in the cabal file
      supportedGhcs = [
        # Not supported in nixpkgs anymore
        # "ghc86"
        # "ghc88"

        "ghc810"
        "ghc90"
        "ghc92"
        "ghc94"
        "ghc96"
        "ghc98"
        "ghc910"
        # "ghc912" # Uncomment as soon as nixpkgs is more advanced
      ];

      haskellPackagesFor = pkgs: genAttrs supportedGhcs (ghc: pkgs.haskell.packages.${ghc})
        // { default = pkgs.haskellPackages; };

      hoverlay = pkgs: hfinal: hprev: with pkgs.haskell.lib;
        (mapAttrs (pname: path: hfinal.callCabal2nix pname path { }) localPackages);

      haskellPackagesExtended = pkgs: mapAttrs
        (ghcVersion: haskellPackages: haskellPackages.override (_: {
          overrides = (hoverlay pkgs);
        }))
        (haskellPackagesFor pkgs);

      localPackagesFor = haskellPackages: mapAttrs (pname: _path: haskellPackages.${pname}) localPackages;
      allLocalPackagesFor = pkgs: ghcVersion: haskellPackages:
        pkgs.linkFarm "${projectName}-all-for-${ghcVersion}"
          (localPackagesFor haskellPackages);
    in
    flake-utils.lib.eachDefaultSystem
      (system:

        let
          pkgs = nixpkgs.legacyPackages.${system};
          forEachGHC = mapAttrs (allLocalPackagesFor pkgs) (haskellPackagesExtended pkgs);
          allGHCs = pkgs.linkFarm "${projectName}-all-ghcs" forEachGHC;
        in
        {
          # "packages" doesn't allow nested sets
          legacyPackages = mapAttrs
            (ghcVersion: haskellPackages: localPackagesFor haskellPackages // {
              "${projectName}-all" = allLocalPackagesFor pkgs ghcVersion haskellPackages;
            })
            (haskellPackagesExtended pkgs) // {
            "${projectName}-all" = forEachGHC;
          };

          packages = {
            default = allGHCs;
          };

          devShells = mapAttrs
            (ghcVersion: haskellPackages: haskellPackages.shellFor {
              packages = hps: attrValues (localPackagesFor haskellPackages);
              nativeBuildInputs = (
                lib.optional (versionAtLeast (haskellPackagesFor pkgs).${ghcVersion}.ghc.version "9.2")
                  (haskellPackagesFor pkgs).${ghcVersion}.haskell-language-server)
              ++ (with pkgs;
                [ cabal-install ]
              )
              ;
            })
            (haskellPackagesExtended pkgs);

          formatter = pkgs.nixpkgs-fmt;
        }) // {

      # GHC in nixpkgs is currently broken on darwin for older versions, https://github.com/NixOS/nixpkgs/issues/367686
      # Remove the filter when this is fixed.
      supportedGhcs = lib.filter (ghcVersion: ! (elem ghcVersion [ "ghc810" "ghc90" "ghc92" "ghc94" ])) supportedGhcs;
      inherit hoverlay;
    };
}
