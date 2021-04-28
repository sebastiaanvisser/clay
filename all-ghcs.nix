let
  travisJobEnvAssignments = (builtins.fromJSON (builtins.readFile ./.travis.yml)).env.jobs;
  getGHCVersionFromEnvAssignment = envAssignment: builtins.elemAt (builtins.split "=" envAssignment) 2;
  supportedGHCVersions = builtins.map getGHCVersionFromEnvAssignment travisJobEnvAssignments;
  buildClayWith = version: import ./default.nix { compiler = version; };
in builtins.map buildClayWith supportedGHCVersions
