{ mkDerivation, base, hspec, hspec-discover, mtl, stdenv, lib, text }:
mkDerivation {
  pname = "clay";
  version = "0.15.0";
  src = ./..;
  libraryHaskellDepends = [ base mtl text ];
  testHaskellDepends = [ base hspec hspec-discover mtl text ];
  homepage = "http://fvisser.nl/clay";
  description = "CSS preprocessor as embedded Haskell";
  license = lib.licenses.bsd3;
}
