{ mkDerivation, base, hspec, hspec-discover, mtl, stdenv, text }:
mkDerivation {
  pname = "clay";
  version = "0.13.3";
  src = ./..;
  libraryHaskellDepends = [ base mtl text ];
  testHaskellDepends = [ base hspec hspec-discover mtl text ];
  homepage = "http://fvisser.nl/clay";
  description = "CSS preprocessor as embedded Haskell";
  license = stdenv.lib.licenses.bsd3;
}
