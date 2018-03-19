{ mkDerivation, base, hspec, hspec-expectations, mtl, stdenv, text
}:
mkDerivation {
  pname = "clay";
  version = "0.13.0";
  src = ./..;
  libraryHaskellDepends = [ base mtl text ];
  testHaskellDepends = [ base hspec hspec-expectations mtl text ];
  homepage = "http://fvisser.nl/clay";
  description = "CSS preprocessor as embedded Haskell";
  license = stdenv.lib.licenses.bsd3;
}
