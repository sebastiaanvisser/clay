{ mkDerivation, base, hspec, hspec-expectations, mtl, stdenv, text
, fixplate
}:
mkDerivation {
  pname = "clay";
  version = "0.13.0";
  src = ./..;
  libraryHaskellDepends = [ base mtl text fixplate ];
  testHaskellDepends = [ base hspec hspec-expectations mtl text ];
  homepage = "http://fvisser.nl/clay";
  description = "CSS preprocessor as embedded Haskell";
  license = stdenv.lib.licenses.bsd3;
}
