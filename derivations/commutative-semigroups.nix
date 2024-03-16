{ mkDerivation, base, containers, lib }:
mkDerivation {
  pname = "commutative-semigroups";
  version = "0.1.0.1";
  sha256 = "0732d53b25fea4822ed8aefdc0aa5586db9a78f67db8cc796adabd614677aaae";
  libraryHaskellDepends = [ base containers ];
  description = "Commutative semigroups";
  license = lib.licenses.bsd3;
}
