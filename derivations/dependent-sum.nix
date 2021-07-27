{ mkDerivation, base, constraints-extras, some, lib }:
mkDerivation {
  pname = "dependent-sum";
  version = "0.7.1.0";
  sha256 = "0aj63gvak0y4mgxndykqfg5w958hf7lp5blml2z647rjgy85bjw1";
  revision = "1";
  editedCabalFile = "0h9rr26ksrqfnfjibnrzbf6hyp1mmffgzbvjjxjs6vdqylvr4h8f";
  libraryHaskellDepends = [ base constraints-extras some ];
  description = "Dependent sum type";
  license = lib.licenses.publicDomain;
}