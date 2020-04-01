{ mkDerivation, auto-update, base, basement, bytestring, clock
, fetchgit, memory, psqueues, stdenv, tls
}:
mkDerivation {
  pname = "tls-session-manager";
  version = "0.0.4";
  src = fetchgit {
    url = "https://github.com/vincenthz/hs-tls";
    sha256 = "1hfkdlczb7y1xw3fw26q9s4gla8gxr7f87b1qsr7hrdw2gl4isyi";
    rev = "f01216dffaa50def08953fe5c0a206eb28427a49";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/session; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    auto-update base basement bytestring clock memory psqueues tls
  ];
  description = "In-memory TLS session manager";
  license = stdenv.lib.licenses.bsd3;
}

