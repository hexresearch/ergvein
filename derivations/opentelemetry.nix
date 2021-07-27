{ mkDerivation, base, bytestring, exceptions, ghc-trace-events
, hashable, lib
}:
mkDerivation {
    pname = "opentelemetry";
    version = "0.6.1";
    sha256 = "0i88ciig40gil4gaj95qw28c2racdr2jb6rcpnsf60fzkqc8b3fk";
    libraryHaskellDepends = [
        base bytestring exceptions ghc-trace-events hashable
    ];
    license = lib.licenses.asl20;
}