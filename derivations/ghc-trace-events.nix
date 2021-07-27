{ mkDerivation, base, bytestring, tasty-bench, text, lib }:
mkDerivation {
    pname = "ghc-trace-events";
    version = "0.1.2.2";
    sha256 = "18vhv99lrfjx6bxww77qxg7gwqmvpylvlrq1bji0hd6mcxxdjn69";
    libraryHaskellDepends = [ base bytestring text ];
    benchmarkHaskellDepends = [ base bytestring tasty-bench ];
    description = "Faster traceEvent and traceMarker, and binary object logging for eventlog";
    license = lib.licenses.bsd3;
}