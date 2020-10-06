{ mkDerivation, base, deepseq, immortal, monad-logger
, safe-exceptions, stdenv, text, unliftio-core
}:
mkDerivation {
  pname = "immortal-worker";
  version = "0.1.0.0";
  sha256 = "f16b4cfc6b18dac24c670309e020bce25d6021fa375758497f3b4d8c619ff4d6";
  libraryHaskellDepends = [
    base deepseq immortal monad-logger safe-exceptions text
    unliftio-core
  ];
  description = "Create worker threads that logs exceptions and restarts";
  license = stdenv.lib.licenses.mit;
}
