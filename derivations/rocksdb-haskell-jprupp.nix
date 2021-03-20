{ mkDerivation, base, bytestring, data-default, directory, hspec
, QuickCheck, rocksdb, stdenv, string-conversions, unliftio
}:
mkDerivation {
  pname = "rocksdb-haskell-jprupp";
  version = "2.1.3";
  sha256 = "fb824955b1ab598858d0bd427ab107928f157071aeb5f1446f464a6cf6391606";
  libraryHaskellDepends = [
    base bytestring data-default directory unliftio
  ];
  librarySystemDepends = [ rocksdb ];
  testHaskellDepends = [
    base bytestring data-default directory hspec QuickCheck
    string-conversions unliftio
  ];
  homepage = "https://github.com/jprupp/rocksdb-haskell#readme";
  description = "Haskell bindings for RocksDB";
  license = stdenv.lib.licenses.bsd3;
}
