{ mkDerivation, base, base16-bytestring, base64-bytestring
, bytestring, Cabal, containers, cryptohash-sha256, directory
, ed25519, filepath, ghc-prim, mtl, network, network-uri, parsec
, pretty, QuickCheck, stdenv, tar, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, temporary, time, transformers
, zlib
}:
mkDerivation {
  pname = "hackage-security";
  version = "0.5.3.0";
  sha256 = "db986e17e9265aa9e40901690815b890b97d53159eb24d0a6cafaa7c18577c21";
  revision = "6";
  editedCabalFile = "1xs2nkzlvkdz8g27yzfxbjdbdadfmgiydnlpn5dm77cg18r495ay";
  libraryHaskellDepends = [
    base base16-bytestring base64-bytestring bytestring Cabal
    containers cryptohash-sha256 directory ed25519 filepath ghc-prim
    mtl network network-uri parsec pretty tar template-haskell time
    transformers zlib
  ];
  testHaskellDepends = [
    base bytestring Cabal containers network-uri QuickCheck tar tasty
    tasty-hunit tasty-quickcheck temporary time zlib
  ];
  doCheck = false;
  homepage = "https://github.com/haskell/hackage-security";
  description = "Hackage security library";
  license = stdenv.lib.licenses.bsd3;
}
