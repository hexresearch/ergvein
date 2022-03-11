{ mkDerivation, array, async, attoparsec, auto-update, base
, base16-bytestring, base64-bytestring, bytestring, containers
, cryptonite, doctest, hourglass, hspec, iproute, lib, mtl, network
, psqueues, QuickCheck, word8, patchelf, stdenv, ghc
}: let
    version = "4.0.1";
in mkDerivation {
  pname = "dns";
  inherit version;
  sha256 = "20cdb4519f19becd5ba321c5acfe03fd3c16b298a78404530b65f10ddb4a68cb";
  libraryHaskellDepends = [
    array async attoparsec auto-update base base16-bytestring
    base64-bytestring bytestring containers cryptonite hourglass
    iproute mtl network psqueues
  ];
  testHaskellDepends = [
    base bytestring doctest hspec iproute network QuickCheck word8
  ];
  postInstall = let
    platform = if stdenv.hostPlatform.isAarch64 then "aarch64-android"
        else if stdenv.hostPlatform.system == "armv7a-linux" then "arm-android"
        else stdenv.hostPlatform.system;
    in ''
    patchelf --version
    PROBLEM_SO=$out/lib/ghc-${ghc.version}/${platform}-ghc-${ghc.version}/libHSdns-${version}-??????????????????????-ghc${ghc.version}.so
    PROBLEM_PATH=/build/dns-${version}/dist/build/dns-internal
    RIGHT_PATH=$out/lib/ghc-${ghc.version}/${platform}-ghc-${ghc.version}
    echo "Problematic rpath: $PROBLEM_PATH"
    echo "Replacing with: $RIGHT_PATH"
    echo "Problematic libraries: $PROBLEM_SO"
    PATCHED_RPATH=$(patchelf --print-rpath $PROBLEM_SO | sed 's@'"$PROBLEM_PATH"'@'"$RIGHT_PATH"'@g')
    echo "Patched rpath $PATCHED_RPATH"
    patchelf --set-rpath $PATCHED_RPATH $PROBLEM_SO
  '';
  doHaddock = false;
  doCheck = false;
  description = "DNS library in Haskell";
  license = lib.licenses.bsd3;
}
