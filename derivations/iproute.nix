{ mkDerivation, appar, base, byteorder, bytestring, containers
, doctest, hspec, network, QuickCheck, safe, stdenv
}:
mkDerivation {
  pname = "iproute";
  version = "1.7.9";
  sha256 = "5547fa599c46b854c98625d7d3b745557193704096d3a947c1d8c534a23360d4";
  revision = "1";
  editedCabalFile = "1vbzch9lainl05ydym5z8x0kz0a0ywwba45d7xgg5fb8cp2n5zxh";
  libraryHaskellDepends = [
    appar base byteorder bytestring containers network
  ];
  testHaskellDepends = [
    appar base byteorder bytestring containers doctest hspec network
    QuickCheck safe
  ];
  homepage = "http://www.mew.org/~kazu/proj/iproute/";
  description = "IP Routing Table";
  license = stdenv.lib.licenses.bsd3;
}
