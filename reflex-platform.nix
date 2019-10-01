# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "reflex-frp";
  repo = "reflex-platform";
  rev = "a7cd9a23e7faa9c2545a4c111229e28208e42351";
  sha256  = "0y52rfrhk4zszgprpyni51l0pgq18dg695k5bmpd62c3zxar5mvm";
})
