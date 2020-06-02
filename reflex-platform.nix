# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "hexresearch";
  repo = "reflex-platform";
  rev = "e1ea98fdb86daf7eb514b83c1444482f1a87cad5";
  sha256  = "1iskdbxqwjlbjh8s158y1x3qh1s7fkqmwikv2yg8f0lbsj6p4r87";
})
/* import ../reflex-platform */
