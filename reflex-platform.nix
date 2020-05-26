# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "hexresearch";
  repo = "reflex-platform";
  rev = "8501b7214a7d4ab05f90936660943f7383673472";
  sha256  = "0ajx99d6gzw3r1pcykkar9yqcqnh8xf04631sjmqrn2lyf6h63gy";
})
/* import ../reflex-platform */
