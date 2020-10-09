# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "hexresearch";
  repo = "reflex-platform";
  rev = "33b949787972634e2e9269c3e6a5bfdc1bb5e2cf";
  sha256  = "15w840pmz4sl8hj9q665w4gvisz8x7gc68qy82qrdly16i3sgxzi";
})
/* import ../reflex-platform */
