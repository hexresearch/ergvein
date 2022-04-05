# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "hexresearch";
  repo = "reflex-platform";
  rev = "1931aebb89d3283216ec2b6eadc257f5b2cc4e67";
  sha256 = "1ihhg31vsibxg3an8z9gz1bzxmzskjbikfq1jph9vma4i231hb4c";
})
# import ../reflex-platform
