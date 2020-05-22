# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "hexresearch";
  repo = "reflex-platform";
  rev = "def4464ba9b1bbadf39e5cdf8b36ebfbf4151327";
  sha256  = "0ms3hi27rikprnknaiqn5wza3vqa5n0siz3fzqr5xz142w2i9gdj";
})
/* import ../reflex-platform */
