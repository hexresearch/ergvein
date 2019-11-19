# Pin version of nixpkgs to ensure build reproduceability
/* import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "hexresearch";
  repo = "reflex-platform";
  rev = "b3fcbbb2aa999a9e1b6cd3137e9b820c304121e3";
  sha256  = "1v440faja06c07nypxhjl6dxgnbgxh9s2x8nkahawfp5412rfsg7";
}) */
import ../reflex-platform
