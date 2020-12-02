# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "hexresearch";
  repo = "reflex-platform";
  rev = "423e618f5faf6ef1fb25cdd0d7561333d8d08fe3";
  sha256  = "19gjgg1d7j9gdkpd2f5sj2n16cxga6hnw7kvqb6pi09zc63yb77c";
})
/* import ../reflex-platform  */
