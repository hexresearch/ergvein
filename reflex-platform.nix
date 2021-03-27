# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "hexresearch";
  repo = "reflex-platform";
  rev = "9d85c4ac31eb9988ecaf9830f1ac2122bca54313";
  sha256  = "1gk5dv0qjvsbpvk9h6hrs09sb2y23rncnm486sbgzxr55dpnksxw";
})
/* import ../reflex-platform  */
