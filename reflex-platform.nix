# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "hexresearch";
  repo = "reflex-platform";
  rev = "269f59069c9d1175687b224aaeaedd34e7d60ad7";
  sha256  = "0dpnwqln2vgpqw1kc1wpywgrlbi6d309yx2vk7lcya5fasi21n36";
})
/* import ../reflex-platform  */
