# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "hexresearch";
  repo = "reflex-platform";
  rev = "f76ff088aa0af677e7f216391690d7df37341ee2";
  sha256  = "15f6szr0fl5q0h10g7vpmc3s93aipb5wmy9mrkslqxz9h0rbcqzh";
})
/* import ../reflex-platform  */
