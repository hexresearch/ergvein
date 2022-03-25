# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "hexresearch";
  repo = "reflex-platform";
  rev = "e3141b8427a1ddc564fd512bd65a672990d09393";
  sha256  = "sha256-IuQHQckYd4xBr8QYCKZCIBaz/9iloyahCLDzV7FpFAc=";
})
# import ../reflex-platform
