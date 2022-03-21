# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
 owner = "hexresearch";
 repo = "reflex-platform";
 rev = "4090ab1ab75f3cae90eecd36e3780b6aecef9580";
 sha256  = "sha256-Sh8uSuVRFvVH8ig+A9OUdvrOZ4SqKnocaW4Uf5uxU2E=";
})
# import ../reflex-platform
