# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
 owner = "hexresearch";
 repo = "reflex-platform";
 rev = "67a9563255fdce351ec0aea5c438449e340cdc43";
 sha256  = "sha256-phqf6ulM1k3OQ8A8owUWaUjcKmXkLO9W39YkqgtSrjA=";
})
# import ../reflex-platform
