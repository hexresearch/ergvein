# Pointing to old commit as we run into https://github.com/mozilla/nixpkgs-mozilla/issues/232
/* import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/4521bc61c2332f41e18664812a808294c8c78580.tar.gz) */
(import (fetchTarball
      "https://github.com/nix-community/fenix/archive/b7fd78ca0973bf19966ffe1ec8f7f69cce337853.tar.gz"))
