#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix-prefetch-scripts

nix-prefetch-git https://github.com/reflex-frp/reflex-platform > reflex-platform.json
