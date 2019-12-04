#! /usr/bin/env nix-shell
#!nix-shell -i bash -p androidenv.androidPkgs_9_0.androidsdk

# You should add `{ android_sdk.accept_license = true; }` to ~/.config/nixpkgs/config.nix

android create avd -n device -t android-28 --abi x86-64
