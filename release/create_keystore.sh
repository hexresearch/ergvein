#! /usr/bin/env nix-shell
#!nix-shell -i bash -p androidenv.androidPkgs_9_0.platform-tools
keytool -genkey -v -keystore ${1:-my_store.keystore} -alias ergvein_releasekey -keyalg RSA -keysize 4096 -validity 10000
