#! /usr/bin/env nix-shell
#!nix-shell -i bash -p adoptopenjdk-bin
keytool -genkey -v -keystore ${1:-my_store.keystore} -alias ergvein_releasekey -keyalg RSA -keysize 4096 -validity 10000
