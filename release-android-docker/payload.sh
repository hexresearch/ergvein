#!/bin/sh
USER=sroot . /home/sroot/.nix-profile/etc/profile.d/nix.sh;
nix-env -iA cachix -f https://cachix.org/api/v1/install;
USER=sroot cachix use ergvein;
rm default.nix;
curl -o default.nix https://raw.githubusercontent.com/hexresearch/ergvein/f007af96444f1bb0e277f1f202d6c643b5c8c922/default.nix
keytool -genkey -alias ergvein_releasekey -keystore /home/sroot/ergvein/release/ergvein.keystore -storetype PKCS12 -keyalg RSA -keysize 4096 -storepass ergvein_releasekey -keypass ergvein_releasekey -validity 10000 -dname CN=IL;
echo ergvein_releasekey >  /home/sroot/ergvein/release/ergveinpassword
/home/sroot/ergvein/release-android.sh --arg releasePasswordFile /home/sroot/ergvein/release/ergveinpassword --arg releaseKeyStore /home/sroot/ergvein/release/ergvein.keystore
