(import ./reflex-platform.nix) {
    nixpkgsOverlays = [
      (self: super: import ./nixpkgs-overlays/default.nix self super )
/*      (self: super:
      let
          oracleurls = {
            "jdk-8u171-linux-x64.tar.gz"            = http://download.oracle.com/otn-pub/java/jdk/8u171-b11/512cd62ec5174c3487ac17c61aaa89e8/jdk-8u171-linux-x64.tar.gz;
            "jdk-8u171-linux-i586.tar.gz"           = http://download.oracle.com/otn-pub/java/jdk/8u171-b11/512cd62ec5174c3487ac17c61aaa89e8/jdk-8u171-linux-i586.tar.gz;
            "jdk-8u171-linux-arm32-vfp-hflt.tar.gz" = http://download.oracle.com/otn-pub/java/jdk/8u171-b11/512cd62ec5174c3487ac17c61aaa89e8/jdk-8u171-linux-arm32-vfp-hflt.tar.gz;
            "jdk-8u171-linux-arm64-vfp-hflt.tar.gz" = http://download.oracle.com/otn-pub/java/jdk/8u171-b11/512cd62ec5174c3487ac17c61aaa89e8/jdk-8u171-linux-arm64-vfp-hflt.tar.gz;

            "jdk-8u181-linux-x64.tar.gz"            = http://download.oracle.com/otn-pub/java/jdk/8u181-b13/96a7b8442fe848ef90c96a2fad6ed6d1/jdk-8u181-linux-x64.tar.gz;
            "jdk-8u181-linux-i586.tar.gz"           = http://download.oracle.com/otn-pub/java/jdk/8u181-b13/96a7b8442fe848ef90c96a2fad6ed6d1/jdk-8u181-linux-i586.tar.gz;
            "jdk-8u181-linux-arm32-vfp-hflt.tar.gz" = http://download.oracle.com/otn-pub/java/jdk/8u181-b13/96a7b8442fe848ef90c96a2fad6ed6d1/jdk-8u181-linux-arm32-vfp-hflt.tar.gz;
            "jdk-8u181-linux-arm64-vfp-hflt.tar.gz" = http://download.oracle.com/otn-pub/java/jdk/8u181-b13/96a7b8442fe848ef90c96a2fad6ed6d1/jdk-8u181-linux-arm64-vfp-hflt.tar.gz;
          };
          requireFileOracle = {name, sha256, url}: self.stdenv.mkDerivation {
            inherit name;
            outputHashMode = "flat";
            outputHashAlgo = "sha256";
            outputHash = sha256;
            buildCommand = "${self.curl}/bin/curl -L --insecure -o $out -H 'Cookie: oraclelicense=accept-securebackup-cookie' ${oracleurls.${name}}";
          };
      in
      import ./nixpkgs-overlays/default.nix self super ) */
    ];
    config = {
      android_sdk.accept_license = true;
      allowBroken = true;
      oraclejdk.accept_license = true;
      allowUnfree = true;
    };
}
