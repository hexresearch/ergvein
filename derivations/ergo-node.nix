{ stdenv, fetchurl, jdk11

}:
stdenv.mkDerivation rec {
  name = "ergo-platform-${version}";
  version = "3.2.1";

  src = fetchurl {
    url = "https://github.com/ergoplatform/ergo/releases/download/v${version}/ergo-${version}.jar";
    sha256= "1f7z3jrxx6f63d76pqkkzp0ix00bna4nk89l27jy0abn4plshc6p";
  };

  buildInputs = [
    jdk11
  ];

  installPhase = ''
    mkdir -p $out/bin $out/lib/ergo-platform
    cp -v $src $out/lib/ergo-platform/ergo.jar
    cat > $out/bin/ergo-node << EOF
    #!/bin/sh
    exec ${jdk11}/bin/java -Xmx3G -Dlogback.stdout.level=DEBUG -jar $out/lib/ergo-platform/ergo.jar \$@ 
    EOF
    chmod +x $out/bin/ergo-node
  '';

  phases = "installPhase";

  meta = {
    description = "Ergo builds advanced cryptographic features and radically new DeFi functionality on the rock-solid foundations laid by a decade of blockchain theory and development.";
    homepage    = "https://ergoplatform.org/en/";
    license     = stdenv.lib.licenses.cc0;
    platforms   = stdenv.lib.platforms.linux ++ stdenv.lib.platforms.darwin;
    maintainers = [ ];
  };
}