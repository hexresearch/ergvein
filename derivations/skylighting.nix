{ mkDerivation, aeson, ansi-terminal, attoparsec, base
, base64-bytestring, binary, blaze-html, bytestring
, case-insensitive, colour, containers, directory, filepath, hxt
, mtl, regex-pcre-builtin, safe, skylighting-core, stdenv, text
, utf8-string
}:
mkDerivation {
  pname = "skylighting";
  version = "0.8.1";
  sha256 = "d5d6f5ceec2a4e9b9c0f8a58c0de09224bbfa6e58c9ca89271f3ec42b1afaa09";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-terminal attoparsec base base64-bytestring binary
    blaze-html bytestring case-insensitive colour containers directory
    filepath hxt mtl regex-pcre-builtin safe skylighting-core text
    utf8-string
  ];
  homepage = "https://github.com/jgm/skylighting";
  description = "syntax highlighting library";
  license = stdenv.lib.licenses.gpl2;
}
