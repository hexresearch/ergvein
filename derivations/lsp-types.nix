{ mkDerivation, aeson, base, binary, bytestring, containers
, data-default, deepseq, dependent-sum, dependent-sum-template
, directory, filepath, hashable, hslogger, lens, network-uri
, rope-utf16-splay, scientific, some, template-haskell, temporary
, text, unordered-containers, lib
}:
mkDerivation {
    pname = "lsp-types";
    version = "1.2.0.0";
    sha256 = "1axl62yafkxh414dxr5i7pwqw0s3kkqphs7b259qk33vin3qayk3";
    libraryHaskellDepends = [
        aeson base binary bytestring containers data-default deepseq
        dependent-sum dependent-sum-template directory filepath hashable
        hslogger lens network-uri rope-utf16-splay scientific some
        template-haskell temporary text unordered-containers
    ];
    description = "Haskell library for the Microsoft Language Server Protocol, data types";
    license = lib.licenses.mit;
}