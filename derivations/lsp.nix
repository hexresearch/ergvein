{ mkDerivation, aeson, async, attoparsec, base, bytestring
, containers, data-default, dependent-map, directory, filepath
, hashable, hslogger, hspec, hspec-discover, lens, lsp-types, mtl
, network-uri, QuickCheck, quickcheck-instances, random
, rope-utf16-splay, scientific, sorted-list, stm, text, time
, transformers, unliftio-core, unordered-containers, uuid, lib
}:
mkDerivation {
    pname = "lsp";
    version = "1.2.0.0";
    sha256 = "0ca8s84xlh9kmi1lx47d6z3c8c827q30spmbxi1nl37dn35ib4b6";
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [
        aeson async attoparsec base bytestring containers data-default
        dependent-map directory filepath hashable hslogger lens lsp-types
        mtl network-uri random scientific sorted-list stm text time
        transformers unliftio-core unordered-containers uuid
    ];
    testHaskellDepends = [
        aeson base bytestring containers data-default directory filepath
        hashable hspec lens network-uri QuickCheck quickcheck-instances
        rope-utf16-splay sorted-list stm text unordered-containers
    ];
    testToolDepends = [ hspec-discover ];
    description = "Haskell library for the Microsoft Language Server Protocol";
    license = lib.licenses.mit;
}