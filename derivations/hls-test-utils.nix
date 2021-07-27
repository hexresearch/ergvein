{ mkDerivation, aeson, async, base, blaze-markup, bytestring
, containers, data-default, directory, extra, filepath, ghcide
, hls-plugin-api, hspec, hspec-core, lens, lsp, lsp-test, lsp-types
, shake, tasty, tasty-expected-failure, tasty-golden, tasty-hunit
, tasty-rerun, temporary, text, unordered-containers, lib
}:
mkDerivation {
    pname = "hls-test-utils";
    version = "1.0.0.0";
    sha256 = "18n7vb9fa39jkgr0gvsrjfc0nh09w2xlniifb25bn6z3qc3w0h6i";
    libraryHaskellDepends = [
        aeson async base blaze-markup bytestring containers data-default
        directory extra filepath ghcide hls-plugin-api hspec hspec-core
        lens lsp lsp-test lsp-types shake tasty tasty-expected-failure
        tasty-golden tasty-hunit tasty-rerun temporary text
        unordered-containers
    ];
    description = "Utilities used in the tests of Haskell Language Server";
    license = lib.licenses.asl20;
}