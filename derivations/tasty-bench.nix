{ mkDerivation, base, containers, deepseq, tasty, lib }:
mkDerivation {
    pname = "tasty-bench";
    version = "0.2.5";
    sha256 = "146i1vkp9008hik7qyi8m9qq3ij5kji84qscsf598rzjmjga7igd";
    revision = "1";
    editedCabalFile = "0rcsdiwri52wng5dj30k3c5qrn8qfr14qs53cs1y99mbqfpzs02g";
    libraryHaskellDepends = [ base containers deepseq tasty ];
    description = "Featherlight benchmark framework";
    license = lib.licenses.mit;
}