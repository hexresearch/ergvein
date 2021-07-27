{ mkDerivation, aeson, base, Chart, Chart-diagrams, diagrams
, diagrams-svg, directory, extra, filepath, shake, text, lib
}:
mkDerivation {
    pname = "shake-bench";
    version = "0.1.0.0";
    sha256 = "09lgmiw77nr3xycxksvzmcw1c2j66h51d5vxpm0lngv1dnsrad64";
    libraryHaskellDepends = [
        aeson base Chart Chart-diagrams diagrams diagrams-svg directory
        extra filepath shake text
    ];
    description = "Build rules for historical benchmarking";
    license = lib.licenses.asl20;
    maintainers = with lib.maintainers; [ maralorn ];
}