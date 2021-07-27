{ mkDerivation, base, constraints-extras, containers
, dependent-sum, lib
}:
mkDerivation {
    pname = "dependent-map";
    version = "0.4.0.0";
    sha256 = "0b0zhyl3wkl4kkrxvq7vwjz3gn0ndxjjgyw9cky8a6xyv190pkjk";
    libraryHaskellDepends = [
        base constraints-extras containers dependent-sum
    ];
    description = "Dependent finite maps (partial dependent products)";
    license = "unknown";
    hydraPlatforms = lib.platforms.none;
}