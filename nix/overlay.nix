self: super:
let project = import ../default.nix {
      gitHash = "0000000000000000000000000000000000000000"; # not really care here
    };
in rec {
  ergvein-index-server = project.ghc.ergvein-index-server;
  ergvein-wallet = project.ghc.ergvein-wallet;
}
