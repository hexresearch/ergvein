self: super:
let project = import ../default.nix {};
in rec {
  ergvein-index-server = project.ghc.ergvein-index-server;
  ergvein-wallet = project.ghc.ergvein-wallet;
}
