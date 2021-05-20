let project = import ./default.nix { release = false; };
in {
  inherit (project.ghc) ergvein-index-server;
  ergvein-wallet-desktop = project.ghc.ergvein-wallet;
  sepulcas-android = project.android.ergvein-wallet;
}
