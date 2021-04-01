let project = import ./default.nix { release = false; };
    /* containers = import ./nix/containers.nix {
      isProd = true;
      gitHash = ?;
      gitTag = ?;
      gitBranch = ?;
      containerTag = ?;
    }; */
in {
  inherit (project.ghc) ergvein-index-server;
  ergvein-wallet-desktop = project.ghc.ergvein-wallet;
  sepulcas-android = project.android.ergvein-wallet;
  /* ergvein-index-server-docker = containers.index-server-container; */
}
