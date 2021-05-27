{ profile ? false }: (import ./reflex-platform.nix) {
    nixpkgsOverlays = [
      (import ./nixpkgs-overlays/default.nix)
      (import ./nixpkgs-overlays/rust.nix)
    ];
    config = {
      android_sdk.accept_license = true;
      allowBroken = true;
      oraclejdk.accept_license = true;
      allowUnfree = true;
    };
    enableLibraryProfiling = profile;
}
