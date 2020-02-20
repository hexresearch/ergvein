(import ./reflex-platform.nix) {
    nixpkgsOverlays = [
      (self: super: import ./nixpkgs-overlays/default.nix self super )
    ];
    config = {
      android_sdk.accept_license = true;
      allowBroken = true;
    };
}