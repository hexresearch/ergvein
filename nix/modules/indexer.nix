{ config, lib, ... }:
with lib;  # use the functions from lib, such as mkIf
let
  pkgs = import <nixpkgs> { };
  # the values of the options set for the service by the user of the service
  cfg = config.services.ergvein-indexer;
in {
  ##### Depedendant services
  imports = [
    ../service/bitcoin.nix
  ];

  ##### interface. here we define the options that users of our service can specify
  options = {
    services.ergvein-indexer = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable ergvein indexer system by default.
        '';
      };
    };
    testnet = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Start in testnet mode. Uses different data dir.
      '';
    };
  };

  ##### implementation
  config = mkIf cfg.enable { # only apply the following settings if enabled
    nixpkgs.overlays = [
      (import ../overlay.nix)
    ];
    services.bitcoin = {
      enable = true;
      testnet = cfg.testnet;
      nodePort = 8332;
      package = with pkgs; pkgs.callPackage ../pkgs/bitcoin-node.nix { withGui = false; };
    };

  };
}
