{ config, lib, pkgs, ... }:
with lib;  # use the functions from lib, such as mkIf
let
  # the values of the options set for the service by the user of the service
  cfg = config.services.ergvein;
  addressType = import ../service/address-type.nix { inherit lib; };
in {
  ##### Depedendant services
  imports = [
    ../service/bitcoin.nix
    ../service/ergvein-indexer.nix
  ];

  ##### interface. here we define the options that users of our service can specify
  options = {
    services.ergvein = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable ergvein indexer system by default.
        '';
      };
      testnet = mkOption {
        type = types.bool;
        default = false;
        description = ''
        Start in testnet mode. Uses different data dir.
        '';
      };
      externalAddress = mkOption {
        type = types.nullOr (types.submodule addressType);
        default = null;
        description = ''
        Which IP and port is assigned to the node as external.
        '';
      };
    };
  };

  ##### implementation
  config = mkIf cfg.enable { # only apply the following settings if enabled
    nixpkgs.overlays = [
      (import ../overlay.nix)
    ];
    services = {
      bitcoin = {
        enable = true;
        testnet = cfg.testnet;
        nodePort = 8332;
        package = with pkgs; pkgs.callPackage ../pkgs/bitcoin-node.nix { withGui = false; };
      };
      ergvein-indexer = {
        enable = true;
        package = pkgs.ergvein-index-server;
        nodeExternalAddress = cfg.externalAddress;
        testnet = cfg.testnet;
      };
    };
  };
}
