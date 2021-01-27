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
      metrics = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Start prometheus and grafana with local metrics for the indexer.
        '';
      };
    };
  };

  ##### implementation
  config = {
    nixpkgs.overlays = [
      (import ../overlay.nix)
    ];
    nixpkgs.config.packageOverrides = pkgs: with pkgs; {
      ergo = pkgs.callPackage ../pkgs/ergo-node.nix {};
    };
    services = {
      bitcoin = {
        enable = cfg.enable;
        testnet = cfg.testnet;
        nodePort = 8332;
        package = with pkgs; pkgs.callPackage ../pkgs/bitcoin-node.nix { withGui = false; };
      };
      ergo = {
        enable = true;
        /* enable = cfg.enable; */
        testnet = cfg.testnet;
      };
      ergvein-indexer = {
        enable = cfg.enable;
        package = pkgs.ergvein-index-server;
        nodeExternalAddress = cfg.externalAddress;
        testnet = cfg.testnet;
        metrics = if cfg.metrics then {
          host = "127.0.0.1";
          port = 9667;
        } else null;
      };
      grafana = {
        enable = cfg.metrics;
        provision = {
          enable = true;
          datasources = [
            {
              name = "Prometheus";
              type = "prometheus";
              isDefault = true;
              url = "http://127.0.0.1:9090";
            }
          ];
          dashboards = [
            {
              options.path = ./dashboards;
            }
          ];
        };
      };
      prometheus = {
        enable = cfg.metrics;
        scrapeConfigs = [
          {
            job_name = "node";
            scrape_interval = "10s";
            metrics_path = "/";
            static_configs = [
              {
                targets = [
                  "127.0.0.1:9667"
                ];
                labels = {
                  alias = "indexer";
                };
              }
            ];
          }
        ];
      };
    };
  };
}
