{ config, lib, ... }:
with lib;  # use the functions from lib, such as mkIf
let
  pkgs = import <nixpkgs> { };
  # the values of the options set for the service by the user of the service
  cfg = config.services.ergvein-indexer;
in {
  ##### interface. here we define the options that users of our service can specify
  options = {
    # the options for our service will be located under services.ergvein-indexer
    services.ergvein-indexer = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable ergvein indexer node by default.
        '';
      };
      package = mkOption {
        type = types.package;
        description = ''
          Which package to use with the service.
        '';
      };
      nodeExternalAddress = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          Which IP and port is assigned to the node as external.
        '';
      };
      nodePort = mkOption {
        type = types.int;
        default = 8085;
        description = ''
          Which port the cryptonode serves RPC.
        '';
      };
      nodeTcpPort = mkOption {
        type = types.int;
        default = 8667;
        description = ''
          Which port the cryptonode listen to TCP protocol connections.
        '';
      };
      nodeHostname = mkOption {
        type = types.str;
        default = "0.0.0.0";
        description = ''
          Which hostname is binded to the node.
        '';
      };
      filtersPath = mkOption {
        type = types.str;
        default = if cfg.testnet then "/var/lib/ergvein-testnet/filters" else "/var/lib/ergvein/filters";
        description = ''
          Path to filters database on filesystem.
        '';
      };
      indexerPath = mkOption {
        type = types.str;
        default = if cfg.testnet then "/var/lib/ergvein-testnet/indexer" else "/var/lib/ergvein/indexer";
        description = ''
          Path to indexer database on filesystem.
        '';
      };
      testnet = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Start in testnet mode. Uses different data dir.
        '';
      };

      btcHost = mkOption {
        type = types.str;
        default = "localhost";
        description = ''
          Host where BTC node is located.
        '';
      };
      btcPort = mkOption {
        type = types.int;
        default = 8332;
        description = ''
          Which port BTC node serves RPC.
        '';
      };
      btcUseTcp = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Connect to BTC node over TCP protocol.
        '';
      };
      btcTcpPort = mkOption {
        type = types.int;
        default = 8333;
        description = ''
          Which port BTC node serves protocol.
        '';
      };
      btcUser = mkOption {
        type = types.str;
        default = "bitcoin";
        description = ''
          Which name of RPC user to use.
        '';
      };
      btcPasswordFile = mkOption {
        type = types.str;
        default = "/run/keys/btcpassword";
        description = ''
          Location of file with password for RPC.
        '';
      };
      btcPasswordFileService = mkOption {
        type = types.str;
        default = "btcpassword-key.service";
        description = ''
          Service that indicates that passwordFile is ready.
        '';
      };

      config = mkOption {
        type = types.str;
        default = ''
          serverPort               : ${toString cfg.nodePort}
          serverTcpPort            : ${toString cfg.nodeTcpPort}
          serverHostname           : ${cfg.nodeHostname}
          filtersDbPath            : ${cfg.filtersPath}
          indexerDbPath            : ${cfg.indexerPath}
          BTCNodeIsTestnet         : ${if cfg.testnet then "true" else "false"}
          BTCNodeHost              : ${cfg.btcHost}
          BTCNodePort              : ${toString cfg.btcPort}
          BTCNodeUser              : ${cfg.btcUser}
          BTCNodePassword          : "_env:BTC_PASSWORD:"
          BTCNodeTCPHost           : ${cfg.btcHost}
          BTCNodeTCPPort           : ${toString cfg.btcTcpPort}
          ERGONodeHost             : localhost
          ERGONodePort             : 9052
          blockchainScanDelay      : 10000000
          ownPeerAddress           : ${if cfg.nodeExternalAddress != null then cfg.nodeExternalAddress else "null"}
          knownPeers               : []
          peerActualizationDelay   : 10000000
          peerActualizationTimeout : 86400

        '';
        description = ''
          Configuration file for indexer server.
        '';
      };
      configName = mkOption {
        type = types.str;
        default = "ergvein-indexer.yaml";
        description = ''
          Configuration file location for indexer server.
        '';
      };

    };
  };

  ##### implementation
  config = mkIf cfg.enable { # only apply the following settings if enabled
    # Write configuration file to /etc/ergvein-indexer.yaml
    environment.etc."${cfg.configName}" = {
      text = cfg.config; # we can use values of options for this service here
    };
    # Create systemd service
    systemd.services.ergvein-indexer = {
      enable = true;
      description = "Ergvein indexer node";
      after = ["network.target" cfg.btcPasswordFileService];
      wants = ["network.target" cfg.btcPasswordFileService];
      script = ''
        export BTC_PASSWORD=$(cat ${cfg.btcPasswordFile} | xargs echo -n)
        ${cfg.package}/bin/ergvein-index-server ${if cfg.btcUseTcp then "--tcp-node" else ""} listen /etc/${cfg.configName}
      '';
      serviceConfig = {
          Restart = "always";
          RestartSec = 30;
          User = "root";
        };
      wantedBy = ["multi-user.target"];
    };
    # Init folder for bitcoin data
    system.activationScripts = {
      int-ergvein-indexer = {
        text = ''
          if [ ! -d "${cfg.filtersPath}" ]; then
            mkdir -p ${cfg.filtersPath}
          fi
          if [ ! -d "${cfg.indexerPath}" ]; then
            mkdir -p ${cfg.indexerPath}
          fi
        '';
        deps = [];
      };
    };
  };
}
