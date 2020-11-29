{ config, lib, ... }:
with lib;  # use the functions from lib, such as mkIf
let
  pkgs = import <nixpkgs> { };
  # the values of the options set for the service by the user of the service
  bitcoin-cfg = config.services.bitcoin;
  # Script to call local bitcoin node
  local-cli-script = pkgs.writeShellScriptBin "local-bitcoin-cli" ''
    export RPC_PASSWORD=$(cat ${bitcoin-cfg.passwordFile} | xargs echo -n)
    ${bitcoin-cfg.package}/bin/bitcoin-cli -datadir=${bitcoin-cfg.datadir} -conf=${bitcoin-cfg.configPath} -rpcpassword=$RPC_PASSWORD "$@"
  '';
in {
  ##### interface. here we define the options that users of our service can specify
  options = {
    # the options for our service will be located under services.bitcoin
    services.bitcoin = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether to enable bitcoin node by default.
        '';
      };
      package = mkOption {
        type = types.package;
        default = pkgs.bitcoind;
        defaultText = "pkgs.bitcoind";
        description = ''
          Which bitcoin package to use with the service. The bitcoin node have to
          be built with ZeroMQ support.
        '';
      };
      nodeUser = mkOption {
        type = types.str;
        default = "bitcoin";
        description = ''
          Which name of RPC user to use.
        '';
      };
      nodePort = mkOption {
        type = types.int;
        description = ''
          Which port the cryptonode serves RPC.
        '';
      };
      nodeAddress = mkOption {
      	type = types.str;
        default = "127.0.0.1";
        description = ''
          Which address to listhen for node RPC.
        '';
      };
      nodeZMQPortBlock = mkOption {
        type = types.int;
        default = 28332;
        description = ''
          Which port the cryptonode serves RPC with ZeroMQ protocol. Block API.
        '';
      };
      nodeZMQPortTx = mkOption {
        type = types.int;
        default = 28333;
        description = ''
          Which port the cryptonode serves RPC with ZeroMQ protocol. Transaction API.
        '';
      };
      testnet = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Start in testnet mode. Uses different data dir.
        '';
      };
      reindex = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Start with -reindex flag. 
        '';
      };

      datadir = mkOption {
        type = types.str;
        default = if bitcoin-cfg.testnet then "/var/lib/bitcoin-testnet" else "/var/lib/bitcoin";
        description = ''
          Path to blockchain database on filesystem.
        '';
      };
      config = mkOption {
        type = types.str;
        default = ''
          server=1
          rest=1
          txindex=1
          testnet=${if bitcoin-cfg.testnet then "1" else "0"}
          ${if bitcoin-cfg.testnet then "[test]" else ""}
          rpcallowip=${bitcoin-cfg.nodeAddress}
          rpcuser=${bitcoin-cfg.nodeUser}
          rpcport=${toString bitcoin-cfg.nodePort}
        '';
        /* zmqpubrawblock=tcp://127.0.0.1:${toString bitcoin-cfg.nodeZMQPortBlock}
        zmqpubrawtx=tcp://127.0.0.1:${toString bitcoin-cfg.nodeZMQPortTx} */
        description = ''
          Configuration file for bitcoin.
        '';
      };
      configPath = mkOption {
        type = types.str;
        default = "/etc/bitcoin.conf";
        description = ''
          Configuration file location for bitcoin.
        '';
      };
      passwordFile = mkOption {
        type = types.str;
        default = "/run/keys/btcpassword";
        description = ''
          Location of file with password for RPC.
        '';
      };
      passwordFileService = mkOption {
        type = types.str;
        default = "btcpassword-key.service";
        description = ''
          Service that indicates that passwordFile is ready.
        '';
      };
    };
  };

  ##### implementation
  config = mkIf bitcoin-cfg.enable { # only apply the following settings if enabled
    # Write configuration file to /etc/bitcoin.conf
    environment.etc."bitcoin.conf" = {
      text = bitcoin-cfg.config; # we can use values of options for this service here
    };
    # Write shortcut script to run commands on the node
    environment.systemPackages = [
      local-cli-script
    ];
    # Create systemd service
    systemd.services.bitcoin = {
      enable = true;
      description = "Bitcoin node";
      after = ["network.target" bitcoin-cfg.passwordFileService];
      wants = ["network.target" bitcoin-cfg.passwordFileService];
      script = ''
        export RPC_PASSWORD=$(cat ${bitcoin-cfg.passwordFile} | xargs echo -n)
        ${bitcoin-cfg.package}/bin/bitcoind -datadir=${bitcoin-cfg.datadir} -conf=${bitcoin-cfg.configPath} -rpcpassword=$RPC_PASSWORD ${if bitcoin-cfg.reindex then "-reindex" else ""}
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
      intbitcoin = {
        text = ''
          if [ ! -d "${bitcoin-cfg.datadir}" ]; then
            mkdir -p ${bitcoin-cfg.datadir}
          fi
        '';
        deps = [];
      };
    };
  };
}
