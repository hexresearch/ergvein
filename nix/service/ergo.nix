{ config, lib, pkgs, ... }:
with lib;  # use the functions from lib, such as mkIf
let
  # the values of the options set for the service by the user of the service
  ergo-cfg = config.services.ergo;
in {
  ##### interface. here we define the options that users of our service can specify
  options = {
    # the options for our service will be located under services.ergo
    services.ergo = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable ergo node by default.
        '';
      };
      package = mkOption {
        type = types.package;
        default = pkgs.ergo;
        defaultText = "pkgs.ergo";
        description = ''
          Which ergo package to use with the service.
        '';
      };
      nodePort = mkOption {
        type = types.int;
        default = 9020;
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
      testnet = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Start in testnet mode. Uses different data dir.
        '';
      };
      datadir = mkOption {
        type = types.str;
        default = if ergo-cfg.testnet then "/var/lib/ergo-testnet" else "/var/lib/ergo";
        description = ''
          Path to blockchain database on filesystem.
        '';
      };
      user = mkOption {
        type = types.str;
        default = "ergo";
        description = ''
          User to run ergo node with.
        '';
      };
      preEip3Derivation = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Use old key derivation befor 3.3.4 version.
        '';
      };
      config = mkOption {
        type = types.str;
        default = ''
          ergo {
            directory = ${ergo-cfg.datadir}
            node {
              mining = false
            }
            wallet.secretStorage.secretDir = ${ergo-cfg.datadir}/wallet/keystore
            wallet.usePreEip3Derivation = ${if ergo-cfg.preEip3Derivation then "true" else "false"}
          }
          scorex {
            network {
              declaredAddress = "${ergo-cfg.nodeAddress}:${toString ergo-cfg.nodePort}"
            }
            restApi {
              apiKeyHash = "${ergo-cfg.secretHash}"
              bindAddress = "0.0.0.0:9052"
            }
          }
        '';
        description = ''
          Configuration file for ergo.
        '';
      };
      configPath = mkOption {
        type = types.str;
        default = "/etc/ergo.conf";
        description = ''
          Configuration file location for ergo.
        '';
      };
      secretHash = mkOption {
        type = types.str;
        description = ''
          Hex-encoded Blake2b256 hash of an API key. Should be 64-chars long Base16 string. You
          can use already running local node to get the hash of your secret http://127.0.0.1:9053/swagger#/utils/hashBlake2b .
          Example for secret 'hello' is '324dcf027dd4a30a932c441f365a25e86b173defa4b8e58948253471b81b72cf'.
        '';
      };
      uid = mkOption {
        type = types.int;
        default = 317;
        description = ''
          User and group id for service user.
        '';
      };
    };
  };

  ##### implementation
  config = mkIf ergo-cfg.enable { # only apply the following settings if enabled
    # Write configuration file to /etc/ergo.conf
    environment.etc."ergo.conf" = {
      text = ergo-cfg.config; # we can use values of options for this service here
    };
    # User configuration
    users.users."${ergo-cfg.user}" = {
      group = ergo-cfg.user;
      home = ergo-cfg.datadir;
      createHome = true;
      uid = ergo-cfg.uid;
    };
    users.groups."${ergo-cfg.user}".gid = ergo-cfg.uid;
    # Create systemd service
    systemd.services.ergo = {
      enable = true;
      description = "ergo node";
      script = ''
        ${ergo-cfg.package}/bin/ergo ${if ergo-cfg.testnet then "--testnet" else "--mainnet"} --config ${ergo-cfg.configPath}
      '';
      serviceConfig = {
          Restart = "always";
          RestartSec = 30;
          User = ergo-cfg.user;
          Group = ergo-cfg.user;
        };
      wantedBy = ["multi-user.target"];
    };
  };
}
