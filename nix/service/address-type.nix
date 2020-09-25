{ lib, ... }:
with lib;
{
  options = {
    host = mkOption {
      type = types.str;
      example = "127.0.0.1";
      description = ''
        Hostname of address.
      '';
    };
    port = mkOption {
      type = types.int;
      example = 8667;
      description = ''
        Port of address.
      '';
    };
  };
}
