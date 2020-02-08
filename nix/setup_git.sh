if grep -q "nixbld" /etc/group; then
  echo "NixOS installation detected. Make sure you added ./private-ssh-pipe.nix to your configuration.nix, see README.md."
else
  echo "Single user nix installation detected. Pass SSH config and agent as usual."
  SSH_CONFIG=$(nix-build -E "let pkgs = import <nixpkgs> {}; in pkgs.writeText \"ssh-config\" (builtins.readFile ./ssh-config)")
  SOCKET_PATH="ssh-config-file=$SSH_CONFIG:ssh-auth-sock=$SSH_AUTH_SOCK:"
fi

export GIT_NIX_PATH=$SOCKET_PATH